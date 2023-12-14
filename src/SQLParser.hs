module SQLParser where

import Control.Applicative
import Data.Char qualified as Char
import GHC.Generics (D)
import GenSQL
import Parser (Parser)
import Parser qualified as P
import SQLPrinter (pp)
import SQLPrinter qualified as SP
import SQLSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.ParserCombinators.ReadP (count)
import Text.PrettyPrint (render)
import Utils

-------- Helper Function for SQL Parser --------
wsP :: Parser a -> Parser a
wsP p = many P.space *> p <* many P.space

wsbP :: Parser a -> Parser a
wsbP p = p <* many P.space

stringP :: String -> Parser ()
stringP str = constP str ()

constP :: String -> a -> Parser a
constP str a = a <$ wsP (P.string str)

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

-- | A helper function for only parsing the string in between a specified
-- character
stringInP :: Char -> (Parser Char -> Parser String) -> Parser String
stringInP c f = P.between cP (f $ escape (c ==)) cP
  where
    cP = P.char c

-- Given a string, will look ahead and read a word. If that word is not keyword,
-- the output is valid, otherwise the output is invalid (return nothing)
catchAnyWord :: String -> Parser (Maybe a) -> Parser (Maybe a)
catchAnyWord kw =
  flip
    (<|>)
    (empty <$ P.filter (/= kw) (P.lookAhead (wsP P.anyWord)))

-- | Given a string and a parser, will check if the string is a full word, and
-- and whether p successfully match. Then it will produce a newer string with
-- space in between
pWordsCons :: String -> Parser String -> Parser String
pWordsCons str p = (\x xs -> x ++ " " ++ xs) <$> wsP (P.fullString str) <*> p

-- | Given a list of string, will check if each of the string is a full word (no
-- space)
pWords :: [String] -> Parser String
pWords xs =
  case splitLast xs of
    Nothing -> empty
    Just (xs, x) -> foldr pWordsCons (P.string x) xs

-- | Given a list, will split the list with non-tail list and tail (efficiently)
-- The function is safe (give an empty list will return Nothing)
splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs) = splitLast xs >>= (\(xs', x') -> return (x : xs', x'))

-- Given a string, will look ahead and read a word and, as long as the keyword
-- is not matched, will return that keyword in a list form
catchAnyWordL :: String -> Parser [a] -> Parser [a]
catchAnyWordL kw =
  flip
    (<|>)
    (empty <$ P.filter (/= kw) (P.lookAhead (wsP P.anyWord)))

-- | Parse something with optional parentheses
optionalParens :: Parser a -> Parser a
optionalParens p = p <|> wsP (parens p)

-- | Parse something with optional space.
optionalSpaceParens :: Parser a -> Parser a
optionalSpaceParens = optionalParens . wsP

-------- SQL Parser --------

-- | Parsing ascending or descending for order command
orderTypeADP :: Parser OrderTypeAD
orderTypeADP = wsP (ASC <$ P.string "ASC" <|> DESC <$ P.string "DESC")

-- | Parsing Nulls first or last for order command
orderTypeFLP :: Parser OrderTypeFL
orderTypeFLP =
  wsP
    ( NULLSFIRST <$ P.string "NULLS FIRST"
        <|> NULLSLAST <$ P.string "NULLS LAST"
    )

-- Parsing Integer Value
intValP :: Parser DValue
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]

-- Parsing Boolean Value
boolValP :: Parser DValue
boolValP = trueP <|> falseP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ wsP (P.string "TRUE")
    falseP :: Parser DValue
    falseP = BoolVal False <$ wsP (P.string "FALSE")

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]

-- Parsing Null Value, different than null in table parser
nullValP :: Parser DValue
nullValP = NullVal <$ wsP (P.string "NULL")

-- >>> P.parse (many nullValP) "NULL NULL\n NULL"
-- Right [NullVal,NullVal,NullVal]

-- | escape function will take in a function
-- and will parse the character that does not make the function return false
escape :: (Char -> Bool) -> Parser Char
escape f = P.satisfy (not . f)

-- | String Value in SQL needs to be covered by single quotes
stringValP :: Parser DValue
stringValP = StringVal <$> wsP (stringInP '\'' many)

-- | DValue can either by integer, boolean, null, or string value
dvalueP :: Parser DValue
dvalueP = intValP <|> boolValP <|> nullValP <|> stringValP

-- Helper functions for function and aggregate functions
string2Function :: String -> Function
string2Function str =
  case str of
    "LENGTH" -> Len
    "LOWER" -> Lower
    _ -> Upper

string2AggFunction :: String -> AggFunction
string2AggFunction str =
  case str of
    "AVG" -> Avg
    "COUNT" -> Count
    "MAX" -> Max
    "MIN" -> Min
    _ -> Sum

-- | Parse function
functionP :: Parser Function
functionP =
  string2Function <$> wsP (P.choice $ map P.string reservedFunction)

-- | Parse aggregate function
aggFunctionP :: Parser AggFunction
aggFunctionP =
  string2AggFunction <$> wsP (P.choice $ map P.string reservedAggFunction)

-- | Parse binary operator
bopP :: Parser Bop
bopP =
  string2Bop
    <$> wsP
      ( P.choice $
          map P.string reservedBopS
            ++ map P.fullString reservedBopW
      )
  where
    string2Bop :: String -> Bop
    string2Bop str =
      case str of
        "+" -> Plus
        "-" -> Minus
        "*" -> Times
        "//" -> Divide
        "%" -> Modulo
        "=" -> Eq
        ">" -> Gt
        ">=" -> Ge
        "<" -> Lt
        "<=" -> Le
        "AND" -> And
        "OR" -> Or
        "LIKE" -> Like
        _ -> Is

-- | Parse unary operator
uopP :: Parser Uop
uopP =
  string2Uop
    <$> wsP
      ( P.choice $
          map P.string reservedUopS
            ++ map P.fullString reservedUopW
      )
  where
    string2Uop :: String -> Uop
    string2Uop str =
      case str of
        "-" -> Neg
        _ -> Not

---------- Section for Parsing variable --------

-- If variable is Name, can be any Name that start with alphabet and contain
-- both alphabet and numbers and underscore If variable is AllVar, parse * If
-- variable is QuotedName, can be a quote and then everything
starP :: Parser ColumnExpression
starP = AllVar <$ wsP (P.string "*")

-- Can be either single word or quoted string
nameP :: Parser String
nameP =
  P.filter
    isValidName
    ( wsP
        ( some baseP
            <|> (\s -> "\"" ++ s ++ "\"") <$> stringInP '\"' many
        )
    )
  where
    baseP :: Parser Char
    baseP = P.choice [P.alpha, P.digit, P.underscore]
    isValidName :: String -> Bool
    isValidName [] = True
    isValidName str@(x : _) =
      notElem
        str
        reservedKeyWords
        && not (Char.isDigit x)

varP :: Parser Var
varP =
  wsP
    ( (Dot <$> pFVarName <*> (P.char '.' *> varP))
        <|> (VarName <$> wsP (stringInP '\"' many))
        <|> (VarName <$> pFVarName)
        -- String name for column should be not empty)
    )

-- | Filter out key words that are reserved and only parse variable name that
-- are not reserved
pFVarName :: Parser String
pFVarName =
  P.filter
    (`notElem` reservedKeyWords)
    nameP

-- | Expression Parser
expP :: Parser Expression
expP = orBopP
  where
    orBopP = andBopP `P.chainl1` opAtLevel (level Or)
    andBopP = compP `P.chainl1` opAtLevel (level And)
    compP = sumP `P.chainl1` opAtLevel (level Gt)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      aggFunP
        <|> funP
        <|> Var <$> varP
        <|> parens expP
        <|> Val <$> dvalueP

-- | Function parser wraped in expression
funP :: Parser Expression
funP = Fun <$> functionP <*> wsP (parens expP)

-- | Aggregate function parser wraped in expression
aggFunP :: Parser Expression
aggFunP = uncurry <$> (AggFun <$> aggFunctionP) <*> aggFunPAux
  where
    aggFunPAux = wsP (parens ((,) <$> countStyleP <*> expP))

-- | Parse whether distinct or not
countStyleP :: Parser CountStyle
countStyleP = distinctStringP <|> pure All
  where
    distinctStringP :: Parser CountStyle
    distinctStringP = wsP (P.string "DISTINCT") *> pure Distinct

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- | Parse one column expression, which can be an alias or name or star
columnExpressionP :: Parser ColumnExpression
columnExpressionP =
  ColumnAlias <$> expP <*> (wsP (P.string "AS") *> nameP)
    <|> ColumnName <$> expP
    <|> starP

-- | Parse a select
exprsSelectP :: Parser (CountStyle, [ColumnExpression])
exprsSelectP =
  (,)
    <$> (wsP (P.string exprsSelectKW) *> countStyleP)
    <*> P.sepBy1 (exprsSelectPAux <|> parens exprsSelectPAux) P.comma
  where
    exprsSelectKW = "SELECT"
    exprsSelectPAux :: Parser ColumnExpression
    exprsSelectPAux = columnExpressionP

-------- Parsing From Expression --------

-- | Parse a join style
joinStyleP :: Parser JoinStyle
joinStyleP =
  str2JoinStyle
    <$> wsP
      ( P.choice
          ( map
              pWords
              [ ["LEFT", "JOIN"],
                ["RIGHT", "JOIN"],
                ["OUTER", "JOIN"],
                ["JOIN"]
              ]
          )
      )
  where
    str2JoinStyle :: String -> JoinStyle
    str2JoinStyle str =
      case str of
        "LEFT JOIN" -> LeftJoin
        "RIGHT JOIN" -> RightJoin
        "JOIN" -> InnerJoin
        _ -> OuterJoin

-- | Join name is in the form A.c = B.c
joinNamesPAux :: Parser (Var, Var)
joinNamesPAux = (,) <$> wsP varP <*> (P.equalSign *> wsP varP)

joinNamesP :: Parser JoinNames
joinNamesP = wsP (P.string joinNamesKW) *> P.sepBy joinNamesPAux (wsP P.comma)
  where
    joinNamesKW = "ON"

-- A fake data to help parsing expressions
data FakeFromExpression
  = Fake TableName
  | FakeAlias TableName TableName
  | FakeOn FakeFromExpression JoinNames
  | FakeJoin FakeFromExpression JoinStyle FakeFromExpression
  deriving (Eq, Show)

-- Parse the from expression to a fake data structure
fakeFromExpressionP :: Parser FakeFromExpression
fakeFromExpressionP = joinP
  where
    joinP = baseP `P.chainl1` joinPAux
    joinPAux ::
      Parser
        ( FakeFromExpression ->
          FakeFromExpression ->
          FakeFromExpression
        )
    joinPAux = flip FakeJoin <$> joinStyleP
    baseP =
      FakeOn <$> parens fakeFromExpressionP <*> joinNamesP
        <|> parens fakeFromExpressionP
        <|> FakeOn
          <$> ( FakeAlias
                  <$> nameP
                  <*> (wsP (P.string "AS") *> nameP)
              )
          <*> joinNamesP
        <|> FakeAlias <$> nameP <*> (wsP (P.string "AS") *> nameP)
        <|> FakeOn <$> (Fake <$> pFVarName) <*> joinNamesP
        <|> Fake <$> pFVarName

-- | Converter for transforming fake expression to real expressions
fakeFE2FE :: FakeFromExpression -> FromExpression
fakeFE2FE (Fake str) = TableRef str
fakeFE2FE (FakeAlias str1 str2) = TableAlias str1 str2
fakeFE2FE (FakeJoin ffe1 js (FakeOn ffe2 jns)) =
  let fe1 = fakeFE2FE ffe1
   in let fe2 = fakeFE2FE ffe2
       in Join fe1 js fe2 jns
fakeFE2FE (FakeJoin ffe1 js ffe2) =
  let fe1 = fakeFE2FE ffe1
   in let fe2 = fakeFE2FE ffe2
       in Join fe1 js fe2 []
fakeFE2FE (FakeOn ffe _) = fakeFE2FE ffe -- This case will not happen

-- | Parser for from clause
fromSelectP :: Parser FromExpression
fromSelectP = wsP (P.string "FROM") *> (fakeFE2FE <$> fakeFromExpressionP)

-- | Parser for where clause
whSelectP :: Parser (Maybe Expression)
whSelectP =
  catchAnyWord
    whSelectKW
    ( wsP (P.string whSelectKW) *> (Just <$> expP)
    )
    <|> return Nothing
  where
    whSelectKW = "WHERE"

-- | Parser for group by clause
groupbySelectP :: Parser [Var]
groupbySelectP =
  catchAnyWordL
    "GROUP"
    (pWords ["GROUP", "BY"] *> P.sepBy1 varP P.comma)
    <|> return []
  where
    groupbySelectKW = "GROUP BY"

-- | Parser for order by clause
orderbySelectP :: Parser [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)]
orderbySelectP =
  catchAnyWordL
    "ORDER"
    (wsP (P.string orderbySelectKW) *> P.sepBy1 orderSelectPAux P.comma)
    <|> return []
  where
    orderbySelectKW = "ORDER BY"
    orderSelectPAux :: Parser (Var, Maybe OrderTypeAD, Maybe OrderTypeFL)
    orderSelectPAux =
      (,,)
        <$> varP
        <*> optional orderTypeADP
        <*> optional orderTypeFLP

-- | Parser for limit clause
limitSelectP :: Parser (Maybe Int)
limitSelectP =
  catchAnyWord
    limitSelectKW
    (wsP (P.string limitSelectKW) *> (Just <$> P.int))
    <|> return Nothing
  where
    limitSelectKW = "LIMIT"

-- | Parser for offset clause
offsetSelectP :: Parser (Maybe Int)
offsetSelectP =
  catchAnyWord
    offsetSelectKW
    (wsP (P.string offsetSelectKW) *> (Just <$> P.int))
    <|> return Nothing
  where
    offsetSelectKW = "OFFSET"

-- | Parser for select command
scP :: Parser SelectCommand
scP =
  SelectCommand
    <$> exprsSelectP
    <*> fromSelectP
    <*> whSelectP
    <*> groupbySelectP
    <*> orderbySelectP
    <*> limitSelectP
    <*> offsetSelectP

-- | Parser for processing prefix of create command
ccPrefixP :: Parser Bool
ccPrefixP =
  wsP (P.string "CREATE")
    *> wsP (P.string "TABLE")
    *> (True <$ pWords ["IF", "NOT", "EXISTS"] <|> pure False)

-- | Parser for dtype
dTypeP :: Parser DType
dTypeP =
  str2DType
    <$> wsP
      (P.choice (map P.string ["INTEGER", "BIGINT", "BOOLEAN"]))
    <|> pIntType
    <|> pStringType
  where
    pIntType :: Parser DType
    pIntType = wsP (P.string "INT" *> (IntType <$> parens P.int))
    pStringType :: Parser DType
    pStringType = wsP (P.string "VARCHAR" *> (StringType <$> parens P.int))
    str2DType :: String -> DType
    str2DType str =
      case str of
        "INTEGER" -> IntType 16
        "BIGINT" -> IntType 32
        _ -> BoolType

-- | Parser for each column in the create command
idCreateP :: Parser (Name, DType, Bool)
idCreateP =
  (,,)
    <$> nameP
    <*> dTypeP
    <*> (True <$ pWords ["PRIMARY", "KEY"] <|> pure False)

-- | Parser for create command
ccP :: Parser CreateCommand
ccP =
  CreateCommand
    <$> ccPrefixP
    <*> nameP
    <*> parens (P.sepBy1 idCreateP P.comma)

-- | Parser for delete command
dcP :: Parser DeleteCommand
dcP = dcPrefixP *> (DeleteCommand <$> nameP <*> whSelectP)
  where
    dcPrefixP = pWords ["DELETE", "FROM"]

-- | Parser for query
queryP :: Parser Query
queryP = SelectQuery <$> scP <|> DeleteQuery <$> dcP <|> CreateQuery <$> ccP

-- | Parser for multiple queries
sqlP :: Parser Queries
sqlP = many (queryP <* wsP (P.char ';'))

-------- Support for IO --------
parseQuery :: String -> Either P.ParseError Query
parseQuery = P.parse queryP

parseSQL :: String -> Either P.ParseError Queries
parseSQL = P.parse sqlP

parseSQLFile :: String -> IO (Either P.ParseError Queries)
parseSQLFile = P.parseFromFile (sqlP <* P.eof)
