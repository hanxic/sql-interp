module SQLParser where

import Control.Applicative
import Data.Char qualified as Char
import GHC.Generics (D)
import GenSQL
import Parser (Parser)
import Parser qualified as P
import SQLPrinter (pp)
import SQLPrinter qualified as SPP
import SQLSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.ParserCombinators.ReadP (count)

errorMsgUnitTest :: Either String b
errorMsgUnitTest = Left "No parses"

prop_roundtrip_val :: DValue -> Bool
prop_roundtrip_val v = P.parse dvalueP (SPP.pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (SPP.pretty e) == Right e

prop_roundtrip_select :: SelectCommand -> Bool
prop_roundtrip_select sc = P.parse scP (SPP.pretty sc) == Right sc

prop_roundtrip_create :: CreateCommand -> Bool
prop_roundtrip_create cc = P.parse ccP (SPP.pretty cc) == Right cc

prop_roundtrip_delete :: DeleteCommand -> Bool
prop_roundtrip_delete dc = P.parse dcP (SPP.pretty dc) == Right dc

wsP :: Parser a -> Parser a
wsP p = many P.space *> p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP str = constP str ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
      P.parse (many (stringP "a")) "a  a  " ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 5, tried = 5, errors = 0, failures = 1}

constP :: String -> a -> Parser a
constP str a = a <$ wsP (P.string str)

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
      P.parse (many (constP "&" 'a')) "&   &     " ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

orderTypeADP :: Parser OrderTypeAD
orderTypeADP = wsP (ASC <$ P.string "ASC" <|> DESC <$ P.string "DESC")

orderTypeFLP :: Parser OrderTypeFL
orderTypeFLP = wsP (NULLSFIRST <$ P.string "NULLS FIRST" <|> NULLSLAST <$ P.string "NULLS LAST")

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]
intValP :: Parser DValue
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser DValue
boolValP = trueP <|> falseP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ wsP (P.string "TRUE")
    falseP :: Parser DValue
    falseP = BoolVal False <$ wsP (P.string "FALSE")

-- >>> P.parse (many nullValP) "NULL NULL\n NULL"
-- Right [NullVal,NullVal,NullVal]
nullValP :: Parser DValue
nullValP = NullVal <$ wsP (P.string "NULL")

-- | escape function will take in a function
-- and will parse the character that does not make the function return false
escape :: (Char -> Bool) -> Parser Char
escape f = P.satisfy (not . f)

stringInP :: Char -> (Parser Char -> Parser String) -> Parser String
stringInP c f = wsP (P.between cP (f $ escape (c ==)) cP)
  where
    cP = P.char c

stringValP :: Parser DValue
stringValP = StringVal <$> stringInP '\"' many

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\""
        ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\""
        ~?= Right [StringVal " a", StringVal "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

dvalueP :: Parser DValue
dvalueP = intValP <|> boolValP <|> nullValP <|> stringValP

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

functionP :: Parser Function
functionP =
  string2Function <$> wsP (P.choice $ map P.string reservedFunction)

test_functionP :: Test
test_functionP =
  TestList
    [ P.parse functionP "LOWER" ~?= Right Lower,
      P.parse functionP "UPPER" ~?= Right Upper,
      P.parse functionP "LENGTH" ~?= Right Len,
      P.parse functionP "*" ~?= errorMsgUnitTest
    ]

aggFunctionP :: Parser AggFunction
aggFunctionP =
  string2AggFunction <$> wsP (P.choice $ map P.string reservedAggFunction)

test_aggFunctionP :: Test
test_aggFunctionP =
  TestList
    [ P.parse aggFunctionP "AVG" ~?= Right Avg,
      P.parse aggFunctionP "COUNT " ~?= Right Count,
      P.parse aggFunctionP "MAX " ~?= Right Max,
      P.parse aggFunctionP "MIN " ~?= Right Min,
      P.parse aggFunctionP "SUM " ~?= Right Sum,
      P.parse functionP "*" ~?= errorMsgUnitTest
    ]

bopP :: Parser Bop
bopP = string2Bop <$> wsP (P.choice $ map P.string reservedBopS ++ map P.fullString reservedBopW)
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

uopP :: Parser Uop
uopP = string2Uop <$> wsP (P.choice $ map P.string reservedUopS ++ map P.fullString reservedUopW)
  where
    string2Uop :: String -> Uop
    string2Uop str =
      case str of
        "-" -> Neg
        _ -> Not

test_uopP :: Test
test_uopP =
  TestList
    [ P.parse uopP "-" ~?= Right Neg,
      P.parse uopP "NOT" ~?= Right Not,
      P.parse uopP "NOTN" ~?= Left "No parses",
      P.parse (many uopP) "- NOT" ~?= Right [Neg, Not],
      P.parse uopP "+" ~?= Left "No parses" -- "+" nor a unary operator
    ]

test_bopP :: Test
test_bopP =
  TestList
    [ P.parse bopP "+" ~?= Right Plus,
      P.parse bopP "-" ~?= Right Minus,
      P.parse bopP "*" ~?= Right Times,
      P.parse bopP "//" ~?= Right Divide,
      P.parse bopP "%" ~?= Right Modulo,
      P.parse bopP "=" ~?= Right Eq,
      P.parse bopP ">" ~?= Right Gt,
      P.parse bopP ">=" ~?= Right Ge,
      P.parse bopP "<" ~?= Right Lt,
      P.parse bopP "<=" ~?= Right Le,
      P.parse bopP "AND" ~?= Right And,
      P.parse bopP "OR" ~?= Right Or,
      P.parse bopP "LIKE" ~?= Right Like,
      P.parse bopP "IS" ~?= Right Is,
      P.parse bopP "ISs" ~?= Left "No parses",
      P.parse (many bopP) "+ - * // %" ~?= Right [Plus, Minus, Times, Divide, Modulo],
      P.parse bopP "!" ~?= Left "No parses" -- "!" not a binary operator
    ]

-- | Parsing variable.
-- If variable is Name, can be any Name that start with alphabet and contain both alphabet and numbers and underscore
-- If variable is AllVar, parse *
-- If variable is QuotedName, can be a quote and then everything
starP :: Parser ColumnExpression
starP = AllVar <$ wsP (P.string "*")

nameP :: Parser String
nameP = P.filter isValidName (wsP $ some baseP)
  where
    baseP :: Parser Char
    baseP = P.choice [P.alpha, P.digit, P.underscore]
    isValidName :: String -> Bool
    isValidName [] = True
    isValidName str@(x : _) = notElem str reservedKeyWords && not (Char.isDigit x)

varP :: Parser Var
varP =
  wsP
    ( (Dot <$> pFVarName <*> (P.char '.' *> varP))
        <|> (VarName <$> pFVarName)
        -- String name for column should be not empty)
    )

pFVarName :: Parser String
pFVarName =
  P.filter
    (`notElem` reservedKeyWords)
    nameP

-- >>> P.parse varP "1st"
-- Left "No parses"

test_varP :: Test
test_varP =
  TestList
    [ P.parse varP "*" ~?= errorMsgUnitTest,
      P.parse varP "1st" ~?= errorMsgUnitTest,
      P.parse varP "st1" ~?= Right (VarName "st1"),
      P.parse varP "\"\"" ~?= errorMsgUnitTest,
      -- TODO: not dealing with "\t" here. Maybe we should?
      P.parse varP "st.st1" ~?= Right (Dot "st" $ VarName "st1")
    ]

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

funP :: Parser Expression
funP = Fun <$> functionP <*> wsP (parens expP)

aggFunP :: Parser Expression
aggFunP = uncurry <$> (AggFun <$> aggFunctionP) <*> aggFunPAux
  where
    aggFunPAux = wsP (parens ((,) <$> countStyleP <*> expP))

-- >>> P.parse (parens expP) "(1)"
-- Right (Val (IntVal 1))

-- >>> P.parse aggFunP "AVG(*)"
-- Right (AggFun Avg All (Var AllVar))

-- >>> P.parse expP "*"
-- Right (Var AllVar)

test_aggFunP :: Test
test_aggFunP =
  TestList
    [ P.parse aggFunP "AVG(*)" ~?= errorMsgUnitTest,
      P.parse aggFunP "SUM(1 + 2)" ~?= Right (AggFun Sum All (Op2 (Val $ IntVal 1) Plus (Val $ IntVal 2))),
      P.parse aggFunP "COUNT(DISTINCT 1)" ~?= Right (AggFun Count Distinct (Val $ IntVal 1)),
      P.parse aggFunP "COUNT(DISTINCT a)" ~?= Right (AggFun Count Distinct (Var $ VarName "a"))
    ]

test3 :: Parser (CountStyle, Expression)
test3 = parens ((,) <$> countStyleP <*> expP)

countStyleP :: Parser CountStyle
countStyleP = distinctStringP <|> pure All
  where
    distinctStringP :: Parser CountStyle
    distinctStringP = wsP (P.string "DISTINCT") *> pure Distinct

test_countStyleP :: Test
test_countStyleP =
  TestList
    [ P.parse countStyleP "*" ~?= Right All,
      P.parse countStyleP "DISTINCT" ~?= Right Distinct,
      P.parse countStyleP "    DISTINCT" ~?= Right Distinct,
      P.parse countStyleP "    DISTINCT      " ~?= Right Distinct,
      P.doParse countStyleP "DISTINCT s" ~?= Just (Distinct, "s"),
      P.doParse countStyleP "DISTIN " ~?= Just (All, "DISTIN ")
    ]

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

catchAnyWord :: String -> Parser (Maybe a) -> Parser (Maybe a)
catchAnyWord kw = flip (<|>) (empty <$ P.filter (/= kw) (P.lookAhead (wsP P.anyWord)))

catchAnyWordL :: String -> Parser [a] -> Parser [a]
catchAnyWordL kw = flip (<|>) (empty <$ P.filter (/= kw) (P.lookAhead (wsP P.anyWord)))

columnExpressionP :: Parser ColumnExpression
columnExpressionP = ColumnAlias <$> expP <*> (wsP (P.string "AS") *> nameP) <|> ColumnName <$> expP <|> starP

exprsSelectP :: Parser [(CountStyle, ColumnExpression)]
exprsSelectP =
  wsP (P.string exprsSelectKW) *> P.sepBy1 (exprsSelectPAux <|> parens exprsSelectPAux) P.comma
  where
    exprsSelectKW = "SELECT"
    exprsSelectPAux :: Parser (CountStyle, ColumnExpression)
    exprsSelectPAux = (,) <$> countStyleP <*> columnExpressionP

test_exprsSelectP :: Test
test_exprsSelectP =
  TestList
    [ P.parse exprsSelectP "SELECT A" ~?= Right [(All, ColumnName $ Var $ VarName "A")],
      P.parse exprsSelectP "SELECT A AS Something" ~?= Right [(All, ColumnAlias (Var $ VarName "A") "Something")],
      P.parse exprsSelectP "SELECT DISTINCT A AS Something" ~?= Right [(Distinct, ColumnAlias (Var $ VarName "A") "Something")],
      P.parse exprsSelectP "SELECT A A" ~?= Right [(All, ColumnName $ Var $ VarName "A")],
      P.parse exprsSelectP "SELECT A AS Something, B AS C" ~?= Right [(All, ColumnAlias (Var $ VarName "A") "Something"), (All, ColumnAlias (Var $ VarName "B") "C")],
      P.parse exprsSelectP "SELECT" ~?= errorMsgUnitTest
    ]

pWordsCons :: String -> Parser String -> Parser String
pWordsCons str p = (\x xs -> x ++ " " ++ xs) <$> wsP (P.fullString str) <*> p

pWords :: [String] -> Parser String
{- pWords [] str = P.string str
pWords (x : xs) str = pStringCons x (pWords xs str) -}
pWords xs =
  case splitLast xs of
    Nothing -> empty
    Just (xs, x) -> foldr pWordsCons (P.string x) xs

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs) = splitLast xs >>= (\(xs', x') -> return (x : xs', x'))

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

{-
joinStyleP :: Parser JoinStyle
joinStyleP = str2JoinStyle <$> wsP (P.choice (map P.string ["LEFT JOIN", "RIGHT JOIN", "OUTER JOIN", "JOIN"]))
  where
    str2JoinStyle :: String -> JoinStyle
    str2JoinStyle str =
      case str of
        "LEFT JOIN" -> LeftJoin
        "RIGHT JOIN" -> RightJoin
        "JOIN" -> InnerJoin
        _ -> OuterJoin -}

optionalParens :: Parser a -> Parser a
optionalParens p = p <|> wsP (parens p)

optionalSpaceParens :: Parser a -> Parser a
optionalSpaceParens = optionalParens . wsP

joinNamesPAux :: Parser (Var, Var)
joinNamesPAux = (,) <$> wsP varP <*> (P.equalSign *> wsP varP)

test_joinNamesPAux :: Test
test_joinNamesPAux =
  TestList
    [ P.parse joinNamesPAux " A = B" ~?= Right (VarName "A", VarName "B"),
      P.parse joinNamesPAux "A.C = B.C" ~?= Right (Dot "A" $ VarName "C", Dot "B" $ VarName "C")
    ]

-- >>> P.parse joinNamesPAux "A = B"
-- Right (VarName "A",VarName "B")

joinNamesP :: Parser JoinNames
joinNamesP = wsP (P.string joinNamesKW) *> P.sepBy joinNamesPAux (wsP P.comma)
  where
    joinNamesKW = "ON"

-- >>> P.doParse joinNamesP "ON A = B"
-- Just ([(VarName "A",VarName "B")],"")

test_joinNamesP :: Test
test_joinNamesP =
  TestList
    [ P.parse joinNamesP "ON A = B, C = D" ~?= Right [(VarName "A", VarName "B"), (VarName "C", VarName "D")],
      P.parse joinNamesP "ON A.C = B.C, E.F   = G.H" ~?= Right [(Dot "A" $ VarName "C", Dot "B" $ VarName "C"), (Dot "E" $ VarName "F", Dot "G" $ VarName "H")]
    ]

data FakeFromExpression
  = Fake TableName
  | FakeAlias TableName TableName
  | FakeOn FakeFromExpression JoinNames
  | FakeJoin FakeFromExpression JoinStyle FakeFromExpression
  deriving (Eq, Show)

fakeFromExpressionP :: Parser FakeFromExpression
fakeFromExpressionP = joinP
  where
    joinP = baseP `P.chainl1` joinPAux
    joinPAux :: Parser (FakeFromExpression -> FakeFromExpression -> FakeFromExpression)
    joinPAux = flip FakeJoin <$> joinStyleP
    baseP =
      FakeOn <$> parens fakeFromExpressionP <*> joinNamesP
        <|> parens fakeFromExpressionP
        <|> FakeOn <$> (FakeAlias <$> nameP <*> (wsP (P.string "AS") *> nameP)) <*> joinNamesP
        <|> FakeAlias <$> nameP <*> (wsP (P.string "AS") *> nameP)
        <|> FakeOn <$> (Fake <$> pFVarName) <*> joinNamesP
        <|> Fake <$> pFVarName

test_fakeFromExpressionP :: Test
test_fakeFromExpressionP =
  TestList
    [ P.parse fakeFromExpressionP "A JOIN B" ~?= Right (FakeJoin (Fake "A") InnerJoin (Fake "B")),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y" ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y, A.X = B.Y" ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y, A.X = B.Y JOIN C ON Z = W" ~?= Right (FakeJoin (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])) InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")])),
      P.parse fakeFromExpressionP "A JOIN (B JOIN C ON Z = W) ON X = Y, A.X = B.Y " ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (FakeJoin (Fake "B") InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")])) [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN (B JOIN C ON Z = W) ON X = Y, A.X = B.Y "
        ~?= Right
          ( FakeJoin
              (Fake "A")
              InnerJoin
              ( FakeOn
                  (FakeJoin (Fake "B") InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")]))
                  [ (VarName "X", VarName "Y"),
                    ( Dot "A" $ VarName "X",
                      Dot "B" $ VarName "Y"
                    )
                  ]
              )
          ),
      P.parse fakeFromExpressionP "A AS C JOIN B AS D ON X = Y, A.X = B.Y " ~?= Right (FakeJoin (FakeAlias "A" "C") InnerJoin (FakeOn (FakeAlias "B" "D") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")]))
    ]

-- >>> P.doParse fakeFromExpressionP "A JOIN (B JOIN C ON Z = W)"
-- Just (Fake "A","JOIN (B JOIN C ON Z = W)")

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
{-
fromSelectPAux :: Parser FromExpression
fromSelectPAux = joinP
  where
    joinP = baseP `P.chainl1` joinPAux
    joinPAux :: Parser (FromExpression -> FromExpression -> FromExpression)
    joinPAux = undefined
    baseP =
      TableAlias <$> nameP <*> (wsP (P.string "AS") *> nameP)
        <|> TableRef
          <$> pFVarName
        <|> SubQuery <$> parens scP
        <|> parens fromSelectPAux -}
{-
test_fromSelectPAux :: Test
test_fromSelectPAux =
  TestList
    [ P.parse fromSelectPAux "A" ~?= Right (TableRef "A"),
      P.parse fromSelectPAux "A JOIN B" ~?= Right (Join (TableRef "A") InnerJoin (TableRef "B") []),
      P.parse fromSelectPAux "A AS C JOIN B" ~?= Right (Join (TableAlias "A" "C") InnerJoin (TableRef "B") []),
      P.parse fromSelectPAux "A AS C JOIN B AS D ON E= F "
        ~?= Right
          (Join (TableAlias "A" "C") InnerJoin (TableAlias "B" "D") [(VarName "E", VarName "F")]),
      P.parse
        fromSelectPAux
        "FROM A JOIN B LEFT JOIN C"
        ~?= Right (Join (Join (TableRef "A") InnerJoin (TableRef "B") []) LeftJoin (TableRef "C") [])
    ] -}

{- opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP-}

{- fromSelectP :: Parser FromExpression
fromSelectP = wsP (P.string "FROM") *> fromSelectPAux -}
fromSelectP :: Parser FromExpression
fromSelectP = wsP (P.string "FROM") *> (fakeFE2FE <$> fakeFromExpressionP {- <|> SubQuery <$> parens scP -})

test_fromSelectP :: Test
test_fromSelectP =
  TestList
    [ P.parse fromSelectP "FROM A" ~?= Right (TableRef "A"),
      P.parse fromSelectP "FROM A JOIN B" ~?= Right (Join (TableRef "A") InnerJoin (TableRef "B") []),
      P.parse fromSelectP "FROM A JOIN B LEFT JOIN C" ~?= Right (Join (Join (TableRef "A") InnerJoin (TableRef "B") []) LeftJoin (TableRef "C") []),
      P.parse fromSelectP "FROM A JOIN B ON C = D" ~?= Right (Join (TableRef "A") InnerJoin (TableRef "B") [(VarName "C", VarName "D")])
    ]

whSelectP :: Parser (Maybe Expression)
whSelectP =
  catchAnyWord whSelectKW (wsP (P.string whSelectKW) *> (Just <$> expP))
  where
    whSelectKW = "WHERE"

groupbySelectP :: Parser [Var]
groupbySelectP =
  catchAnyWordL "GROUP" (pWords ["GROUP", "BY"] *> P.sepBy1 varP P.comma)
  where
    groupbySelectKW = "GROUP BY"

test_groupbySelectP :: Test
test_groupbySelectP =
  TestList
    [ P.parse groupbySelectP "GROUP BY" ~?= errorMsgUnitTest,
      P.parse groupbySelectP "GROUP" ~?= errorMsgUnitTest,
      P.parse groupbySelectP "" ~?= errorMsgUnitTest,
      P.parse groupbySelectP "GROUP BY S" ~?= Right [VarName "S"]
    ]

orderbySelectP :: Parser [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)]
orderbySelectP =
  catchAnyWordL "ORDER" (wsP (P.string orderbySelectKW) *> P.sepBy1 orderSelectPAux P.comma)
  where
    orderbySelectKW = "ORDER BY"
    orderSelectPAux :: Parser (Var, Maybe OrderTypeAD, Maybe OrderTypeFL)
    orderSelectPAux = (,,) <$> varP <*> optional orderTypeADP <*> optional orderTypeFLP

test_orderbySelectP :: Test
test_orderbySelectP =
  TestList
    [ P.parse orderbySelectP "ORDER" ~?= errorMsgUnitTest,
      P.parse orderbySelectP "ORDER BY" ~?= errorMsgUnitTest,
      P.parse orderbySelectP "" ~?= errorMsgUnitTest, -- Not the right test cases
      P.parse orderbySelectP "ORDER BY S" ~?= Right [(VarName "S", Nothing, Nothing)],
      P.parse orderbySelectP "ORDER BY S ASC, A DESC" ~?= Right [(VarName "S", Just ASC, Nothing), (VarName "A", Just DESC, Nothing)],
      P.parse orderbySelectP "ORDER BY S ASC, A NULLS FIRST " ~?= Right [(VarName "S", Just ASC, Nothing), (VarName "A", Nothing, Just NULLSFIRST)]
    ]

limitSelectP :: Parser (Maybe Int)
limitSelectP =
  catchAnyWord limitSelectKW (wsP (P.string limitSelectKW) *> (Just <$> P.int))
  where
    limitSelectKW = "LIMIT"

{- P.maybeParse
  (wsP $ P.string "LIMIT")
  P.int
  (P.parsePredicate (const False) (P.fullString "LIMIT")) -}

test_limitSelectP :: Test
test_limitSelectP =
  TestList
    [ P.parse limitSelectP "LIMIT" ~?= errorMsgUnitTest,
      P.parse limitSelectP "LIMIT 1" ~?= Right (Just 1),
      P.parse limitSelectP "    " ~?= errorMsgUnitTest,
      P.parse limitSelectP "    ;" ~?= Right Nothing
    ]

{- wsP (P.string "LIMIT") *> wsP (Just <$> P.int) <|> pure Nothing -}

offsetSelectP :: Parser (Maybe Int)
offsetSelectP =
  catchAnyWord offsetSelectKW (wsP (P.string offsetSelectKW) *> (Just <$> P.int))
  where
    {- wsP (P.string "OFFSET") *> (Just <$> P.int)
      <|> Nothing <$ P.lookAhead (wsP P.anyWord) -}

    offsetSelectKW = "OFFSET"

test_offsetSelectP :: Test
test_offsetSelectP =
  TestList
    [ P.parse offsetSelectP "OFFSET" ~?= errorMsgUnitTest,
      P.parse offsetSelectP "OFFSET 1" ~?= Right (Just 1),
      P.parse offsetSelectP "   " ~?= errorMsgUnitTest,
      P.parse offsetSelectP " ;" ~?= Right Nothing
    ]

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

test7 =
  SelectCommand
    { exprsSelect = [(All, AllVar), (Distinct, ColumnAlias (Fun Upper (Var (Dot "Table0" $ VarName "Var0"))) "Table3"), (Distinct, AllVar), (Distinct, ColumnAlias (Fun Len (Var (VarName "Var3"))) "Table2")],
      fromSelect = Join (TableRef "Table0") RightJoin (Join (TableRef "Table5") LeftJoin (TableRef "Table0") [(VarName "Var0", Dot "Table2" $ VarName "Var2"), (Dot "Table0" $ VarName "Var2", VarName "Var3"), (VarName "Var3", Dot "Table4" $ VarName "Var3")]) [(Dot "Table0" $ VarName "Var2", Dot "Table0" $ VarName "Var2"), (VarName "Var0", VarName "Var3"), (VarName "Var0", VarName "Var0")],
      whSelect = Nothing,
      groupbySelect = [Dot "Table2" $ VarName "Var0", Dot "Table1" $ VarName "Var3", VarName "Var2"],
      orderbySelect = [(Dot "Table0" $ VarName "Var0", Nothing, Nothing), (Dot "Table5" $ VarName "Var0", Just DESC, Just NULLSFIRST), (VarName "Var4", Nothing, Just NULLSFIRST)],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

-- >>> SPP.pretty test7
-- "SELECT *, DISTINCT UPPER(Table0.Var0) AS Table3, DISTINCT *, DISTINCT LENGTH(Var3) AS Table2\n FROM Table0 RIGHT JOIN (Table5 LEFT JOIN Table0 ON Var0=Table2.Var2,Table0.Var2=Var3,Var3=Table4.Var3) ON Table0.Var2=Table0.Var2,Var0=Var3,Var0=Var0\n GROUP BY Table2.Var0, Table1.Var3, Var2\n ORDER BY Table0.Var0, Table5.Var0 DESC NULLS FIRST, Var4 NULLS FIRST;"

test8 :: String
test8 = "SELECT *, DISTINCT UPPER(Table0.Var0) AS Table3, DISTINCT *, DISTINCT LENGTH(Var3) AS Table2\n FROM Table0 RIGHT JOIN (Table5 LEFT JOIN Table0 ON Var0=Table2.Var2,Table0.Var2=Var3,Var3=Table4.Var3) ON Table0.Var2=Table0.Var2,Var0=Var3,Var0=Var0\n GROUP BY Table2.Var0, Table1.Var3, Var2\n ORDER BY Table0.Var0, Table5.Var0 DESC NULLS FIRST, Var4 NULLS FIRST;"

-- >>> P.parse scP test8
-- Right (SelectCommand {exprsSelect = [(All,AllVar),(Distinct,ColumnAlias (Fun Upper (Var (Dot "Table0" "Var0"))) "Table3"),(Distinct,AllVar),(Distinct,ColumnAlias (Fun Len (Var (VarName "Var3"))) "Table2")], fromSelect = Join (TableRef "Table0") RightJoin (Join (TableRef "Table5") LeftJoin (TableRef "Table0") [(VarName "Var0",Dot "Table2" "Var2"),(Dot "Table0" "Var2",VarName "Var3"),(VarName "Var3",Dot "Table4" "Var3")]) [], whSelect = Nothing, groupbySelect = [], orderbySelect = [], limitSelect = Nothing, offsetSelect = Nothing})

test14 =
  SelectCommand
    { exprsSelect = [(All, AllVar), (Distinct, ColumnAlias (Fun Upper (Var (Dot "Table0" $ VarName "Var0"))) "Table3"), (Distinct, AllVar), (Distinct, ColumnAlias (Fun Len (Var (VarName "Var3"))) "Table2")],
      fromSelect = Join (TableRef "Table0") RightJoin (Join (TableRef "Table5") LeftJoin (TableRef "Table0") [(VarName "Var0", Dot "Table2" $ VarName "Var2"), (Dot "Table0" $ VarName "Var2", VarName "Var3"), (VarName "Var3", Dot "Table4" $ VarName "Var3")]) [],
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

test11 = "FROM Table0 RIGHT JOIN (Table5 LEFT JOIN Table0 ON Var0=Table2.Var2,Table0.Var2=Var3,Var3=Table4.Var3) ON Table0.Var2=Table0.Var2,Var0=Var3,Var0=Var0\n "

test16 = "FROM (Table5 LEFT JOIN Table4) OUTER JOIN Table0 ON Table0.Var0=Var0,Var2=Var0,Var0=Table1.Var0\n "

test12 = fromSelectP

-- >>> P.doParse test12 test11
-- Just (Join (TableRef "Table0") RightJoin (Join (TableRef "Table5") LeftJoin (TableRef "Table0") [(VarName "Var0",Dot "Table2" "Var2"),(Dot "Table0" "Var2",VarName "Var3"),(VarName "Var3",Dot "Table4" "Var3")]) [],"ON Table0.Var2=Table0.Var2,Var0=Var3,Var0=Var0\n ")

ccPrefixP :: Parser Bool
ccPrefixP =
  wsP (P.string "CREATE")
    *> wsP (P.string "TABLE")
    *> (True <$ pWords ["IF", "NOT", "EXISTS"] <|> pure False)

test_ccPrefixP :: Test
test_ccPrefixP =
  TestList
    [ P.parse ccPrefixP "CREATE" ~?= errorMsgUnitTest,
      P.parse ccPrefixP "CREATE  TABLE" ~?= Right False,
      P.parse ccPrefixP "CREATE TABLE IF" ~?= Right False,
      P.parse ccPrefixP " CREATE TABLE IF NOT EXISTS" ~?= Right True
    ]

dTypeP :: Parser DType
dTypeP = str2DType <$> wsP (P.choice (map P.string ["INTEGER", "BIGINT", "BOOLEAN"])) <|> pIntType <|> pStringType
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

-- >>>

{- data DType
  = StringType Int
  | IntType Int
  | BoolType
  deriving (Eq, Show)-}

idCreateP :: Parser (Name, DType, Bool, Bool)
idCreateP =
  (,,,)
    <$> nameP
    <*> dTypeP
    <*> (True <$ pWords ["NOT", "NULL"] <|> pure False)
    <*> (True <$ pWords ["PRIMARY", "KEY"] <|> pure False)

test_idCreateP :: Test
test_idCreateP =
  TestList
    [ P.parse idCreateP "id BIGINT NOT NULL PRIMARY KEY" ~?= Right ("id", IntType 32, True, True),
      P.parse idCreateP "id BOOLEAN NOT NULL PRIMARY KEY" ~?= Right ("id", BoolType, True, True),
      P.parse idCreateP "Var4 INT(26) PRIMARY KEY" ~?= Right ("Var4", IntType 26, False, True)
    ]

ccP :: Parser CreateCommand
ccP = CreateCommand <$> ccPrefixP <*> nameP <*> parens (P.sepBy1 idCreateP P.comma)

test9 = "Var4 INT(26)"

test13 = "(Var0 BOOLEAN, Var1 BOOLEAN NOT NULL PRIMARY KEY, Var1 VARCHAR(159) NOT NULL)"

test10 = idCreateP

-- >>> P.parse test10 test9
-- Left "No parses"

dcP :: Parser DeleteCommand
dcP = dcPrefixP *> (DeleteCommand <$> nameP <*> whSelectP)
  where
    dcPrefixP = pWords ["DELETE", "FROM"]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_wsP,
        test_stringP,
        test_constP,
        test_stringValP,
        test_functionP,
        test_aggFunctionP,
        test_uopP,
        test_bopP,
        test_varP,
        test_aggFunP,
        test_countStyleP,
        test_exprsSelectP,
        test_joinNamesPAux,
        test_joinNamesP,
        test_fakeFromExpressionP,
        test_fromSelectP,
        test_groupbySelectP,
        test_orderbySelectP,
        test_limitSelectP,
        test_offsetSelectP,
        test_ccPrefixP,
        test_idCreateP
      ]

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_select"
  QC.quickCheck prop_roundtrip_select
  putStrLn "roundtrip_create"
  QC.quickCheck prop_roundtrip_create
  putStrLn "roundtrip_delete"
  QC.quickCheck prop_roundtrip_delete
