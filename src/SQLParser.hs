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
stringValP = StringVal <$> stringInP '\'' many

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\'a\'" ~?= Right (StringVal "a"),
      P.parse stringValP "\'a\\\'\'" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\'a\'   \'b\'"
        ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\' a\'   \'b\'"
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
starP :: Parser Var
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
    ( starP
        <|> ( VarName
                <$> P.filter
                  (`notElem` reservedKeyWords)
                  nameP
            )
        <|> ( QuotedName
                <$> P.filter
                  (`notElem` reservedKeyWords)
                  (stringInP '\"' some)
            )
            -- String name for column should be not empty)
    )

-- >>> P.parse varP "1st"
-- Left "No parses"

test_varP :: Test
test_varP =
  TestList
    [ P.parse varP "*" ~?= Right AllVar,
      P.parse varP "1st" ~?= errorMsgUnitTest,
      P.parse varP "st1" ~?= Right (VarName "st1"),
      P.parse varP "\"\"" ~?= errorMsgUnitTest,
      -- TODO: not dealing with "\t" here. Maybe we should?
      P.parse varP "\"1st\"" ~?= Right (QuotedName "1st")
    ]

-- >>> P.parse expP "1 AND b OR 1"
-- Right (Op2 (Op2 (Val (IntVal 1)) And (Var (VarName "b"))) Or (Val (IntVal 1)))

test =
  Op2 (AggFun Count Distinct (Val (IntVal 3028638))) Plus (Op1 Not (Op1 Neg (Fun Upper (Val (StringVal "\23278\DC2 q9m\158086\SO\1002059\DC3\b)\39083'\1108928")))))

{- Fun Len Distinct (Val (StringVal "\993599")) -}

-- >>> SPP.pretty test
-- "COUNT(DISTINCT 3028638) + NOT - UPPER('\23278\DC2 q9m\158086\SO\1002059\DC3\b)\39083'\1108928')"

test2 = "COUNT(DISTINCT 3028638) + NOT - UPPER('\23278\DC2 q9m\158086\SO\1002059\DC3\b)\39083'\1108928')"

-- >>> P.parse expP test2
-- Right (AggFun Count Distinct (Val (IntVal 3028638)))

-- >>> Char.ord '\''
-- 39

test4 =
  Op2
    (Fun Lower (Val (StringVal "2\1046076\46352\1096575 \983403")))
    Plus
    (Op2 (AggFun Avg Distinct (Var (VarName "Var2"))) And (Fun Len (Var (QuotedName "Var5"))))

-- >>> P.parse aggFunP test2
-- Left "No parses"

-- >>> P.parse expP "(Var0 IS NULL) % LENGTH('QR\1053562I'N')"
-- Right (Op2 (Var (VarName "Var0")) Is (Val NullVal))

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
    [ P.parse aggFunP "AVG(*)" ~?= Right (AggFun Avg All (Var AllVar)),
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

exprsSelectP :: Parser [(CountStyle, ColumnExpression)]
exprsSelectP = undefined

fromSelectP :: Parser FromExpression
fromSelectP = undefined

whSelectP :: Parser (Maybe Expression)
whSelectP =
  catchAnyWord whSelectKW (wsP (P.string whSelectKW) *> (Just <$> expP))
  where
    whSelectKW = "WHERE"

groupbySelectP :: Parser [Var]
groupbySelectP =
  catchAnyWordL "GROUP" (wsP (P.string groupbySelectKW) *> P.sepBy1 varP P.comma)
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
scP = undefined

ccP :: Parser CreateCommand
ccP = undefined

dcP :: Parser DeleteCommand
dcP = undefined
