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
wsP p = p <* many P.space

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

stringValP :: Parser DValue
stringValP = StringVal <$> wsP (P.between quoteChar (many $ escape (quote ==)) quoteChar)
  where
    quote :: Char
    quote = '\"'
    quoteChar :: Parser Char
    quoteChar = P.char quote

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

reservedFunction :: [String]
reservedFunction = ["AVG", "COUNT", "MAX", "MIN", "SUM", "LEN", "LOWER", "UPPER"]

functionP :: Parser Function
functionP = string2Function <$> wsP (P.choice $ map P.fullString reservedFunction)
  where
    string2Function :: String -> Function
    string2Function str =
      case str of
        "AVG" -> Avg
        "COUNT" -> Count
        "MAX" -> Max
        "MIN" -> Min
        "SUM" -> Sum
        "LENGTH" -> Len
        "LOWER" -> Lower
        _ -> Upper

reservedBopS :: [String]
reservedBopS = ["+", "-", "*", "//", "%", "=", ">=", "<=", ">", "<"]

reservedBopW :: [String]
reservedBopW = ["AND", "OR", "LIKE", "IS"]

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

reservedUopS :: [String]
reservedUopS = ["-"]

reservedUopW :: [String]
reservedUopW = ["NOT"]

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
starP = AllVar <$ wsP (P.fullString "*")

varP :: Parser Var
varP = starP <|> undefined

expP :: Parser Expression
expP = compP
  where
    compP = sumP `P.chainl1` opAtLevel (level Gt)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      Var <$> varP
        <|> parens expP
        <|> Val <$> dvalueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

scP :: Parser SelectCommand
scP = undefined

ccP :: Parser CreateCommand
ccP = undefined

dcP :: Parser DeleteCommand
dcP = undefined
