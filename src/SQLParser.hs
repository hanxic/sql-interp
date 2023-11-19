module SQLParser where

import Control.Applicative
import Data.Char qualified as Char
import Parser (Parser)
import Parser qualified as P
import SQLPrinter (pp)
import SQLPrinter qualified as SPP
import SQLSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (SPP.pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (SPP.pretty e) == Right e

prop_roundtrip_stat :: Command -> Bool
prop_roundtrip_stat s = P.parse commandP (SPP.pretty s) == Right s

valueP :: Parser Value
valueP = undefined

expP :: Parser Expression
expP = undefined

commandP :: Parser Command
commandP = undefined