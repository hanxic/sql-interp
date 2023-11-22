module TableParser where

import Control.Applicative
import Data.Char qualified as Char
import GHC.Generics (D)
import GenSQL
import Parser (Parser)
import Parser qualified as P
import SQLPrinter (pp)
import SQLPrinter qualified as SPP
import SQLSyntax
import TableSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

prop_roundtrip_row :: Row -> Bool
prop_roundtrip_row r = P.parse rowP (TP.pretty r) == Right r

prop_roundtrip_table :: Row -> Bool
prop_roundtrip_table t = P.parse t (TP.pretty t) == Right t

rowP :: Parser Row
rowP = undefined