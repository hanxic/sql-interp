module TablePrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import SQLPrinter qualified as SPP
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, punctuate, render, (<+>))
import Text.PrettyPrint qualified as PP

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (SPP.PP a) => a -> String
pretty = render . SPP.pp

instance SPP.PP Row where
  pp r = PP.cat $ PP.punctuate PP.comma (map (SPP.pp . snd) $ Map.toList r)

test :: Row -> [Doc]
test r = map (SPP.pp . snd) $ Map.toList r

instance SPP.PP TableMap where
  pp = undefined