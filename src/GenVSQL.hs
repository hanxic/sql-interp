module GenVSQL where

import Control.Monad (liftM2, liftM3, mapM_, replicateM)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

data GenState = GenState
  { numVar :: Int,
    numTable :: Int,
    genStore :: Map Name Table
  }