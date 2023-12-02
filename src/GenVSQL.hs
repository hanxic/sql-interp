module GenVSQL where

import Control.Monad (liftM2, liftM3, mapM_, replicateM)
import Control.Monad.State (MonadState (..), State, StateT, runState, runStateT)
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

data GenV t = StateT GenState (Gen t)

class ArbitraryV a where
  arbitraryV :: GenV a

{- Need to define a few operations -}

instance ArbitraryV Int where
  arbitraryV = undefined
