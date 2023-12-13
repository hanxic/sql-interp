{-# LANGUAGE GADTs #-}

module GenVSQL where

import Control.Monad
import Control.Monad.State qualified as S
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen

data GenState = GenState
  { numVar :: Int,
    numTable :: Int,
    genStore :: Map Name Table
  }

type GenV t = S.StateT GenState Gen t

{- instance Monad GenV where
  return :: a -> GenV a
  return = return
 -}
getVarCount :: GenV Int
getVarCount = do
  genState <- S.get
  return $ numVar genState

class ArbitraryV a where
  arbitraryV :: GenV a

{- Need to define a few operations -}

instance ArbitraryV Int where
  arbitraryV = undefined
