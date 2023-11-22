module GenSQL where

import Control.Monad (mapM_)
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import SQLSYntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC

instance Arbitrary OrderTypeFL where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary OrderTypeAD where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary DValue where
  arbitrary = undefined