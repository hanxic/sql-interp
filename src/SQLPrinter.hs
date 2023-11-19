module SQLPrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

class PP a where
  pp :: a -> Doc

pretty :: (PP a) => a -> String
pretty = PP.render . pp

oneLine :: (PP a) => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Bool where
  pp True = PP.text "TRUE"
  pp False = PP.text "False"

instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

instance PP Uop where
  pp Not = PP.text "NOT"
  pp Neg = PP.char '-'

instance PP Bop where
  pp = undefined

instance PP DValue where
  pp = undefined

instance PP Expression where
  pp = undefined

instance PP Command where
  pp = undefined