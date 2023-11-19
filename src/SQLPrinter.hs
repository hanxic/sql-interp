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

instance PP OrderType where
  pp = PP.text . show

instance PP DValue where
  pp (IntVal i) = pp i
  pp (BoolVal i) = pp i

instance PP Uop where
  pp Not = PP.text "NOT"
  pp Neg = PP.char '-'

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.text "//"
  pp Modulo = PP.text "%"
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "="
  pp And = PP.text "AND"
  pp Or = PP.text "OR"
  pp Like = PP.text "LIKE"
  pp Is = PP.text "IS"

instance PP Function where
  pp Avg = PP.text "AVG"
  pp Count = PP.text "COUNT"
  pp Max = PP.text "MAX"
  pp Min = PP.text "MIN"
  pp Sum = PP.text "SUM"
  pp Len = PP.text "LENGTH"
  pp Lower = PP.text "LOWER"
  pp Upper = PP.text "UPPER"

instance PP DValue where
  pp = undefined

instance PP Expression where
  pp = undefined