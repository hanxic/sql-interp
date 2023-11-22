module Interpretation where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import SQLSyntax
import Test.QuickCheck

type TableMap = Map Name (Map Name DValue)

instance Arbitrary TableMap where
  arbitrary :: Gen TableMap
  arbitrary = undefined

  shrink :: TableMap -> [TableMap]
  shrink = undefined

prop_where_size :: TableMap -> Bool
prop_where_size = undefined

-- prop_where_size = size (tableWithWhereClause) <= size (table)

prop_select_all :: TableMap -> Bool
prop_select_all = undefined

-- prop_select_all = select every column == table

emptyTable :: TableMap
emptyTable = Map.empty

evalCommand :: Command -> TableMap -> TableMap
evalCommand (C v f w) t = undefined

evalVerb :: Verb -> TableMap -> TableMap
evalVerb (Select xs) t = Map.map (`restrictKeys` Set.fromList xs) t
evalVerb Drop _ = emptyTable
evalVerb (Delete xs) t = undefined
evalVerb _ _ = undefined

-- parses a csv file or finds an intermediate TableMap
evalFrom :: Command -> TableMap
evalFrom = undefined

-- TODO "Join"
evalWhere :: ConditionalStatement -> TableMap -> TableMap
evalWhere = undefined

-- evaluates nested operations like "and", "or", "like"
evalExpression :: Expression -> Maybe Value
evalExpression (Value v) = Just v
evalExpression (Op1 o e) = do
  v <- evalExpression e
  evalUop o v
evalExpression (Op2 e1 o e2) = do
  v1 <- evalExpression e1
  v2 <- evalExpression e2
  evalBop v1 o v2

evalUop :: Uop -> Value -> Maybe Value
evalUop Neg (IntVal i) = Just $ IntVal (-i)
evalUop Not (BoolVal b) = Just $ BoolVal (not b)
evalUop Len (StringVal s) = Just $ IntVal (length s)
evalUop _ _ = Nothing

-- TODO "Between"
evalBop :: Value -> Bop -> Value -> Maybe Value
evalBop (IntVal i1) Plus (IntVal i2) = Just $ IntVal (i1 + i2)
evalBop (IntVal i1) Minus (IntVal i2) = Just $ IntVal (i1 - i2)
evalBop (IntVal i1) Times (IntVal i2) = Just $ IntVal (i1 * i2)
evalBop (IntVal i1) Divide (IntVal i2) | i2 /= 0 = Just $ IntVal (div i1 i2)
evalBop (IntVal i1) Modulo (IntVal i2) = Just $ IntVal (mod i1 i2)
evalBop (IntVal i1) Eq (IntVal i2) = Just $ BoolVal (i1 == i2)
evalBop (IntVal i1) Gt (IntVal i2) = Just $ BoolVal (i1 > i2)
evalBop (IntVal i1) Ge (IntVal i2) = Just $ BoolVal (i1 >= i2)
evalBop (IntVal i1) Lt (IntVal i2) = Just $ BoolVal (i1 < i2)
evalBop (IntVal i1) Le (IntVal i2) = Just $ BoolVal (i1 <= i2)
evalBop (BoolVal b1) And (BoolVal b2) = Just $ BoolVal (b1 && b2)
evalBop (BoolVal b1) Or (BoolVal b2) = Just $ BoolVal (b1 || b2)
evalBop (StringVal s1) Concat (StringVal s2) = Just $ StringVal (s1 ++ s2)
evalBop _ _ _ = Nothing
