module Interpretation where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import SQLSyntax
import Test.QuickCheck

type Scope = Map TableName Table

data Table = Table
  { tableName :: TableName,
    indexName :: Name,
    colNames :: [Name],
    tableMap :: TableMap
  }

type TableMap = Map DValue (Map Name DValue)

instance Arbitrary Table where
  arbitrary :: Gen Table
  arbitrary = undefined

  shrink :: Table -> [Table]
  shrink = undefined

prop_where_size :: Table -> Bool
prop_where_size = undefined

-- prop_where_size = size (tableWithWhereClause) <= size (table)

prop_select_all :: Table -> Bool
prop_select_all = undefined

-- prop_select_all = select every column == table

emptyTable :: Table
emptyTable =
  Table
    { tableName = "",
      indexName = "",
      colNames = [],
      tableMap = Map.empty
    }

-- Evaluates nested operations
evalExpression :: Expression -> Maybe DValue
evalExpression (Val v) = Just v
evalExpression (Op1 o e) = do
  v <- evalExpression e
  evalUop o v
evalExpression (Op2 e1 o e2) = do
  v1 <- evalExpression e1
  v2 <- evalExpression e2
  evalBop v1 o v2
evalExpression (SQLSyntax.Fun f _ exp) = do
  v <- evalExpression exp
  evalFunction f v
evalExpression _ = Nothing

evalUop :: Uop -> DValue -> Maybe DValue
evalUop Neg (IntVal i) = Just $ IntVal (-1 * i)
evalUop Not (BoolVal b) = Just $ BoolVal (not b)
evalUop _ _ = Nothing

evalBop :: DValue -> Bop -> DValue -> Maybe DValue
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
evalBop (StringVal s1) Like (StringVal s2) = Just $ BoolVal (s1 == s2)
evalBop (StringVal s1) Is (StringVal s2) = Just $ BoolVal (s1 == s2)
evalBop _ _ _ = Nothing

evalFunction :: SQLSyntax.Function -> DValue -> Maybe DValue
evalFunction Avg _ = Nothing
evalFunction Count _ = Nothing
evalFunction Max _ = Nothing
evalFunction Min _ = Nothing
evalFunction Sum _ = Nothing
evalFunction Len _ = Nothing
evalFunction Lower _ = Nothing
evalFunction Upper _ = Nothing
