{-# LANGUAGE GADTs #-}

module Interpretation where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import SQLSyntax
import TableSyntax
import Test.QuickCheck

emptyScope :: Scope
emptyScope = Map.empty

emptyTableMap :: TableMap
emptyTableMap = Map.empty

emptyIndexName :: IndexName
emptyIndexName = SingleName ""

emptyTable :: Table
emptyTable =
  Table
    { indexName = emptyIndexName,
      orderName = emptyIndexName,
      tableMap = emptyTableMap
    }

emptyRow :: Row
emptyRow = Map.empty

tableFMap :: (Row -> Row) -> Table -> Table
tableFMap f t = t {tableMap = Map.map f (tableMap t)}

tableMapMaybe :: (Row -> Maybe Row) -> Table -> Table
tableMapMaybe f t = t {tableMap = Map.mapMaybe f (tableMap t)}

tableMapEither :: (Row -> Either ErrorMsg Row) -> Table -> Either ErrorMsg Table
tableMapEither f t = do
  tm <-
    Map.foldlWithKey
      ( \t' k r -> do
          t'' <- t'
          r' <- f r
          Right $ Map.insert k r' t''
      )
      (Right emptyTableMap)
      (tableMap t)
  return $ t {tableMap = tm}

-- Evaluates Query
evalQuery :: Query -> Scope -> Either ErrorMsg Table
evalQuery (SelectQuery q) = evalSelect q
evalQuery (DeleteQuery q) = evalDelete q

-- Evaluates SelectCommand
evalSelect :: SelectCommand -> Scope -> Either ErrorMsg Table
evalSelect q sc = do
  tableFrom <- evalFrom (fromSelect q) sc
  tableWhere <- evalWhere (whSelect q) tableFrom
  tableExpr <- evalSelectExpr (exprsSelect q) tableWhere
  tableSorted <- evalSort (orderbySelect q) tableExpr
  let tableLimited = evalLimitOffset (limitSelect q) (offsetSelect q) tableSorted
  Right tableLimited

-- Evaluates Select Expressions
evalSelectExpr :: [(CountStyle, ColumnExpression)] -> Table -> Either ErrorMsg Table
evalSelectExpr l = tableMapEither (evalListColumnExpr $ convertSelectExpr l)

convertSelectExpr :: [(CountStyle, ColumnExpression)] -> [ColumnExpression]
convertSelectExpr = undefined

evalListColumnExpr :: [ColumnExpression] -> Row -> Either ErrorMsg Row
evalListColumnExpr l r =
  List.foldl
    ( \n x -> do
        n' <- n
        r' <- evalColumnExpr x r
        Right $ Map.union r' n'
    )
    (Right emptyRow)
    l

evalColumnExpr :: ColumnExpression -> Row -> Either ErrorMsg Row
evalColumnExpr (ColumnName e) r = do
  v <- evalExpression e r
  Right $ Map.singleton (show e) v
evalColumnExpr (ColumnAlias e n) r = do
  v <- evalExpression e r
  Right $ Map.singleton (show n) v
evalColumnExpr AllVar r = Right r

-- Evaluates Where
evalWhere :: Maybe Expression -> Table -> Either ErrorMsg Table
evalWhere (Just e) t = tableMapEither (evalWhereBool e) t
evalWhere Nothing t = Right t

evalWhereBool :: Expression -> Row -> Either ErrorMsg Row
evalWhereBool e r = case evalExpression e r of
  Right (BoolVal b) | b -> Right r
  Right (BoolVal b) | not b -> Right emptyRow
  _ -> Left "Where clause is not a boolean expression"

-- Evaluates GroupBy
evalGroupBy :: [Var] -> Table -> Table
evalGroupBy vs t = undefined

-- Evaluates From
evalFrom :: FromExpression -> Scope -> Either ErrorMsg Table
evalFrom (TableRef name) sc = case Map.lookup name sc of
  Just t -> Right t
  Nothing -> Left $ "Table '" ++ name ++ "' does not exist in scope"
evalFrom (SubQuery q) sc = evalSelect q sc
evalFrom (Join st f1 f2) sc = do
  t1 <- evalFrom f1 sc
  t2 <- evalFrom f2 sc
  return $ evalJoin st t1 t2

-- Evaluates Joins
evalJoin :: JoinStyle -> Table -> Table -> Table
evalJoin = undefined

-- Sorts Table
evalSort :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> Table -> Either ErrorMsg Table
evalSort _ _ = undefined

-- Slices Table
evalLimitOffset :: Maybe Int -> Maybe Int -> Table -> Table
evalLimitOffset (Just l) (Just o) t = t {tableMap = Map.take l $ Map.drop o (tableMap t)}
evalLimitOffset (Just l) Nothing t = t {tableMap = Map.take l (tableMap t)}
evalLimitOffset Nothing _ t = t

-- Evaluates DeleteCommand
evalDelete :: DeleteCommand -> Scope -> Either ErrorMsg Table
evalDelete q sc = do
  tableFrom <- evalFrom (fromDelete q) sc
  evalWhereDelete (whDelete q) tableFrom

evalWhereDelete :: Maybe Expression -> Table -> Either ErrorMsg Table
evalWhereDelete (Just e) t = evalWhere (Just $ Op1 Not e) t
evalWhereDelete Nothing _ = Right emptyTable

-- Evaluates nested operations
evalExpression :: Expression -> Row -> Either ErrorMsg DValue
evalExpression (Var v) r = evalVar v r
evalExpression (Val v) _ = Right v
evalExpression (Op1 o e) r = do
  v1 <- evalExpression e r
  evalUop o v1
evalExpression (Op2 e1 o e2) r = do
  v1 <- evalExpression e1 r
  v2 <- evalExpression e2 r
  evalBop v1 o v2
evalExpression (SQLSyntax.Fun f _ exp) r = undefined

evalVar :: Var -> Row -> Either ErrorMsg DValue
evalVar (VarName s) r = case Map.lookup s r of
  Just v -> Right v
  Nothing -> Left $ "No column named '" ++ s ++ "'"
evalVar (QuotedName s) r = undefined

evalUop :: Uop -> DValue -> Either ErrorMsg DValue
evalUop Neg (IntVal i) = Right $ IntVal (-1 * i)
evalUop Not (BoolVal b) = Right $ BoolVal (not b)
evalUop o _ = Left $ "Incorrect DType for operation '" ++ show o ++ "'"

evalBop :: DValue -> Bop -> DValue -> Either ErrorMsg DValue
evalBop (IntVal i1) Plus (IntVal i2) = Right $ IntVal (i1 + i2)
evalBop (IntVal i1) Minus (IntVal i2) = Right $ IntVal (i1 - i2)
evalBop (IntVal i1) Times (IntVal i2) = Right $ IntVal (i1 * i2)
evalBop (IntVal i1) Divide (IntVal i2) | i2 /= 0 = Right $ IntVal (div i1 i2)
evalBop (IntVal i1) Divide (IntVal i2) | i2 == 0 = Right NullVal
evalBop (IntVal i1) Modulo (IntVal i2) = Right $ IntVal (mod i1 i2)
evalBop (IntVal i1) Eq (IntVal i2) = Right $ BoolVal (i1 == i2)
evalBop (IntVal i1) Gt (IntVal i2) = Right $ BoolVal (i1 > i2)
evalBop (IntVal i1) Ge (IntVal i2) = Right $ BoolVal (i1 >= i2)
evalBop (IntVal i1) Lt (IntVal i2) = Right $ BoolVal (i1 < i2)
evalBop (IntVal i1) Le (IntVal i2) = Right $ BoolVal (i1 <= i2)
evalBop (BoolVal b1) And (BoolVal b2) = Right $ BoolVal (b1 && b2)
evalBop (BoolVal b1) Or (BoolVal b2) = Right $ BoolVal (b1 || b2)
evalBop (StringVal s1) Like (StringVal s2) = Right $ BoolVal (s1 == s2)
evalBop (StringVal s1) Is (StringVal s2) = Right $ BoolVal (s1 == s2)
evalBop _ o _ = Left $ "Incorrect DType for operation '" ++ show o ++ "'"

evalFunction :: SQLSyntax.Function -> GroupBy a -> Either ErrorMsg DValue
evalFunction Avg g = Right NullVal
evalFunction Count g = Right NullVal
evalFunction Max g = Right $ maxGroupBy g
evalFunction Min g = Right NullVal
evalFunction Sum g = Right NullVal
evalFunction Len g = Right NullVal
evalFunction Lower g = Right NullVal
evalFunction Upper g = Right NullVal

avgGroupBy :: GroupBy a -> DValue
avgGroupBy g = IntVal $ sumGroupBy g `div` lengthGroupBy g

maxGroupBy :: GroupBy a -> DValue
maxGroupBy (SingleGroupBy v) = v
maxGroupBy (MultiGroupBy v vs) = max v (maxGroupBy vs)

sumGroupBy :: GroupBy a -> Int
sumGroupBy (SingleGroupBy (IntVal v)) = v
sumGroupBy (MultiGroupBy (IntVal v) vs) = 1 + sumGroupBy vs
sumGroupBy (SingleGroupBy (BoolVal v)) | v = 1
sumGroupBy (SingleGroupBy (BoolVal v)) | not v = 0
sumGroupBy (MultiGroupBy (BoolVal v) vs) | v = 1 + sumGroupBy vs
sumGroupBy (MultiGroupBy (BoolVal v) vs) | not v = 0 + sumGroupBy vs
sumGroupBy _ = 0

lengthGroupBy :: GroupBy a -> Int
lengthGroupBy (SingleGroupBy _) = 1
lengthGroupBy (MultiGroupBy _ vs) = 1 + lengthGroupBy vs