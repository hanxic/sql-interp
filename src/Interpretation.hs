{-# LANGUAGE GADTs #-}

module Interpretation where

import Data.List as List
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import Data.Set as Set
import SQLSyntax
import TableSyntax
import Test.QuickCheck

-- Empty data types
emptyScope :: Scope
emptyScope = Map.empty

emptyTableData :: TableData
emptyTableData = []

emptyIndexName :: IndexName
emptyIndexName = NE.singleton ""

emptyTable :: Table
emptyTable =
  Table
    { indexName = emptyIndexName,
      tableData = emptyTableData
    }

emptyRow :: Row
emptyRow = Map.empty

-- Helper functions
tableFMap :: (Row -> Row) -> Table -> Table
tableFMap f t = t {tableData = fmap f (tableData t)}

tableMapEither :: (Row -> Either ErrorMsg Row) -> Table -> Either ErrorMsg Table
tableMapEither f t = do
  tm <-
    List.foldr
      ( \r n -> do
          n' <- n
          r' <- f r
          Right $ r' : n'
      )
      (Right emptyTableData)
      (tableData t)
  Right $ t {tableData = tm}

tableLength :: Table -> Int
tableLength = length . tableData

-- Evaluates Query
evalQuery :: Query -> Scope -> Either ErrorMsg Table
evalQuery (SelectQuery q) = evalSelect q
evalQuery (DeleteQuery q) = evalDelete q

-- Parses SelectCommand
-- @Gary THIS IS THE MAIN WORK HORSE
-- 1. Find the table referenced in FROM
-- 2. Filter out the rows we don't care about
-- 3. Calculate new columns, 1 per Expression
-- 4. Sort the table based on SORT BY
-- 5. Slice the top N rows based on Limit/Offset
-- X. TODO GroupBy, I am thinking of branching
evalSelect :: SelectCommand -> Scope -> Either ErrorMsg Table
evalSelect q sc = do
  tableFrom <- evalFrom (fromSelect q) sc
  tableWhere <- evalWhere (whSelect q) tableFrom
  tableExpr <- evalSelectExpr (exprsSelect q) tableWhere
  tableSorted <- evalSort (orderbySelect q) tableExpr
  let tableLimited = evalLimitOffset (limitSelect q) (offsetSelect q) tableSorted
  Right tableLimited

-- Evaluates Column Expressions
evalSelectExpr :: [(CountStyle, ColumnExpression)] -> Table -> Either ErrorMsg Table
evalSelectExpr l = tableMapEither (evalListColumnExpr $ convertSelectExpr l)

-- TODO handle the DISTINCT keyword
convertSelectExpr :: [(CountStyle, ColumnExpression)] -> [ColumnExpression]
convertSelectExpr = List.map snd

-- Fold through each Exprssion which is (Row -> Row)
-- Apply to Row and collect the result
-- Any error stop the process
evalListColumnExpr :: [ColumnExpression] -> Row -> Either ErrorMsg Row
evalListColumnExpr l r =
  List.foldl
    ( \n x -> do
        n' <- n
        r' <- evalColumnExpr x r
        Right $ n' <> r'
    )
    (Right emptyRow)
    l

-- Evaluate the expression and rename the column (key) in map
evalColumnExpr :: ColumnExpression -> Row -> Either ErrorMsg Row
evalColumnExpr (ColumnName e) r = do
  v <- evalExpression e r
  Right $ Map.singleton (show e) v
evalColumnExpr (ColumnAlias e n) r = do
  v <- evalExpression e r
  Right $ Map.singleton (show n) v
evalColumnExpr AllVar r = Right r

-- Evaluates Where Clasues (filters the rows)
evalWhere :: Maybe Expression -> Table -> Either ErrorMsg Table
evalWhere (Just e) t = tableMapEither (evalWhereBool e) t
evalWhere Nothing t = Right t

evalWhereBool :: Expression -> Row -> Either ErrorMsg Row
evalWhereBool e r = case evalExpression e r of
  Right (BoolVal b) | b -> Right r
  Right (BoolVal b) | not b -> Right emptyRow
  Left s -> Left s
  _ -> Left "Where clause is not a boolean expression"

-- TODO Evaluates GroupBy
evalGroupBy :: [Var] -> Table -> Either ErrorMsg [[Row]]
evalGroupBy vs t =
  let tm = tableData t
   in if checkGroupVars vs tm
        then Right $ groupBy (boolGroupBy vs) tm
        else Left $ "Column name(s) '" ++ show vs ++ "' missing"

-- Check that all Vars are in the table
checkGroupVars :: [Var] -> TableData -> Bool
checkGroupVars vs [] = False
checkGroupVars vs (r : _) = all (`Map.member` r) vs

-- Equality for two rows based on values within key subset
boolGroupBy :: [Var] -> Row -> Row -> Bool
boolGroupBy [] r1 r2 = True
boolGroupBy (k : ks) r1 r2 = do
  let v1 = (!) r1 k
  let v2 = (!) r2 k
  v1 == v2 && boolGroupBy ks r1 r2

-- Evaluates From
evalFrom :: FromExpression -> Scope -> Either ErrorMsg Table
evalFrom (TableRef name) sc = case Map.lookup name sc of
  Just t -> Right t
  Nothing -> Left $ "Table '" ++ name ++ "' does not exist in scope"
evalFrom (SubQuery q) sc = evalSelect q sc
evalFrom (Join st (UnamedJoin f1 f2)) sc = do
  t1 <- evalFrom f1 sc
  t2 <- evalFrom f2 sc
  evalJoin st t1 t2
evalFrom (Join st (NamedJoin f1 f2 i1 i2)) sc = do
  t1 <- evalFrom f1 sc
  t2 <- evalFrom f2 sc
  evalJoin st t1 t2

-- Evaluates Joins
evalJoin :: JoinStyle -> Table -> Table -> Either ErrorMsg Table
evalJoin s t1 t2 = undefined

evalJoinMap :: JoinStyle -> TableData -> TableData -> Either ErrorMsg TableData
evalJoinMap InnerJoin t1 t2 = undefined
evalJoinMap s _ _ = Left $ "unimplemented joinstyle '" ++ show s

appendKey :: String -> Row -> Row
appendKey s = Map.foldlWithKey (\n k v -> n <> Map.singleton (k ++ s) v) emptyRow

-- Sorts Table
evalSort :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> Table -> Either ErrorMsg Table
evalSort _ _ = undefined

-- Slices Table
evalLimitOffset :: Maybe Int -> Maybe Int -> Table -> Table
evalLimitOffset (Just l) (Just o) t = t {tableData = List.take l $ List.drop o (tableData t)}
evalLimitOffset (Just l) Nothing t = t {tableData = List.take l (tableData t)}
evalLimitOffset Nothing _ t = t

-- Evaluates DeleteCommand
evalDelete :: DeleteCommand -> Scope -> Either ErrorMsg Table
evalDelete q sc = do
  tableFrom <- evalFrom (fromDelete q) sc
  evalWhereDelete (whDelete q) tableFrom

evalWhereDelete :: Maybe Expression -> Table -> Either ErrorMsg Table
evalWhereDelete (Just e) t = evalWhere (Just $ Op1 Not e) t
evalWhereDelete Nothing _ = Right emptyTable

-- Evaluates nested expressions
evalExpression :: Expression -> Row -> Either ErrorMsg DValue
evalExpression (Var s) r = evalVar s r
evalExpression (Val v) _ = Right v
evalExpression (Op1 o e) r = do
  v1 <- evalExpression e r
  evalUop o v1
evalExpression (Op2 e1 o e2) r = do
  v1 <- evalExpression e1 r
  v2 <- evalExpression e2 r
  evalBop v1 o v2
evalExpression (AggFun f _ exp) r = Left $ "Cannot call Aggregation Function '" ++ show f ++ "' on row"
evalExpression (SQLSyntax.Fun f exp) r = Left $ "Cannot call Function '" ++ show f ++ "' on row"

evalVar :: Var -> Row -> Either ErrorMsg DValue
evalVar s r = case Map.lookup s r of
  Just d -> Right d
  Nothing -> Left $ "No column named '" ++ s ++ "'"

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

evalAggFunction :: AggFunction -> GroupBy a -> Either ErrorMsg DValue
evalAggFunction Avg g = Right $ IntVal $ avgGroupBy g
evalAggFunction Count g = Right $ IntVal $ lengthGroupBy g
evalAggFunction Max g = Right $ maxGroupBy g
evalAggFunction Min g = Right $ minGroupBy g
evalAggFunction Sum g = Right $ IntVal $ sumGroupBy g

evalFunction :: SQLSyntax.Function -> GroupBy a -> Either ErrorMsg DValue
evalFunction Len g = Right $ IntVal $ lengthGroupBy g
evalFunction Lower g = Right NullVal
evalFunction Upper g = Right NullVal

-- Execution of aggregation functions
avgGroupBy :: GroupBy a -> Int
avgGroupBy g = sumGroupBy g `div` lengthGroupBy g

maxGroupBy :: GroupBy a -> DValue
maxGroupBy (SingleGroupBy v) = v
maxGroupBy (MultiGroupBy v vs) = max v (maxGroupBy vs)

minGroupBy :: GroupBy a -> DValue
minGroupBy (SingleGroupBy v) = v
minGroupBy (MultiGroupBy v vs) = min v (minGroupBy vs)

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
