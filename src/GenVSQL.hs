{-# LANGUAGE GADTs #-}

module GenVSQL where

import Control.Monad
import Control.Monad.State qualified as S
import Data.List as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import GenSQL
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen

-- generate tables
--   - generate index
--   - generate columns, dtypes
--   - generate values
-- generate store
--   - generate names
--   - match tables
-- generate query
--   - based on tables
--   - based on columns in certain tables
--   - based on dtypes

data GenState = GenState
  { numMaxVar :: Int,
    numMaxTable :: Int,
    numMaxRow :: Int,
    numMaxExpr :: Int
  }

initGenState :: GenState
initGenState =
  GenState
    { numMaxVar = 8,
      numMaxTable = 4,
      numMaxRow = 32,
      numMaxExpr = 2
    }

genVarList :: Int -> [Var]
genVarList n = List.map (\x -> VarName ("var" ++ show x)) [1 .. n]

--      Dot ("table" ++ show n) ("var" ++ show n)

genTableList :: Int -> [TableName]
genTableList n = List.map (\x -> "table" ++ show x) [1 .. n]

genColumns :: [Var] -> [TableName] -> Gen (Map TableName [Var])
genColumns vs ts = do
  varSubsets <- QC.vectorOf (length ts) (QC.sublistOf vs)
  return $ Map.fromList (ts `zip` varSubsets)

genPrimaryKeys :: [(Var, DType)] -> Gen PrimaryKeys
genPrimaryKeys vs = NE.singleton <$> QC.elements vs

-- genDValNull :: DType -> Gen DValue
-- genDValNull (IntType i) = do
--   n <- genPos
--   QC.frequency [(n, IntVal <$> QC.chooseInt (1, 2 ^ i)), (1, pure NullVal)]
-- genDValNull (StringType i) = do
--   n <- genPos
--   QC.frequency [(n, StringVal <$> (genStringLit `QC.suchThat` (\s -> length s <= i))), (1, pure NullVal)]
-- genDValNull BoolType = do
--   n <- genPos
--   QC.frequency
--     [(n, BoolVal <$> arbitrary), (1, pure NullVal)]

genDVal :: DType -> Gen DValue
genDVal (IntType i) = IntVal <$> QC.chooseInt (1, 2 ^ i)
genDVal (StringType i) = StringVal <$> (genStringLit `QC.suchThat` (\s -> length s <= i))
genDVal BoolType = BoolVal <$> arbitrary

genRow :: [(Var, DType)] -> Gen Row
genRow [] = return Map.empty
genRow ((var, dtype) : xs) = do
  dvalue <- genDVal dtype
  rest <- genRow xs
  return (Map.singleton var dvalue `Map.union` rest)

genTData :: Int -> [(Var, DType)] -> Gen TableData
genTData i vs = do
  maxRow <- QC.chooseInt (1, i)
  QC.vectorOf maxRow (genRow vs)

genTable :: Int -> TableName -> [Var] -> Gen Table
genTable i s vs = do
  ds <- QC.vectorOf (length vs) (arbitrary :: Gen DType)
  let idxName = vs `zip` ds
  pKeys <- genPrimaryKeys idxName
  tData <- genTData i idxName
  return
    Table
      { primaryKeys = pKeys,
        indexName = idxName,
        tableData = tData
      }

genScope :: GenState -> Gen Scope
genScope g = do
  varList <- genVarList <$> QC.chooseInt (1, numMaxVar g)
  tableList <- genTableList <$> QC.chooseInt (1, numMaxTable g)
  columns <- genColumns varList tableList
  tables <- sequence $ Map.elems $ Map.mapWithKey (genTable (numMaxRow g)) columns
  return $ Map.fromList (tableList `zip` tables)

genFromExpr :: [TableName] -> Gen FromExpression
genFromExpr ts = do
  name <- QC.elements ts
  QC.elements
    [ TableRef name,
      TableAlias name (name ++ "_alias")
    ]

genColumnExpr :: Int -> [(Var, DType)] -> Gen ColumnExpression
genColumnExpr i vs = do
  resultDType <- QC.elements (List.map snd vs)
  let resultVars = List.map fst $ List.filter (\x -> resultDType == snd x) vs
  QC.frequency
    [ (8, ColumnName <$> genExpr i resultDType resultVars),
      (2, flip ColumnAlias (show i ++ "_alias") <$> genExpr i resultDType resultVars),
      (1, return AllVar)
    ]

genExpr :: Int -> DType -> [Var] -> Gen Expression
genExpr 0 _ vs = Var <$> QC.elements vs
genExpr i _ [v] = return (Var v)
genExpr i BoolType vs = do
  v1 <- Var <$> QC.elements vs
  v2 <- Var <$> QC.elements vs
  QC.elements
    [ Op1 Not v1,
      Op1 Neg v1,
      Op2 v1 Plus v2,
      Op2 v1 Minus v2,
      Op2 v1 Times v2,
      Op2 v1 Divide v2,
      Op2 v1 Modulo v2,
      Op2 v1 Eq v2,
      Op2 v1 Gt v2,
      Op2 v1 Ge v2,
      Op2 v1 Lt v2,
      Op2 v1 Le v2,
      Op2 v1 And v2,
      Op2 v1 Or v2
    ]
genExpr i (StringType _) vs = do
  v1 <- Var <$> QC.elements vs
  v2 <- Var <$> QC.elements vs
  QC.elements
    [ Op2 v1 Like v2,
      Op2 v1 Is v2
    ]
genExpr i (IntType _) vs = do
  v1 <- Var <$> QC.elements vs
  v2 <- Var <$> QC.elements vs
  QC.elements
    [ Op1 Neg v1,
      Op2 v1 Plus v2,
      Op2 v1 Minus v2,
      Op2 v1 Times v2,
      Op2 v1 Divide v2,
      Op2 v1 Modulo v2,
      Op2 v1 Eq v2,
      Op2 v1 Gt v2,
      Op2 v1 Ge v2,
      Op2 v1 Lt v2,
      Op2 v1 Le v2
    ]

genSelectBasic :: GenState -> Gen (Scope, SelectCommand)
genSelectBasic g = do
  varList <- genVarList <$> QC.chooseInt (1, numMaxVar g)
  tableList <- genTableList <$> QC.chooseInt (1, numMaxTable g)
  columns <- genColumns varList tableList

  tables <- sequence $ Map.elems $ Map.mapWithKey (genTable (numMaxRow g)) columns
  let idxNames = List.concatMap indexName tables
  let scope = Map.fromList (tableList `zip` tables)

  fromExpr <- genFromExpr (Map.keys columns)
  countStyle <- arbitrary :: Gen CountStyle
  colExpr <- genColumnExpr (numMaxExpr g) idxNames

  return
    ( scope,
      SelectCommand
        { exprsSelect = (countStyle, []),
          fromSelect = fromExpr,
          whSelect = Nothing,
          groupbySelect = [],
          orderbySelect = [],
          limitSelect = Nothing,
          offsetSelect = Nothing
        }
    )

genAggExpr :: [Var] -> Gen ColumnExpression
genAggExpr = undefined

genSelectGroupBy :: GenState -> Gen (Scope, SelectCommand)
genSelectGroupBy g = do
  varList <- genVarList <$> QC.chooseInt (1, numMaxVar g)
  tableList <- genTableList <$> QC.chooseInt (1, numMaxTable g)
  columns <- genColumns varList tableList

  tables <- sequence $ Map.elems $ Map.mapWithKey (genTable (numMaxRow g)) columns
  let scope = Map.fromList (tableList `zip` tables)

  fromExpr <- genFromExpr (Map.keys columns)
  aggExpr <- genAggExpr varList
  countStyle <- arbitrary :: Gen CountStyle
  groupByVars <- QC.sublistOf varList

  return
    ( scope,
      SelectCommand
        { exprsSelect = (countStyle, [aggExpr]),
          fromSelect = fromExpr,
          whSelect = Nothing,
          groupbySelect = groupByVars,
          orderbySelect = [],
          limitSelect = Nothing,
          offsetSelect = Nothing
        }
    )

genSelectJoin :: GenState -> Gen (Scope, SelectCommand)
genSelectJoin = undefined

genSelect :: Gen (Scope, SelectCommand)
genSelect =
  QC.oneof
    [ genSelectBasic initGenState,
      genSelectGroupBy initGenState,
      genSelectJoin initGenState
    ]

-- data SelectCommand = SelectCommand
-- { exprsSelect :: (CountStyle, [ColumnExpression]),
--   fromSelect :: FromExpression,
--   whSelect :: Maybe Expression,
--   groupbySelect :: [Var],
--   orderbySelect :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)],
--   limitSelect :: Maybe Int,
--   offsetSelect :: Maybe Int
-- }
-- deriving (Eq, Show)
