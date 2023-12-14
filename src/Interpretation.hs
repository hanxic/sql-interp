{-# LANGUAGE GADTs #-}

module Interpretation where

{- import Data.Bits (Bits (bitSize), FiniteBits (finiteBitSize)) -}

import Control.Applicative
import Control.Monad
import Control.Monad.Except (Except, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.State qualified as S
import Data.Char (toLower, toUpper)
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List as List
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import GenSQL qualified as GSQL
import Parser qualified as PAR
import SQLParser qualified as SPAR
import SQLPrinter qualified as SP
import SQLSyntax
import TableParser qualified as TPAR
import TablePrinter qualified as TP
import TableSyntax
  ( ErrorMsg,
    IndexAttribute (PrimaryKey),
    IndexName,
    PrimaryKeys,
    Row,
    Scope,
    Store (..),
    Table (..),
    TableData,
  )
import Test.HUnit (Counts, Test (..), assertFailure, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen.Unsafe qualified as QCGU
import Text.Printf (FieldFormat (FieldFormat))
import Text.Read (readMaybe)
import Text.Regex.Base qualified as REGEX
import Text.Regex.Posix qualified as POSIX ((=~))
import Utils

{- import Test.QuickCheck.Monadic qualified as QCM -}

type SQLI a = ExceptT String (State Store) a

{- Helper function on monad manipulation -}

getAlias :: SQLI (Map TableName TableName)
getAlias = do S.gets alias

goStEx :: a -> (a -> SQLI a) -> Store -> (Either String a, Store)
goStEx a f store = f a & runExceptT & flip runState store

prop_roundtrip_getAlias :: Store -> Bool
prop_roundtrip_getAlias store = fst (goStEx Map.empty (const getAlias) store) == Right (alias store)

getAliasName :: TableName -> SQLI TableName
getAliasName tn = do Map.findWithDefault tn tn <$> getAlias

setAlias :: TableName -> TableName -> SQLI ()
setAlias tn1 tn2 = do
  store <- S.get
  if Map.member tn1 $ scope store
    then
      let newAlias = Map.insert tn2 tn1 $ alias store
       in let newState = Store (scope store) newAlias
           in S.put newState
    else throwError "Update Alias failure"

delAlias :: TableName -> SQLI ()
delAlias tn = do
  store <- S.get
  let newAlias = Map.delete tn $ alias store
   in let newStore = Store (scope store) newAlias
       in S.put newStore

printAlias :: SQLI String
printAlias = do show <$> getAlias

getFreeNameCount :: SQLI Int
getFreeNameCount = do
  alias <- getAlias
  return $ length alias + 1

getFreeName :: SQLI TableName
getFreeName = do
  i <- getFreeNameCount
  return $ "_Table" ++ show i

prop_roundtrip_getScope :: Store -> Bool
prop_roundtrip_getScope store = fst (goStEx Map.empty (const getScope) store) == Right (scope store)

getScope :: SQLI Scope
getScope = do S.gets scope

getTable :: TableName -> SQLI (Maybe (TableName, Table))
getTable tn = do
  scope <- getScope
  case Map.lookup tn scope of
    Just table -> return $ Just (tn, table)
    _ -> return Nothing

setScope :: TableName -> Table -> SQLI Store
setScope tn t = do
  store <- S.get
  let newScope = Map.insert tn t $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore >> return newStore

delScope :: TableName -> SQLI ()
delScope tn = do
  store <- S.get
  let newScope = Map.delete tn $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore

getStore :: SQLI Store
getStore = S.get

hideScope :: SQLI a -> SQLI a
hideScope sqli = do
  store <- S.get
  result <- sqli
  S.put store
  return result

-- @Gary THIS IS THE MAIN WORK HORSE
-- 1. Find the table referenced in FROM
-- 2. Filter out the rows we don't care about
-- 3. Calculate new columns, 1 per Expression
-- 4. Sort the table based on SORT BY
-- 5. Slice the top N rows based on Limit/Offset
-- X. TODO GroupBy, I am thinking of branching

evalFrom :: FromExpression -> SQLI (TableName, Table)
evalFrom (TableRef tn) = do
  mt <- getTable tn
  case mt of
    Just tnTable -> return tnTable
    Nothing -> throwError $ "No Table for table reference: " ++ SP.pretty tn
evalFrom fe@(TableAlias tn1 tn2) = do
  mt <- getTable tn1
  case mt of
    Just tnTable ->
      setAlias tn1 tn2 >> return (tn2, snd tnTable)
    _ -> throwError $ "No Table for table alias: " ++ SP.pretty fe
evalFrom (Join fe1 js fe2 jns) = do
  tnTable1 <- evalFrom fe1
  tnTable2 <- evalFrom fe2
  (newName, newTable) <- evalJoin tnTable1 js tnTable2 jns
  setScope newName newTable
  return (newName, newTable)
evalFrom (SubQuery q) = do
  table <- evalSelectCommand q
  newName <- getFreeName
  return (newName, table)

interp :: SQLI a -> Store -> Either String a
interp = S.evalState . runExceptT

exec :: SQLI a -> Store -> Store
exec = S.execState . runExceptT

run :: SQLI a -> Store -> (Either String a, Store)
run = S.runState . runExceptT

test_evalFrom :: Test
test_evalFrom =
  "evaluate From" ~:
    TestList
      [ interp (evalFrom (TableRef "Students")) sampleStore ~?= Right ("Students", tableSampleStudents),
        interp (evalFrom (TableRef "bla")) sampleStore ~?= Left "No Table for table reference: bla",
        interp (evalFrom (TableAlias "Students" "Student1")) sampleStore ~?= Right ("Student1", tableSampleStudents),
        S.execState (runExceptT (evalFrom (TableAlias "Students" "Student1"))) sampleStore ~?= Store (scope sampleStore) (alias sampleStore `Map.union` Map.singleton "Student1" "Students"),
        interp (evalFrom (TableAlias "bla" "Student1")) sampleStore ~?= Left "No Table for table alias: bla AS Student1"
      ]

evalJoin :: (TableName, Table) -> JoinStyle -> (TableName, Table) -> JoinNames -> SQLI (TableName, Table)
evalJoin (tn1, table1) js (tn2, table2) [] = do
  freeName <- getFreeName
  let joinSpec = const $ const True
   in (freeName,) <$> evalJoinTable table1 js table2 joinSpec {- evalJoinStyle table1 OuterJoin table2 joinSpec -}
evalJoin (tn1, table1) js (tn2, table2) jns = do
  freeName <- getFreeName
  joinSpec <- getJoinSpec tn1 tn2 jns
  (freeName,) <$> evalJoinTable table1 js table2 joinSpec

{- table <- {- evalJoinStyle table1 js table2 joinSpec -}
return (freeName, table) -}

mergeIndex :: IndexName -> IndexName -> IndexName
mergeIndex in1 in2 =
  let inMap1 = Map.fromList in1
   in let inMap2 = Map.fromList in2
       in Map.toList $ Map.union inMap1 inMap2

mergePrimaryKeys :: PrimaryKeys -> PrimaryKeys -> PrimaryKeys
mergePrimaryKeys pk1 pk2 =
  NE.fromList $ mergeIndex (NE.toList pk1) (NE.toList pk2)

evalJoinTable :: Table -> JoinStyle -> Table -> (Row -> Row -> Bool) -> SQLI Table
evalJoinTable (Table pk1 in1 td1) js (Table pk2 in2 td2) joinSpec =
  let mergedPK = mergePrimaryKeys pk1 pk2
   in let mergedIN = mergeIndex in1 in2 in Table mergedPK mergedIN <$> evalJoinStyle td1 js td2 joinSpec

joinMid :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinMid td1 td2 joinSpec =
  [row1 `Map.union` row2 | row1 <- td1, row2 <- td2, joinSpec row1 row2]

test504 = joinMid (tableData tableSampleGrades) (tableData tableSampleStudents) <$> getJoinSpec "Students" "Grades" [(Dot "Students" $ VarName "student_id", Dot "Grades" $ VarName "student_id")]

test505 = interp test504 sampleStore

-- >>> test505
-- Right [fromList [(VarName "age",IntVal 20),(VarName "first_name",StringVal "John"),(VarName "gender",StringVal "Male"),(VarName "grade",IntVal 85),(VarName "last_name",StringVal "Doe"),(VarName "student_id",IntVal 1),(VarName "subject",StringVal "Math")],fromList [(VarName "age",IntVal 20),(VarName "first_name",StringVal "John"),(VarName "gender",StringVal "Male"),(VarName "grade",IntVal 78),(VarName "last_name",StringVal "Doe"),(VarName "student_id",IntVal 1),(VarName "subject",StringVal "English")],fromList [(VarName "age",IntVal 20),(VarName "first_name",StringVal "John"),(VarName "gender",StringVal "Male"),(VarName "grade",IntVal 92),(VarName "last_name",StringVal "Doe"),(VarName "student_id",IntVal 1),(VarName "subject",StringVal "History")],fromList [(VarName "age",IntVal 21),(VarName "first_name",StringVal "Jane"),(VarName "gender",StringVal "Female"),(VarName "grade",IntVal 88),(VarName "last_name",StringVal "Smith"),(VarName "student_id",IntVal 2),(VarName "subject",StringVal "English")],fromList [(VarName "age",IntVal 21),(VarName "first_name",StringVal "Jane"),(VarName "gender",StringVal "Female"),(VarName "grade",IntVal 76),(VarName "last_name",StringVal "Smith"),(VarName "student_id",IntVal 2),(VarName "subject",StringVal "History")]]

testJoinMidRes1 =
  [ fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 85), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "Math")],
    fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 78), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "English")],
    fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 92), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "History")],
    fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 88), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2), (VarName "subject", StringVal "English")],
    fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 76), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2), (VarName "subject", StringVal "History")]
  ]

test_joinMid :: Test
test_joinMid =
  "evaluate Op2" ~:
    TestList
      [ interp (joinMid (tableData tableSampleGrades) (tableData tableSampleStudents) <$> getJoinSpec "Students" "Grades" [(Dot "Students" $ VarName "student_id", Dot "Grades" $ VarName "student_id")]) sampleStore ~?= Right testJoinMidRes1
      ]

joinLeftEx :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinLeftEx td1 td2 joinSpec =
  let keys2 = keys $ head td2
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys2
       in let tdLeft = [row1 `Map.union` uninitRow | row1 <- td1, (not . any (joinSpec row1)) td2]
           in tdLeft

joinRightEx :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinRightEx td1 td2 joinSpec =
  let keys1 = keys $ head td1
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys1
       in let tdRight = [row2 `Map.union` uninitRow | row2 <- td2, (not . any (joinSpec row2)) td1]
           in tdRight

evalJoinStyle :: TableData -> JoinStyle -> TableData -> (Row -> Row -> Bool) -> SQLI TableData
evalJoinStyle td1 InnerJoin td2 joinSpec =
  return $
    joinMid td1 td2 joinSpec
evalJoinStyle td1 LeftJoin td2 joinSpec =
  if List.null td2
    then return td1
    else
      let tdLeft = joinLeftEx td1 td2 joinSpec
       in let tdMid = joinMid td1 td2 joinSpec
           in return (tdLeft ++ tdMid)
evalJoinStyle td1 RightJoin td2 joinSpec =
  if List.null td1
    then return td2
    else
      let tdRight = joinRightEx td1 td2 joinSpec
       in let tdMid = joinMid td1 td2 joinSpec
           in return (tdRight ++ tdMid)
evalJoinStyle td1 OuterJoin td2 joinSpec = do
  tdLeftMid <- evalJoinStyle td1 LeftJoin td2 joinSpec
  let tdRight = joinRightEx td1 td2 joinSpec
   in return (tdLeftMid ++ tdRight)

{- joinMid :: Table -> Table -> (Row -> Row -> Bool) -> Table
joinMid (Table pk1 in1 td1) (Table pk2 in2 td2) joinSpec =
  Table
    (mergePrimaryKeys pk1 pk2)
    (mergeIndex in1 in2)
    [row1 `Map.union` row2 | row1 <- td1, row2 <- td2, joinSpec row1 row2]

joinLeftEx :: Table -> Table -> (Row -> Row -> Bool) -> Table
joinLeftEx table1@(Table pk1 in1 td1) table2@(Table pk2 in2 td2) joinSpec =
  let keys2 = keys $ head td2
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys2
       in let tdLeft = [row1 `Map.union` uninitRow | row1 <- td1, (not . any (joinSpec row1)) td2]
           in Table
                (mergePrimaryKeys pk1 pk2)
                (mergeIndex in1 in2)
                tdLeft

joinRightEx :: Table -> Table -> (Row -> Row -> Bool) -> Table
joinRightEx table1@(Table pk1 in1 td1) table2@(Table pk2 in2 td2) joinSpec =
  let keys1 = keys $ head td1
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys1
       in let tdRight = [row2 `Map.union` uninitRow | row2 <- td2, (not . any (joinSpec row2)) td1]
           in Table
                (mergePrimaryKeys pk1 pk2)
                (mergeIndex in2 in1)
                tdRight

evalJoinStyle :: Table -> JoinStyle -> Table -> (Row -> Row -> Bool) -> SQLI Table
evalJoinStyle table1 InnerJoin table2 joinSpec =
  return $
    joinMid table1 table2 joinSpec
evalJoinStyle table1 LeftJoin table2 joinSpec =
  if List.null $ tableData table2
    then return table1
    else
      let tableLeft = joinLeftEx table1 table2 joinSpec
       in let tableMid = joinMid table1 table2 joinSpec
           in return $ Table (primaryKeys tableLeft) (indexName tableLeft) (tableData tableLeft ++ tableData tableMid)
evalJoinStyle table1 RightJoin table2 joinSpec =
  if List.null $ tableData table1
    then return table2
    else
      let tableRight = joinRightEx table1 table2 joinSpec
       in let tableMid = joinMid table1 table2 joinSpec
           in return $ Table (primaryKeys tableRight) (indexName tableRight) (tableData tableRight ++ tableData tableMid)
evalJoinStyle table1 OuterJoin table2 joinSpec = do
  tableLeftMid <- evalJoinStyle table1 LeftJoin table2 joinSpec
  let tableRight = joinRightEx table1 table2 joinSpec
   in return $ Table (primaryKeys tableLeftMid) (indexName tableLeftMid) (tableData tableLeftMid ++ tableData tableRight) -}

{-
Anything in left + anything in middle
1. Get everything in left that are not in middle
  List comprehension -> Get everything form left such that the f function is not true -> Append them with td1'
2. Get everything in middle
  Do the usual evalJoinStyle thing
3. Merge them together. Simply merge based on newtable.

-}

-- | Turn joinnames into a single conditional that must be true for two row to be match
getJoinSpec :: TableName -> TableName -> [(Var, Var)] -> SQLI (Row -> Row -> Bool)
getJoinSpec tn1 tn2 = foldM (getJoinSpecAux tn1 tn2) (const $ const True)
  where
    throwAliases :: Var -> Var -> SQLI (Row -> Row -> Bool)
    throwAliases var1 var2 = do
      {- aliasStr <- printAlias -}
      throwError $ "undefined join names: left is " ++ SP.pretty var1 ++ " right is " ++ SP.pretty var2
    getJoinSpecAux :: TableName -> TableName -> (Row -> Row -> Bool) -> (Var, Var) -> SQLI (Row -> Row -> Bool)
    getJoinSpecAux tn1 tn2 f (var1, var2) = do
      (alias1, jo1) <- case var1 of
        VarName name -> throwError "undefined join name: join name needs to be a dot format"
        Dot name jo -> return (name, jo)
      (alias2, jo2) <- case var2 of
        VarName name -> throwError "undefined join name: join name needs to be a dot format"
        Dot name jo -> return (name, jo)
      if tn1 == alias1 && tn2 == alias2
        then return $ \row1 row2 -> Map.lookup jo1 row1 == Map.lookup jo2 row2 && f row1 row2
        else
          if tn1 == alias2 && tn2 == alias1
            then return $ \row1 row2 -> Map.lookup jo2 row1 == Map.lookup jo1 row2
            else throwAliases var1 var2

evalQuery :: Query -> SQLI Table
evalQuery (SelectQuery q) = evalSelectCommand q
evalQuery (DeleteQuery q) = evalDeleteCommand q
evalQuery (CreateQuery q) = evalCreateCommand q

eval :: Queries -> SQLI ()
eval = mapM_ evalQuery

evalSelectCommand :: SelectCommand -> SQLI Table
evalSelectCommand q = do
  tableFrom <- evalFrom (fromSelect q)
  tableWhere <- evalWhere (whSelect q) tableFrom
  tableGroupBy <- evalGroupBySelect1 (groupbySelect q) (exprsSelect q) tableWhere
  tableOrder <- evalOrderBy (orderbySelect q) tableGroupBy
  tableOffset <- evalOffset (offsetSelect q) tableOrder
  tableResult <- evalLimit (limitSelect q) tableOffset
  freename <- getFreeName
  setScope freename tableResult
  return tableResult

{-   tableGroupBy <- evalGroupBySelect (groupbySelect q) (exprsSelect q) tableWhere
  tableOffset <- evalOffset (offsetSelect q) tableGroupBy
  evalLimit (limitSelect q) tableOffset -}

tSC :: String -> Store -> Table -> Test
tSC commandStr store table =
  case PAR.parse SPAR.scP commandStr of
    Left errorMsg -> TestCase (assertFailure errorMsg)
    Right sc -> interp (evalSelectCommand sc) store ~?= Right table

tStudentStar :: Test
tStudentStar =
  let selectCommandTxt = "SELECT *\nFROM Students"
   in tSC selectCommandTxt sampleStore tableSampleStudents

test201 = PAR.parse SPAR.scP "SELECT *\nFROM Students"

-- >>> test201
-- Right (SelectCommand {exprsSelect = (All,[AllVar]), fromSelect = TableRef "Students", whSelect = Nothing, groupbySelect = [], orderbySelect = [], limitSelect = Nothing, offsetSelect = Nothing})

test301 =
  Table
    { primaryKeys = NE.fromList [(VarName "student_id", IntType 32)],
      indexName = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "gender", StringType 255), (VarName "age", IntType 32)],
      tableData =
        [ fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1)],
          fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2)],
          fromList [(VarName "age", IntVal 22), (VarName "first_name", StringVal "Michael"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Johnson"), (VarName "student_id", IntVal 3)],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "Emily"), (VarName "gender", StringVal "Female"), (VarName "last_name", StringVal "Williams"), (VarName "student_id", IntVal 4)],
          fromList [(VarName "age", IntVal 23), (VarName "first_name", StringVal "Chris"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Anderson"), (VarName "student_id", IntVal 5)]
        ]
    }

test302 =
  Table
    { primaryKeys = NE.fromList [(VarName "student_id", IntType 32)],
      indexName = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "gender", StringType 255), (VarName "age", IntType 32)],
      tableData =
        [ fromList [(VarName "age", IntVal 23), (VarName "first_name", StringVal "Chris"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Anderson"), (VarName "student_id", IntVal 5)],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "Emily"), (VarName "gender", StringVal "Female"), (VarName "last_name", StringVal "Williams"), (VarName "student_id", IntVal 4)],
          fromList [(VarName "age", IntVal 22), (VarName "first_name", StringVal "Michael"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Johnson"), (VarName "student_id", IntVal 3)],
          fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2)],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1)]
        ]
    }

evalWhere :: Maybe Expression -> (TableName, Table) -> SQLI (TableName, Table)
evalWhere Nothing (tn, table) = return (tn, table)
evalWhere (Just expr) (tn, table) = do
  newTD <- evalWhereExpr expr (tableData table)
  return (tn, Table (primaryKeys table) (indexName table) newTD)

evalWhereExpr :: Expression -> TableData -> SQLI TableData
evalWhereExpr expr td = do
  exprNorm <- evalEAgg expr td
  foldrM (\r acc -> evalE exprNorm r >>= getBool >>= (\b -> if b then return (r : acc) else return acc)) [] td

evalEAgg :: Expression -> TableData -> SQLI Expression
evalEAgg v@(Var _) td = return v
evalEAgg v@(Val _) td = return v
evalEAgg (Op1 uop expr) td = Op1 uop <$> evalEAgg expr td
evalEAgg (Op2 expr1 bop expr2) td = do
  expr1' <- evalEAgg expr1 td
  expr2' <- evalEAgg expr2 td
  return $ Op2 expr1' bop expr2'
evalEAgg (Fun fun expr) td = do
  expr' <- evalEAgg expr td
  return $ Fun fun expr'
evalEAgg (AggFun aggfun cs expr) td = do
  expr' <- evalEAgg expr td
  Val <$> evalEAggAux aggfun cs expr' td
  where
    evalEAggAux :: AggFunction -> CountStyle -> Expression -> TableData -> SQLI DValue
    evalEAggAux Sum _ expr td =
      foldM
        (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc >>= (\i2 -> return $ IntVal (i1 + i2))))
        (IntVal 0)
        td
    evalEAggAux Count _ expr td = foldM (\acc r -> evalE expr r >>= (\x -> getInt acc >>= (\i -> return $ IntVal $ if x /= NullVal then i + 1 else i))) (IntVal 0) td
    -- Count non-bool expression not supported / may generate undefined behavior
    evalEAggAux Max _ expr td = foldM (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc Data.Functor.<&> (IntVal . max i1))) (IntVal 0) td
    evalEAggAux Min _ expr td = foldM (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc Data.Functor.<&> (IntVal . min i1))) (IntVal 0) td
    evalEAggAux Avg cs expr td = do
      s <- evalEAggAux Sum cs expr td
      c <- evalEAggAux Count cs expr td
      case (s, c) of
        (IntVal numerator, IntVal denomenator) -> if denomenator == 0 then return $ IntVal 0 else return $ IntVal $ numerator `div` denomenator
        (_, _) -> throwError "AggFunction Type error in Avg"

{- evalEAggNI :: DValue -> DValue -> ()SQLI DValue -}
test_evalEAgg :: Test
test_evalEAgg =
  TestList
    [ interp (evalEAgg (AggFun Sum All (Var $ VarName "var1")) []) sampleStore ~?= Right (Val $ IntVal 0),
      interp (evalEAgg (AggFun Sum All (Var $ VarName "var2")) [Map.fromList [(VarName "var1", IntVal 1)]]) sampleStore ~?= Right (Val $ IntVal 0)
    ]

-- first do a regularization in expr -> make sure its Aggfunction free
-- Map each row into some DValue
-- Then Aggregate

test1002 =
  Table
    { primaryKeys = (VarName "age", IntType 16) NE.:| [(VarName "amount", IntType 16), (VarName "country", StringType 255), (VarName "first_name", StringType 255), (VarName "item", StringType 255), (VarName "last_name", StringType 255), (VarName "order_id", IntType 16)],
      indexName = [],
      tableData =
        [ fromList [(VarName "age", IntVal 25), (VarName "amount", IntVal 400), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Keyboard"), (VarName "last_name", StringVal "Reinhardt"), (VarName "order_id", IntVal 1)],
          fromList [(VarName "age", IntVal 25), (VarName "amount", IntVal 300), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Mouse"), (VarName "last_name", StringVal "Reinhardt"), (VarName "order_id", IntVal 2)],
          fromList [(VarName "age", IntVal 22), (VarName "amount", IntVal 12000), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "David"), (VarName "item", StringVal "Monitor"), (VarName "last_name", StringVal "Robinson"), (VarName "order_id", IntVal 3)],
          fromList [(VarName "age", IntVal 31), (VarName "amount", IntVal 400), (VarName "country", StringVal "USA"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Keyboard"), (VarName "last_name", StringVal "Doe"), (VarName "order_id", IntVal 4)],
          fromList [(VarName "age", IntVal 22), (VarName "amount", IntVal 250), (VarName "country", StringVal "USA"), (VarName "first_name", StringVal "Robert"), (VarName "item", StringVal "Mousepad"), (VarName "last_name", StringVal "Luna"), (VarName "order_id", IntVal 5)],
          fromList [(VarName "age", IntVal 28), (VarName "amount", NullVal), (VarName "country", StringVal "UAE"), (VarName "first_name", StringVal "Betty"), (VarName "item", NullVal), (VarName "last_name", StringVal "Doe"), (VarName "order_id", NullVal)]
        ]
    }

test1003 =
  Table
    { primaryKeys = (VarName "age", IntType 16) NE.:| [(VarName "amount", IntType 16), (VarName "country", StringType 255), (VarName "first_name", StringType 255), (VarName "item", StringType 255), (VarName "last_name", StringType 255), (VarName "order_id", IntType 16)],
      indexName = [],
      tableData =
        [ fromList [(VarName "age", IntVal 25), (VarName "amount", IntVal 400), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Keyboard"), (VarName "last_name", StringVal "Reinhardt"), (VarName "order_id", IntVal 1)],
          fromList [(VarName "age", IntVal 25), (VarName "amount", IntVal 300), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Mouse"), (VarName "last_name", StringVal "Reinhardt"), (VarName "order_id", IntVal 2)],
          fromList [(VarName "age", IntVal 22), (VarName "amount", IntVal 12000), (VarName "country", StringVal "UK"), (VarName "first_name", StringVal "David"), (VarName "item", StringVal "Monitor"), (VarName "last_name", StringVal "Robinson"), (VarName "order_id", IntVal 3)],
          fromList [(VarName "age", IntVal 31), (VarName "amount", IntVal 400), (VarName "country", StringVal "USA"), (VarName "first_name", StringVal "John"), (VarName "item", StringVal "Keyboard"), (VarName "last_name", StringVal "Doe"), (VarName "order_id", IntVal 4)],
          fromList [(VarName "age", IntVal 22), (VarName "amount", IntVal 250), (VarName "country", StringVal "USA"), (VarName "first_name", StringVal "Robert"), (VarName "item", StringVal "Mousepad"), (VarName "last_name", StringVal "Luna"), (VarName "order_id", IntVal 5)]
        ]
    }

getInt :: DValue -> SQLI Int
getInt (IntVal i) = return i
getInt b@(BoolVal _) = getInt =<< weakCast b (IntType 32)
getInt NullVal = return 0
getInt v = throwCastError v (IntType 32)

getBool :: DValue -> SQLI Bool
getBool (BoolVal b) = return b
getBool ival@(IntVal _) = getBool =<< weakCast ival BoolType
getBool v = throwCastError v BoolType

evalE :: Expression -> Row -> SQLI DValue
evalE (Var v) r =
  case Map.lookup v r of
    Nothing -> return NullVal
    Just val -> return val
evalE (Val v) r = return v
evalE (Op2 e1 o e2) r = do
  dval1 <- evalE e1 r
  dval2 <- evalE e2 r
  evalOp2Robust o dval1 dval2
{- evalOp2 o <*> evalE e1 r <*> evalE e2 r -}
evalE (Op1 o e1) r = evalOp1 o =<< evalE e1 r
evalE (Fun f e) r = evalFun f =<< evalE e r
evalE e r = throwError $ "Illegal expression: " ++ TP.pretty e

evalOp2 :: Bop -> DValue -> DValue -> SQLI DValue
evalOp2 Plus (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = return NullVal
evalOp2 Divide (IntVal i1) (IntVal i2) = return $ IntVal (i1 `div` i2)
evalOp2 Modulo _ (IntVal i2) | i2 <= 0 = return NullVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = return $ IntVal (i1 `mod` i2)
evalOp2 Eq val1 val2 = return $ BoolVal $ val1 == val2
evalOp2 Gt val1 val2 = return $ BoolVal $ val1 > val2
evalOp2 Ge val1 val2 = return $ BoolVal $ val1 >= val2
evalOp2 Lt val1 val2 = return $ BoolVal $ val1 < val2
evalOp2 Le val1 val2 = return $ BoolVal $ val1 <= val2
evalOp2 And (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 && b2
evalOp2 Or (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 || b2
evalOp2 Like (StringVal str1) (StringVal str2) = return $ BoolVal $ str1 POSIX.=~ str2
evalOp2 Is val1 val2 = evalOp2 Eq val1 val2
evalOp2 bop NullVal _ = return NullVal -- If either is NULL, return NULL
evalOp2 bop _ NullVal = return NullVal
evalOp2 bop dval1 dval2 =
  throwExpressionError (Op2 (Val dval1) bop (Val dval2))

test_evaluateOp2 :: Test
test_evaluateOp2 =
  "evaluate Op2" ~:
    TestList
      [ interp (evalE (Op2 (Val NullVal) Eq (Val NullVal)) Map.empty) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Val $ IntVal 3) Eq (Val (IntVal 3))) Map.empty) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Val $ StringVal "CIS") Eq (Val $ StringVal "CI")) Map.empty) sampleStore ~?= Right (BoolVal False),
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) Map.empty) sampleStore ~?= Right NullVal,
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (StringVal "what"))) sampleStore ~?= Left "Illegal Casting into INT(64): \"what\"",
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (IntVal 1))) sampleStore ~?= Right (IntVal 2),
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (BoolVal False))) sampleStore ~?= Right (IntVal 1),
        interp (evalE (Op2 (Var $ VarName "student_id") Like (Val $ StringVal "Hi")) (Map.singleton (VarName "student_id") (StringVal "Hi"))) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Var $ VarName "student_id") Like (Val $ StringVal "John[1-9]")) (Map.singleton (VarName "student_id") (StringVal "John1"))) sampleStore ~?= Right (BoolVal True)
      ]

evalOp2Robust :: Bop -> DValue -> DValue -> SQLI DValue
evalOp2Robust bop dval1 dval2 =
  if level bop >= 17
    then
      weakCast dval1 (IntType 64)
        >>= (\c1 -> weakCast dval2 (IntType 64) >>= evalOp2 bop c1)
    else evalOp2 bop dval1 dval2

weakCast :: DValue -> DType -> SQLI DValue
weakCast bval@(BoolVal _) BoolType = return bval
weakCast (BoolVal b) (IntType n) = return $ IntVal $ if b then 1 else 0
weakCast ival@(IntVal i) (IntType n) = return ival
weakCast (IntVal i) BoolType = return $ BoolVal $ i == 1
weakCast sval@(StringVal _) (StringType n) = return sval
weakCast NullVal _ = return NullVal
weakCast dval dtyp = throwCastError dval dtyp

test_weakCast :: Test
test_weakCast =
  TestList
    [ interp (weakCast (IntVal 0) (IntType 1)) sampleStore ~?= Right (IntVal 0),
      interp (weakCast (IntVal 8) (IntType 4)) sampleStore ~?= Right (IntVal 8),
      interp (weakCast (BoolVal True) (IntType 3)) sampleStore ~?= Right (IntVal 1),
      interp (weakCast (StringVal "bbb") (IntType 3)) sampleStore ~?= interp (throwCastError (StringVal "bbb") (IntType 3)) sampleStore
    ]

evalOp1 :: Uop -> DValue -> SQLI DValue
evalOp1 Neg (IntVal i) = return $ IntVal (-i)
evalOp1 Neg bval@(BoolVal _) = weakCast bval (IntType 1) >>= evalOp1 Neg
evalOp1 Not (BoolVal b) = return $ BoolVal (not b)
evalOp1 Not ival@(IntVal _) = weakCast ival BoolType >>= evalOp1 Not
evalOp1 uop dval = throwExpressionError $ Op1 uop (Val dval)

evalFun :: Function -> DValue -> SQLI DValue
evalFun Len (StringVal str) = return $ IntVal $ length str
evalFun Lower (StringVal str) = return $ StringVal $ List.map toLower str
evalFun Upper (StringVal str) = return $ StringVal $ List.map toUpper str
evalFun _ NullVal = return NullVal
evalFun fun dval = throwExpressionError (Fun fun (Val dval))

evalOffset :: Maybe Int -> Table -> SQLI Table
evalOffset Nothing td = return td
evalOffset (Just i) table | i >= 0 = return $ Table (primaryKeys table) (indexName table) (List.drop i $ tableData table)
evalOffset _ _ = throwError "Offset cannot be negative"

evalLimit :: Maybe Int -> Table -> SQLI Table
evalLimit Nothing td = return td
evalLimit (Just i) table | i >= 0 = return $ Table (primaryKeys table) (indexName table) (List.take i $ tableData table)
evalLimit _ _ = throwError "Limit cannot be negative"

evalOrderBy :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> Table -> SQLI Table
evalOrderBy [] table = return table
evalOrderBy orders (Table pk iName td) = do
  ordering <- getOrdering orders
  sortedTD <- evalSort ordering td
  return $ Table pk iName sortedTD

evalSort :: (Row -> Row -> Ordering) -> TableData -> SQLI TableData
evalSort ordering td = return $ List.sortBy ordering td

{-
sort on the first variable, and group by -> repeatedly sort on the second and group by

-}

test2013 = SPAR.parseSQLFile "./test/test-suite/setup/setup.sql"

-- >>> test2013
-- Right [CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Customers", idCreate = [("customer_id",IntType 16,True),("first_name",StringType 255,False),("last_name",StringType 255,False),("age",IntType 16,False),("country",StringType 255,False)]}),CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Orders", idCreate = [("order_id",IntType 16,True),("item",StringType 255,False),("amount",IntType 16,False),("customer_id",IntType 16,False)]}),CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Shippings", idCreate = [("shipping_id",IntType 16,True),("status",StringType 255,False),("customer",IntType 16,False)]})]

test2015 = [CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Customers", idCreate = [("customer_id", IntType 16, True), ("first_name", StringType 255, False), ("last_name", StringType 255, False), ("age", IntType 16, False), ("country", StringType 255, False)]}), CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Orders", idCreate = [("order_id", IntType 16, True), ("item", StringType 255, False), ("amount", IntType 16, False), ("customer_id", IntType 16, False)]}), CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "Shippings", idCreate = [("shipping_id", IntType 16, True), ("status", StringType 255, False), ("customer", IntType 16, False)]})]

test2014 = exec (eval test2015) emptyStore

-- >>> test2014
-- Store {scope = fromList [("Customers",Table {primaryKeys = (VarName "customer_id",IntType 16) :| [], indexName = [(VarName "first_name",StringType 255),(VarName "last_name",StringType 255),(VarName "age",IntType 16),(VarName "country",StringType 255)], tableData = []}),("Orders",Table {primaryKeys = (VarName "order_id",IntType 16) :| [], indexName = [(VarName "item",StringType 255),(VarName "amount",IntType 16),(VarName "customer_id",IntType 16)], tableData = []}),("Shippings",Table {primaryKeys = (VarName "shipping_id",IntType 16) :| [], indexName = [(VarName "status",StringType 255),(VarName "customer",IntType 16)], tableData = []})], alias = fromList []}

test2017 = Store {scope = fromList [("Customers", Table {primaryKeys = (VarName "customer_id", IntType 16) NE.:| [], indexName = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "age", IntType 16), (VarName "country", StringType 255)], tableData = []}), ("Orders", Table {primaryKeys = (VarName "order_id", IntType 16) NE.:| [], indexName = [(VarName "item", StringType 255), (VarName "amount", IntType 16), (VarName "customer_id", IntType 16)], tableData = [fromList [(VarName "amount", IntVal 400), (VarName "customer_id", IntVal 4), (VarName "item", StringVal "Keyboard"), (VarName "order_id", IntVal 1)], fromList [(VarName "amount", IntVal 300), (VarName "customer_id", IntVal 4), (VarName "item", StringVal "Mouse"), (VarName "order_id", IntVal 2)], fromList [(VarName "amount", IntVal 12000), (VarName "customer_id", IntVal 3), (VarName "item", StringVal "Monitor"), (VarName "order_id", IntVal 3)], fromList [(VarName "amount", IntVal 400), (VarName "customer_id", IntVal 1), (VarName "item", StringVal "Keyboard"), (VarName "order_id", IntVal 4)], fromList [(VarName "amount", IntVal 250), (VarName "customer_id", IntVal 2), (VarName "item", StringVal "Mousepad"), (VarName "order_id", IntVal 5)]]}), ("Shippings", Table {primaryKeys = (VarName "shipping_id", IntType 16) NE.:| [], indexName = [(VarName "status", StringType 255), (VarName "customer", IntType 16)], tableData = []})], alias = fromList []}

test401 =
  Table
    { primaryKeys = (VarName "amount", IntType 16) NE.:| [(VarName "customer_id", IntType 16), (VarName "item", StringType 255), (VarName "order_id", IntType 16)],
      indexName = [],
      tableData =
        [ fromList [(VarName "amount", IntVal 400), (VarName "customer_id", IntVal 4), (VarName "item", StringVal "Keyboard"), (VarName "order_id", IntVal 1)],
          fromList [(VarName "amount", IntVal 300), (VarName "customer_id", IntVal 4), (VarName "item", StringVal "Mouse"), (VarName "order_id", IntVal 2)],
          fromList [(VarName "amount", IntVal 12000), (VarName "customer_id", IntVal 3), (VarName "item", StringVal "Monitor"), (VarName "order_id", IntVal 3)],
          fromList [(VarName "amount", IntVal 400), (VarName "customer_id", IntVal 1), (VarName "item", StringVal "Keyboard"), (VarName "order_id", IntVal 4)],
          fromList [(VarName "amount", IntVal 250), (VarName "customer_id", IntVal 2), (VarName "item", StringVal "Mousepad"), (VarName "order_id", IntVal 5)]
        ]
    }

test402 = interp (evalOrderBy [(VarName "amount", Nothing, Nothing)] test401) test2017

-- >>> test402
-- Right (Table {primaryKeys = (VarName "amount",IntType 16) :| [(VarName "customer_id",IntType 16),(VarName "item",StringType 255),(VarName "order_id",IntType 16)], indexName = [], tableData = [fromList [(VarName "amount",IntVal 250),(VarName "customer_id",IntVal 2),(VarName "item",StringVal "Mousepad"),(VarName "order_id",IntVal 5)],fromList [(VarName "amount",IntVal 300),(VarName "customer_id",IntVal 4),(VarName "item",StringVal "Mouse"),(VarName "order_id",IntVal 2)],fromList [(VarName "amount",IntVal 400),(VarName "customer_id",IntVal 4),(VarName "item",StringVal "Keyboard"),(VarName "order_id",IntVal 1)],fromList [(VarName "amount",IntVal 400),(VarName "customer_id",IntVal 1),(VarName "item",StringVal "Keyboard"),(VarName "order_id",IntVal 4)],fromList [(VarName "amount",IntVal 12000),(VarName "customer_id",IntVal 3),(VarName "item",StringVal "Monitor"),(VarName "order_id",IntVal 3)]]})

getOrdering :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> SQLI (Row -> Row -> Ordering)
getOrdering = foldrM decideOrdering (const $ const EQ) -- will not happen

test_evalOrderBy :: Test
test_evalOrderBy =
  TestList
    [ interp (evalOrderBy [(VarName "student_id", Just DESC, Nothing)] tableSampleStudents) sampleStore ~?= Right (Table (primaryKeys tableSampleStudents) (indexName tableSampleStudents) (reverse $ tableData tableSampleStudents))
    ]

-- | A helper function that, given a single order by parameter and a ordering function, returning a new ordering function with the parameter as the first decider
decideOrdering :: (Var, Maybe OrderTypeAD, Maybe OrderTypeFL) -> (Row -> Row -> Ordering) -> SQLI (Row -> Row -> Ordering)
decideOrdering (v, mad, mfl) accF = return $ \r1 r2 ->
  let (dval1, dval2) =
        ( Map.findWithDefault NullVal v r1,
          Map.findWithDefault NullVal v r2
        )
   in case mfl of
        Just NULLSFIRST ->
          case (dval1, dval2) of
            (NullVal, NullVal) -> accF r1 r2
            (NullVal, _) -> LT
            (_, NullVal) -> GT
            _ -> decideOrderingAux dval1 dval2 mad accF r1 r2
        _ ->
          case (dval1, dval2) of
            (NullVal, _) -> compare dval1 dval2
            (_, NullVal) -> compare dval1 dval2
            _ -> decideOrderingAux dval1 dval2 mad accF r1 r2
  where
    decideOrderingAux :: DValue -> DValue -> Maybe OrderTypeAD -> (Row -> Row -> Ordering) -> (Row -> Row -> Ordering)
    decideOrderingAux dval1 dval2 mad accF r1 r2 =
      case mad of
        Just DESC ->
          -- Normal ordering is ascending order
          case compare dval1 dval2 of
            EQ -> accF r1 r2
            LT -> GT
            GT -> LT
        _ -> case compare dval1 dval2 of
          EQ -> accF r1 r2
          o -> o

-- | A helper function that check whether the select expressions have aggregate function or not
hasAggFun :: [ColumnExpression] -> Bool
hasAggFun = List.foldr (\x acc -> acc || hasAggFunCol x) False
  where
    hasAggFunCol :: ColumnExpression -> Bool
    hasAggFunCol (ColumnName expr) = hasAggFunExpr expr
    hasAggFunCol (ColumnAlias expr _) = hasAggFunExpr expr
    hasAggFunCol AllVar = False

hasAggFunExpr :: Expression -> Bool
hasAggFunExpr (AggFun {}) = True
hasAggFunExpr (Op1 _ expr) = hasAggFunExpr expr
hasAggFunExpr (Op2 expr1 _ expr2) = hasAggFunExpr expr1 || hasAggFunExpr expr2
hasAggFunExpr (Fun _ expr) = hasAggFunExpr expr
hasAggFunExpr _ = False

{-
Rethink the workflow for group by
1. For every column expression, annotated it to have a type
2. evalGroupBySelect should be correct
3. The question is about type, if I annotate it first and then find primary keys: Two conditions:
  1. Either the columns cover all primary keys
  2. Or the first group key is covered in some expression as the main dominator

-}

-- | A helper function that, given the first parameter of group by, the select clause, and primary keys and index, return the newer primary keys and index
evalGroupBySelectPKIN :: Maybe Var -> [ColumnExpression] -> PrimaryKeys -> IndexName -> SQLI (PrimaryKeys, IndexName)
evalGroupBySelectPKIN fstGroup ces pk iName =
  case deterPK fstGroup ces pk of -- Determine if there exists a primary key, maybe not possible
    Nothing -> throwError "No primary key is valid"
    Just pks ->
      let newTypeMap = extractAH ces pk iName
       in let pkMap = Map.fromList $ NE.toList pks
           in let newIName =
                    -- index name is anything other than the primary key
                    Map.foldrWithKey
                      ( \var dtype acc ->
                          if var `notMember` pkMap
                            then (var, dtype) : acc
                            else acc
                      )
                      []
                      newTypeMap
               in return (pks, newIName)

annotatedMapCreate :: PrimaryKeys -> IndexName -> Map Var DType
annotatedMapCreate pk iName = Map.fromList (NE.toList pk) `Map.union` Map.fromList iName

-- | The function for grouping the table by certain columns and then select
-- For performance, group by function will essentially leverage sorting functions and groupping
annotatedSelect :: [ColumnExpression] -> PrimaryKeys -> IndexName -> SQLI (Map Expression (Var, DType))
annotatedSelect ces pk iName =
  let ahMap = annotatedMapCreate pk iName
   in foldM (go ahMap) Map.empty ces
  where
    go :: Map Var DType -> Map Expression (Var, DType) -> ColumnExpression -> SQLI (Map Expression (Var, DType))
    go ahMap aeMap AllVar =
      let eMap = Map.mapKeys Var (Map.mapWithKey (,) ahMap)
       in return $ eMap `Map.union` aeMap
    go ahMap aeMap (ColumnName expr) =
      case inferExprType expr ahMap of
        Nothing -> throwError $ "Cannot infer the type of " ++ TP.pretty expr
        Just dtype -> return $ Map.singleton expr (VarName $ TP.pretty expr, dtype) `Map.union` aeMap
    {-         >>= (\x -> return $ x `Map.union` aeMap)
              . (\x -> Map.singleton expr (VarName $ TP.pretty expr, x)) -}
    go ahMap aeMap (ColumnAlias expr alias) =
      case inferExprType expr ahMap of
        Nothing -> throwError $ "Cannot infer  the type of " ++ TP.pretty expr
        Just dtype -> return $ Map.singleton expr (VarName alias, dtype) `Map.union` aeMap

evalGroupBySelect1 :: [Var] -> (CountStyle, [ColumnExpression]) -> (TableName, Table) -> SQLI Table
evalGroupBySelect1 vars (cs, ces) (tn, Table pk iName td) = do
  aeMap <- annotatedSelect ces pk iName
  if Map.null aeMap
    then throwError "Zero valid selection"
    else evalGroupBySelectUlt vars aeMap pk iName tn td

evalGroupBySelectUlt :: [Var] -> Map Expression (Var, DType) -> PrimaryKeys -> IndexName -> TableName -> TableData -> SQLI Table
evalGroupBySelectUlt [] aeMap pk iName tn td =
  let rowGroups = if any hasAggFunExpr (keys aeMap) then [td] else List.map (: []) td
   in evalSelectRowGroups' aeMap pk iName rowGroups
evalGroupBySelectUlt vars aeMap pk iName tn td = do
  ordering <- getOrdering (List.map (,Nothing,Nothing) vars)
  td' <- evalSort ordering td
  let rowGroups = List.groupBy (\r1 r2 -> EQ == ordering r1 r2) td'
   in evalSelectRowGroups' aeMap pk iName rowGroups

-- | The use of this function is that it will actually filter every expressions to form a row.
evalSelectRowGroups' :: Map Expression (Var, DType) -> PrimaryKeys -> IndexName -> [[Row]] -> SQLI Table
evalSelectRowGroups' aeMap pk iName rowGroups =
  let newPK = Map.toList aeMap
   in do
        newTD <- foldrM (\x acc -> (: acc) <$> evalSelectAux (Map.toList aeMap) pk iName x) [] rowGroups
        return $ Table (NE.fromList $ List.map snd newPK) [] newTD
  where
    evalSelectAux :: [(Expression, (Var, DType))] -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalSelectAux ce pk iName td =
      foldM (\acc (expr, (alias, _)) -> evalExpr expr alias pk iName td <&> (acc `Map.union`)) Map.empty ce
    evalExpr :: Expression -> Var -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalExpr expr alias pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton alias resValue

{- evalGroupBySelect' :: [Var] -> (CountStyle, [ColumnExpression]) -> (TableName, Table) -> SQLI Table
evalGroupBySelect' [] (cs, ces) (tn, Table pk iName td) =
  let rowGroups = if hasAggFun ces then [td] else List.map (: []) td
   in do
        (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
        evalSelectRowGroups (cs, ces) tn pk iName newPK newIName rowGroups
evalGroupBySelect' vars selectParams@(_, ces) (tn, Table pk iName td) = do
  (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
  ordering <- getOrdering (List.map (,Nothing,Nothing) vars)
  td <- evalSort ordering td
  let rowGroups = List.groupBy (\r1 r2 -> EQ == ordering r1 r2) td
   in evalSelectRowGroups selectParams tn pk iName newPK newIName rowGroups -}

{- go AllVar ahMap = List.map (\(var, dtype) -> (Var var, (var, dtype))) $ Map.toList ahMap
go (ColumnName expr) ahMap = [(expr,VarName $ TP.pretty expr,)] -}

evalGroupBySelect :: [Var] -> (CountStyle, [ColumnExpression]) -> (TableName, Table) -> SQLI Table
-- If no group by parameter is presented
-- If there exists an aggregate function, will only present one row. If not, will present into multiple rows
evalGroupBySelect [] (cs, ces) (tn, Table pk iName td) =
  let rowGroups = if hasAggFun ces then [td] else List.map (: []) td
   in do
        (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
        evalSelectRowGroups (cs, ces) tn pk iName newPK newIName rowGroups
evalGroupBySelect vars selectParams@(_, ces) (tn, Table pk iName td) = do
  (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
  ordering <- getOrdering (List.map (,Nothing,Nothing) vars)
  td <- evalSort ordering td
  let rowGroups = List.groupBy (\r1 r2 -> EQ == ordering r1 r2) td
   in evalSelectRowGroups selectParams tn pk iName newPK newIName rowGroups

{-
Steps for select
1. For each group, do the following
  1.1. If it is aggregate, then do it with that group
  1.2. Otherwise, only select the first one
2. Need to regroup pk and iName

-}

{-
Still need to do
  1. Select DISTINCT
  2. renaming
-}

{-
How can primary keys exists
1. If the first level variables is a superset of the primary key
2. If there exists a variable that is the first element of group by
-}

{-
Determine primary key -> next step is to get the rest of them
-}

deterPK :: Maybe Var -> [ColumnExpression] -> PrimaryKeys -> Maybe PrimaryKeys
deterPK fstGroup ces pk =
  let pkMap = Map.fromList $ NE.toList pk
   in let pospkMap = foldMap (`extractPosPK` pkMap) ces
       in let identifiedPK = foldMap (\x -> maybeToList $ Map.lookup (fst x) pospkMap) pk
           in if length identifiedPK == length pk
                then Just $ NE.fromList identifiedPK
                else Just . NE.singleton =<< flip Map.lookup pospkMap =<< fstGroup

test_deterPK :: Test
test_deterPK =
  TestList
    [ deterPK Nothing [ColumnName (Var $ VarName "order_id")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Just (NE.fromList [(VarName "order_id", IntType 16)]),
      deterPK Nothing [ColumnName (Var $ VarName "order_id"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Just (NE.fromList [(VarName "order_id", IntType 16)]),
      deterPK Nothing [ColumnName (Var $ VarName "name"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Nothing,
      deterPK (Just $ VarName "name") [ColumnName (Var $ VarName "name"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Nothing,
      deterPK (Just $ VarName "name") [ColumnName (Var $ VarName "name"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Just (NE.fromList [(VarName "name", IntType 16)])
    ]

extractPosPK :: ColumnExpression -> Map Var DType -> Map Var (Var, DType)
extractPosPK AllVar typeMap = Map.mapWithKey (,) typeMap
extractPosPK (ColumnName expr) typeMap =
  let alias = VarName $ TP.pretty expr
   in case extractPosPKExpr expr alias typeMap of
        Nothing -> Map.empty
        Just pospk -> uncurry Map.singleton pospk
extractPosPK (ColumnAlias expr alias) pk =
  case extractPosPKExpr expr (VarName alias) pk of
    Nothing -> Map.empty
    Just pospk -> uncurry Map.singleton pospk

test_extractPosPK :: Test
test_extractPosPK =
  TestList
    [ extractPosPK (ColumnName (Var $ VarName "order_id")) (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order_id", IntType 16))],
      extractPosPK (ColumnAlias (Var $ VarName "order_id") "order") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order", IntType 16))],
      extractPosPK AllVar (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order_id", IntType 16)), (VarName "amount", (VarName "amount", IntType 32))]
    ]

-- | Given an expression, a variable, and a map for primary key
-- Return a variable, its alias and data type, should it exists in the
-- Primary key map
extractPosPKExpr :: Expression -> Var -> Map Var DType -> Maybe (Var, (Var, DType))
extractPosPKExpr expr@(Var var) alias typeMap =
  (\x -> (var, (alias, x))) <$> inferExprType expr typeMap
extractPosPKExpr (Val _) _ _ = Nothing
extractPosPKExpr (Op1 uop expr1) alias pk = extractPosPKExpr expr1 alias pk
extractPosPKExpr (Op2 expr1 bop expr2) alias pk =
  let pospk1 = extractPosPKExpr expr1 alias pk
   in let pospk2 = extractPosPKExpr expr2 alias pk
       in case (pospk1, pospk2) of
            (Just _, Nothing) -> pospk1
            (Nothing, Just _) -> pospk2
            _ -> Nothing
extractPosPKExpr (AggFun _ _ expr) alias pk = extractPosPKExpr expr alias pk
extractPosPKExpr (Fun _ expr) alias pk = extractPosPKExpr expr alias pk

test_extractPosPKExpr :: Test
test_extractPosPKExpr =
  TestList
    [ extractPosPKExpr (Var $ VarName "order_id") (VarName "order_id") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Just (VarName "order_id", (VarName "order_id", IntType 16)),
      extractPosPKExpr (Val $ IntVal 1) (VarName "order_id") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Nothing,
      extractPosPKExpr (Op1 Neg $ Var $ VarName "order_id") (VarName "order") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Just (VarName "order_id", (VarName "order", IntType 16))
    ]

{- extractpospkexpr expr1 -}
extractAH :: [ColumnExpression] -> PrimaryKeys -> IndexName -> Map Var DType
extractAH ces pk iName =
  let ahMap = Map.fromList (NE.toList pk) `Map.union` Map.fromList iName
   in List.foldr (\x acc -> extractAHAux x ahMap `Map.union` acc) Map.empty ces
  where
    extractAHAux :: ColumnExpression -> Map Var DType -> Map Var DType
    extractAHAux AllVar ahMap = ahMap
    extractAHAux (ColumnName expr) ahMap =
      maybe Map.empty (Map.singleton (VarName $ TP.pretty expr)) $ inferExprType expr ahMap
    extractAHAux (ColumnAlias expr alias) ahMap =
      maybe Map.empty (Map.singleton (VarName $ TP.pretty alias)) $ inferExprType expr ahMap

evalSelectRowGroups :: (CountStyle, [ColumnExpression]) -> TableName -> PrimaryKeys -> IndexName -> PrimaryKeys -> IndexName -> [[Row]] -> SQLI Table
evalSelectRowGroups (cs, ce) tn pk iName newPK newIName rowGroups =
  Table newPK newIName
    <$> foldrM
      (\x acc -> (: acc) <$> evalSelectAux ce pk iName x)
      []
      rowGroups
  where
    evalSelectAux :: [ColumnExpression] -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalSelectAux ce pk iName td =
      foldM (\acc x -> evalColumnExpr x pk iName td <&> (acc `Map.union`)) Map.empty ce
    {- foldM (\acc x -> evalColumnExpr pk iName td x) (Map.empty) ce -}
    evalColumnExpr :: ColumnExpression -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalColumnExpr (ColumnName expr) pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton (VarName $ TP.pretty expr) resValue
    evalColumnExpr (ColumnAlias expr alias) pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton (VarName alias) resValue
    evalColumnExpr AllVar pk iName td =
      return $ List.head td

-- Basically an eval expression and return a dvalue -> Do this for every columm
{-
1. Evaluate the expression, inside aggregation
2. Maybe need a fake aggfunction called same
3. Basically need to find the elements in each expression and create a newer one
4. Optimizing
-}

{-Given groupby returns an [[Row]], need information for -}

{- compare :: DValue -> DValue -> Ordering
compare (NullVal) _ = GT -}

-- >>> compare NullVal (IntVal 2)
-- GT

-- ******** Throw Errors ********

throwExpressionError :: Expression -> SQLI a
throwExpressionError expr = throwError $ "Illegal Op2: " ++ TP.pretty expr

throwCastError :: DValue -> DType -> SQLI a
throwCastError dval dtype = throwError $ "Illegal Casting into " ++ TP.pretty dtype ++ ": " ++ TP.pretty dval

{- evalE :: Expression -> Row -> Maybe Row
evalE (Op2 e1 o e2)= evalOp2 o <$> evalE  -}

{- do
  maybeTable <- getTable tn
  case maybeTable of
    Nothing -> throwError "Failure on checking From in Where clause"
    Just (_, table) ->
      case maybeExpr of
        Nothing -> return (tn, table)
        Just e  -}

{-
-- Evaluates Where Clasues (filters the rows)
evalWhere :: Maybe Expression -> Table -> Either ErrorMsg Table
evalWhere (Just e) t = tableMapEither (evalWhereBool e) t
evalWhere Nothing t = Right t

evalWhereBool :: Expression -> Row -> Either ErrorMsg Row
evalWhereBool e r = case evalExpression e r of
  Right (BoolVal b) | b -> Right r
  Right (BoolVal b) | not b -> Right emptyRow
  Left s -> Left s
  _ -> Left "Where clause is not a boolean expression"-}

-- Empty data types
emptyScope :: Scope
emptyScope = Map.empty

emptyTableData :: TableData
emptyTableData = []

emptyIndexName :: IndexName
emptyIndexName = []

emptyTable :: Table
emptyTable =
  Table
    { primaryKeys = NE.singleton (VarName "default", BoolType),
      indexName = emptyIndexName,
      tableData = emptyTableData
    }

emptyRow :: Row
emptyRow = Map.empty

emptyStore :: Store
emptyStore = Store emptyScope Map.empty

tableSampleGradesTXT = "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,English,88\n2,History,76\n3,Math,78"

tableSampleGradesPK = NE.fromList [(VarName "student_id", IntType 32), (VarName "subject", StringType 255)]

tableSampleGradesIN = [(VarName "grade", IntType 32)]

tableSampleGrades = case PAR.parse (TPAR.tableP tableSampleGradesPK tableSampleGradesIN) tableSampleGradesTXT of
  Right x -> x
  Left x -> Table tableSampleGradesPK tableSampleGradesIN []

-- >>> TP.pretty tableSampleGrades
-- "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,English,88\n2,History,76\n3,Math,78"

tableSampleStudentsTXT = "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n4,Emily,Williams,Female,20"

tableSampleStudentsPK = NE.fromList [(VarName "student_id", IntType 32)]

tableSampleStudentsIN = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "gender", StringType 255), (VarName "age", IntType 32)]

tableSampleStudents = case PAR.parse (TPAR.tableP tableSampleStudentsPK tableSampleStudentsIN) tableSampleStudentsTXT of
  Right x -> x
  Left x -> Table tableSampleStudentsPK tableSampleStudentsIN []

minimumTableStudent = [(VarName "first_name", "")]

-- >>> TP.pretty tableSampleStudents
-- "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n3,Michael,Johnson,Male,22\n4,Emily,Williams,Female,20\n5,Chris,Anderson,Male,23"

tableNumber = Table (NE.fromList [(VarName "num1", IntType 32), (VarName "num2", IntType 32)]) [(VarName "num3", StringType 255)] [Map.fromList [(VarName "num1", IntVal 1), (VarName "num2", IntVal 1), (VarName "num3", StringVal "a")]]

-- >>> TP.pretty tableNumber
-- "num1,num2,num3\n1,1,a"

sampleStore :: Store
sampleStore = Store (Map.fromList [("Students", tableSampleStudents), ("Grades", tableSampleGrades), ("Number", tableNumber)]) Map.empty

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

evalCreateCommand :: CreateCommand -> SQLI Table
evalCreateCommand (CreateCommand ifnotexists namecreate idcreate) = do
  tableMaybe <- getTable namecreate
  table <- evalIDCreate idcreate
  if ifnotexists && isJust tableMaybe
    then return emptyTable
    else setScope namecreate table >> return table

evalIDCreate :: [(Name, DType, Bool)] -> SQLI Table
evalIDCreate idcreate = do
  pkList <- mapM parseIDCreate (List.filter (\(_, _, isPrimary) -> isPrimary) idcreate)
  iName <- mapM parseIDCreate (List.filter (\(_, _, isPrimary) -> not isPrimary) idcreate)
  return $ Table (NE.fromList pkList) iName []
  where
    parseIDCreate :: (Name, DType, Bool) -> SQLI (Var, DType)
    parseIDCreate (name, dtype, isPrimary) =
      case PAR.parse SPAR.varP name of
        Right var -> return (var, dtype)
        Left _ -> throwError "Fail to parse name"

test208 = [("\"COUNT(order_id)\"", IntType 16, True)]

-- >>> interp (evalIDCreate test208) emptyStore
-- Right (Table {primaryKeys = (VarName "COUNT(order_id)",IntType 16) :| [], indexName = [], tableData = []})

evalDeleteCommand :: DeleteCommand -> SQLI Table
evalDeleteCommand (DeleteCommand fromdelete whdelete) = do
  delScope fromdelete
  return emptyTable

{- let pkList = List.foldr (\x acc -> parseIDCreate x) idcreate
 in let indexName = List.filter (\(_, _, isPrimary) -> not isPrimary) idcreate in
  return $ Table  -}

{- -- Evaluates Query
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
evalFrom :: FromExpression -> Scope -> Either ErrorMsg (TableName, Table)
evalFrom = undefined

{- evalFrom (TableRef name) sc = case Map.lookup name sc of
  Just t -> Right t
  Nothing -> Left $ "Table '" ++ name ++ "' does not exist in scope"
evalFrom (SubQuery q) sc = evalSelect q sc
evalFrom (Join fexp1 js fexp2 jn) sc = do
  t1 <- evalFrom f1 sc
  t2 <- evalFrom f2 sc
  evalJoin st t1 t2 jn -}

-- Evaluates Joins
evalJoin :: JoinStyle -> Table -> Table -> JoinNames -> Either ErrorMsg Table
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
 -}

{- test1002 = TP.pretty test1001 -}

-- >>> test1002
-- "student_id,subject,age,first_name,gender,grade,last_name\n1,Math,20,John,Male,85,Doe\n1,English,20,John,Male,78,Doe\n1,History,20,John,Male,92,Doe\n2,Math,21,Jane,Female,92,Smith\n2,English,21,Jane,Female,88,Smith\n2,History,21,Jane,Female,76,Smith\n3,Math,22,Michael,Male,78,Johnson\n3,English,22,Michael,Male,95,Johnson\n3,History,22,Michael,Male,84,Johnson\n4,Math,20,Emily,Female,90,Williams\n4,English,20,Emily,Female,85,Williams\n4,History,20,Emily,Female,88,Williams\n5,Math,23,Chris,Male,86,Anderson\n5,English,23,Chris,Male,92,Anderson\n5,History,23,Chris,Male,80,Anderson"
