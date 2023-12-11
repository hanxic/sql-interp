{-# LANGUAGE GADTs #-}

module Interpretation where

import Control.Monad
import Control.Monad.Except (Except, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.State qualified as S
import Data.Function ((&))
import Data.List as List
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import Data.Maybe (fromMaybe)
import GenSQL qualified as GSQL
import Parser qualified as PAR
import SQLParser qualified as SPAR
import SQLPrinter qualified as SP
import SQLSyntax
import TableParser qualified as TPAR
import TablePrinter qualified as TP
import TableSyntax
  ( ErrorMsg,
    IndexName,
    PrimaryKeys,
    Row,
    Scope,
    Store (..),
    Table (..),
    TableData,
  )
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen.Unsafe qualified as QCGU
import Text.Printf (FieldFormat (FieldFormat))
import Text.RE.TDFA.String
import Text.Read (readMaybe)

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

setScope :: TableName -> Table -> SQLI ()
setScope tn t = do
  store <- S.get
  let newScope = Map.insert tn t $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore

delScope :: TableName -> SQLI ()
delScope tn = do
  store <- S.get
  let newScope = Map.delete tn $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore

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

interp :: SQLI a -> Store -> Either String a
interp = S.evalState . runExceptT

test_evalFrom :: Test
test_evalFrom =
  "evaluate From" ~:
    TestList
      [ interp (evalFrom (TableRef "Students")) sampleStore ~?= Right ("Students", tableSampleStudents),
        interp (evalFrom (TableRef "bla")) sampleStore ~?= Left "No Table for table reference: bla",
        interp (evalFrom (TableAlias "Students" "Student1")) sampleStore ~?= Right ("Student1", tableSampleStudents),
        interp (evalFrom (TableAlias "bla" "Student1")) sampleStore ~?= Left "No Table for table alias: bla AS Student1"
      ]

evalJoin :: (TableName, Table) -> JoinStyle -> (TableName, Table) -> JoinNames -> SQLI (TableName, Table)
evalJoin (tn1, table1) js (tn2, table2) [] = do
  freeName <- getFreeName
  let joinSpec = const $ const True
   in (freeName,) <$> evalJoinStyle table1 OuterJoin table2 joinSpec
evalJoin (tn1, table1) js (tn2, table2) jns = do
  freeName <- getFreeName
  joinSpec <- getJoinSpec tn1 tn2 jns
  table <- evalJoinStyle table1 js table2 joinSpec
  return (freeName, table)

mergeIndex :: IndexName -> IndexName -> IndexName
mergeIndex in1 in2 =
  let inMap1 = Map.fromList in1
   in let inMap2 = Map.fromList in2
       in Map.toList $ Map.union inMap1 inMap2

mergePrimaryKeys :: PrimaryKeys -> PrimaryKeys -> PrimaryKeys
mergePrimaryKeys pk1 pk2 =
  NE.fromList $ mergeIndex (NE.toList pk1) (NE.toList pk2)

joinMid :: Table -> Table -> (Row -> Row -> Bool) -> Table
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
   in return $ Table (primaryKeys tableLeftMid) (indexName tableLeftMid) (tableData tableLeftMid ++ tableData tableRight)

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

evalSelect :: SelectCommand -> SQLI ()
evalSelect (SelectCommand expS fS whS gbS oS lS ofS) = do
  tableFrom <- evalFrom fS
  undefined

evalQuery :: Query -> SQLI ()
evalQuery (SelectQuery q) = evalSelectCommand q
evalQuery (DeleteQuery q) = evalDeleteCommand q

evalSelectCommand :: SelectCommand -> SQLI ()
evalSelectCommand q = do
  tableFrom <- evalFrom (fromSelect q)
  tableWhere <- evalWhere (whSelect q) tableFrom
  tableExpr <- evalSelectExpr (exprsSelect q)
  undefined

evalDeleteCommand :: DeleteCommand -> SQLI ()
evalDeleteCommand = undefined

evalWhere :: Maybe Expression -> (TableName, Table) -> SQLI (TableName, Table)
evalWhere Nothing (tn, table) = return (tn, table)
evalWhere (Just expr) (tn, table) = do
  newTD <- evalWhereExpr expr (tableData table)
  return (tn, Table (primaryKeys table) (indexName table) newTD)

evalWhereExpr :: Expression -> TableData -> SQLI TableData
evalWhereExpr expr td = undefined

{-   return $
    List.foldr (\x acc -> maybe [] (: acc) (evalE expr x)) [] td -}

evalE :: Expression -> Row -> SQLI DValue
evalE (Var v) r =
  case Map.lookup v r of
    Nothing -> return NullVal
    Just val -> return val
evalE (Val v) r = return v
evalE (Op2 e1 o e2) r = do
  dval1 <- evalE e1 r
  dval2 <- evalE e2 r
  evalOp2 o dval1 dval2
{- evalOp2 o <*> evalE e1 r <*> evalE e2 r -}
evalE (Op1 o e1) r = evalOp1 o =<< evalE e1 r
evalE (Fun f e) r = evalFun f <$> evalE e r
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
evalOp2 Like (StringVal str1) (StringVal str2) = undefined
evalOp2 bop dval1 dval2 = throwError $ "Illegal Op2: " ++ TP.pretty (Op2 (Val dval1) bop (Val dval2))

evalOp1 :: Uop -> DValue -> SQLI DValue
evalOp1 = undefined

evalFun :: Function -> DValue -> DValue
evalFun = undefined

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

evalSelectExpr :: [(CountStyle, ColumnExpression)] -> SQLI ()
evalSelectExpr = undefined

evalSort :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> SQLI ()
evalSort = undefined

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

tableSampleGradesTXT = "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,Math,92\n2,English,88\n2,History,76\n3,Math,78\n3,English,95\n3,History,84\n4,Math,90\n4,English,85\n4,History,88\n5,Math,86\n5,English,92\n5,History,80"

tableSampleGradesPK = NE.fromList [(VarName "student_id", IntType 32), (VarName "subject", StringType 255)]

tableSampleGradesIN = [(VarName "grade", IntType 32)]

tableSampleGrades = case PAR.parse (TPAR.tableP tableSampleGradesPK tableSampleGradesIN) tableSampleGradesTXT of
  Right x -> x
  Left x -> Table tableSampleGradesPK tableSampleGradesIN []

-- >>> TP.pretty tableSampleGrades
-- "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,Math,92\n2,English,88\n2,History,76\n3,Math,78\n3,English,95\n3,History,84\n4,Math,90\n4,English,85\n4,History,88\n5,Math,86\n5,English,92\n5,History,80"

tableSampleStudentsTXT = "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n3,Michael,Johnson,Male,22\n4,Emily,Williams,Female,20\n5,Chris,Anderson,Male,23"

tableSampleStudentsPK = NE.fromList [(VarName "student_id", IntType 32)]

tableSampleStudentsIN = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "gender", StringType 255), (VarName "age", IntType 32)]

tableSampleStudents = case PAR.parse (TPAR.tableP tableSampleStudentsPK tableSampleStudentsIN) tableSampleStudentsTXT of
  Right x -> x
  Left x -> Table tableSampleStudentsPK tableSampleStudentsIN []

minimumTableStudent = [(VarName "first_name", "")]

-- >>> TP.pretty tableSampleStudents
-- "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n3,Michael,Johnson,Male,22\n4,Emily,Williams,Female,20\n5,Chris,Anderson,Male,23"

sampleStore :: Store
sampleStore = Store (Map.fromList [("Students", tableSampleStudents), ("Grades", tableSampleGrades)]) Map.empty

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

test1000 = interp (evalFrom (Join (TableRef "Students") InnerJoin (TableRef "Grades") [(Dot "Students" (VarName "student_id"), Dot "Grades" (VarName "student_id"))])) sampleStore

test1001 =
  Table
    { primaryKeys = NE.fromList [(VarName "student_id", IntType 32), (VarName "subject", StringType 255)],
      indexName = [(VarName "age", IntType 32), (VarName "first_name", StringType 255), (VarName "gender", StringType 255), (VarName "grade", IntType 32), (VarName "last_name", StringType 255)],
      tableData =
        [ Map.fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 85), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "Math")],
          Map.fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 78), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "English")],
          Map.fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "John"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 92), (VarName "last_name", StringVal "Doe"), (VarName "student_id", IntVal 1), (VarName "subject", StringVal "History")],
          Map.fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 92), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2), (VarName "subject", StringVal "Math")],
          Map.fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 88), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2), (VarName "subject", StringVal "English")],
          Map.fromList [(VarName "age", IntVal 21), (VarName "first_name", StringVal "Jane"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 76), (VarName "last_name", StringVal "Smith"), (VarName "student_id", IntVal 2), (VarName "subject", StringVal "History")],
          fromList [(VarName "age", IntVal 22), (VarName "first_name", StringVal "Michael"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 78), (VarName "last_name", StringVal "Johnson"), (VarName "student_id", IntVal 3), (VarName "subject", StringVal "Math")],
          fromList [(VarName "age", IntVal 22), (VarName "first_name", StringVal "Michael"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 95), (VarName "last_name", StringVal "Johnson"), (VarName "student_id", IntVal 3), (VarName "subject", StringVal "English")],
          fromList [(VarName "age", IntVal 22), (VarName "first_name", StringVal "Michael"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 84), (VarName "last_name", StringVal "Johnson"), (VarName "student_id", IntVal 3), (VarName "subject", StringVal "History")],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "Emily"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 90), (VarName "last_name", StringVal "Williams"), (VarName "student_id", IntVal 4), (VarName "subject", StringVal "Math")],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "Emily"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 85), (VarName "last_name", StringVal "Williams"), (VarName "student_id", IntVal 4), (VarName "subject", StringVal "English")],
          fromList [(VarName "age", IntVal 20), (VarName "first_name", StringVal "Emily"), (VarName "gender", StringVal "Female"), (VarName "grade", IntVal 88), (VarName "last_name", StringVal "Williams"), (VarName "student_id", IntVal 4), (VarName "subject", StringVal "History")],
          fromList [(VarName "age", IntVal 23), (VarName "first_name", StringVal "Chris"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 86), (VarName "last_name", StringVal "Anderson"), (VarName "student_id", IntVal 5), (VarName "subject", StringVal "Math")],
          fromList [(VarName "age", IntVal 23), (VarName "first_name", StringVal "Chris"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 92), (VarName "last_name", StringVal "Anderson"), (VarName "student_id", IntVal 5), (VarName "subject", StringVal "English")],
          fromList [(VarName "age", IntVal 23), (VarName "first_name", StringVal "Chris"), (VarName "gender", StringVal "Male"), (VarName "grade", IntVal 80), (VarName "last_name", StringVal "Anderson"), (VarName "student_id", IntVal 5), (VarName "subject", StringVal "History")]
        ]
    }

test1002 = TP.pretty test1001

-- >>> test1002
-- "student_id,subject,age,first_name,gender,grade,last_name\n1,Math,20,John,Male,85,Doe\n1,English,20,John,Male,78,Doe\n1,History,20,John,Male,92,Doe\n2,Math,21,Jane,Female,92,Smith\n2,English,21,Jane,Female,88,Smith\n2,History,21,Jane,Female,76,Smith\n3,Math,22,Michael,Male,78,Johnson\n3,English,22,Michael,Male,95,Johnson\n3,History,22,Michael,Male,84,Johnson\n4,Math,20,Emily,Female,90,Williams\n4,English,20,Emily,Female,85,Williams\n4,History,20,Emily,Female,88,Williams\n5,Math,23,Chris,Male,86,Anderson\n5,English,23,Chris,Male,92,Anderson\n5,History,23,Chris,Male,80,Anderson"