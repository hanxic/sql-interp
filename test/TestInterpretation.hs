module TestInterpretation where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import Data.List qualified as List
import Data.List qualified as List (sort)
import Data.List.NonEmpty qualified as NE (fromList, sort)
import Data.Map
import Data.Map qualified as Map (empty, fromList, lookup, singleton, union)
import Interpretation
import Parser qualified as P
import SQLParser qualified as SPAR
import SQLSyntax
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath
import TableParser qualified as TPAR
import TablePrinter qualified as TP
import TableSyntax
import Test.HUnit
import TestUtils

type IOS a = StateT IOStore IO a

testPath = "./test/test-suite"

setupPath = testPath </> "setup"

setupSQL = "setup"

setupCSVs = ["Customers", "Orders", "Shippings"]

csvformat = "csv"

sqlformat = "sql"

data IOStore = IOStore
  { currentQueries :: Queries,
    stack :: Stack,
    outputLoc :: OutputLoc
  }

type Stack = Store

data OutputLoc
  = File FilePath
  | Terminal

testedit = "    "

getStack :: IOS Stack
getStack = do gets stack

setStack :: Stack -> IOS ()
setStack newStore = do
  ioStore <- get
  put (IOStore (currentQueries ioStore) newStore (outputLoc ioStore))

getStackTable :: TableName -> IOS (Maybe Table)
getStackTable tablename = do Map.lookup tablename . scope <$> getStack

getCurrentQueries :: IOS Queries
getCurrentQueries = do gets currentQueries

setCurrentQueries :: Queries -> IOS ()
setCurrentQueries qs = do
  ioStore <- get
  put (IOStore qs (stack ioStore) (outputLoc ioStore))

addCurrentQueries :: Queries -> IOS ()
addCurrentQueries qs' = do
  qs <- getCurrentQueries
  setCurrentQueries $ qs ++ qs'

popCurrentQueries :: IOS (Maybe Query)
popCurrentQueries = do
  qs <- getCurrentQueries
  case qs of
    [] -> return Nothing
    (q : qs') -> setCurrentQueries qs' >> return (Just q)

flushCurrentQueries :: IOS Queries
flushCurrentQueries = do
  qs <- getCurrentQueries
  setCurrentQueries []
  return qs

getOutputLoc :: IOS OutputLoc
getOutputLoc = do gets outputLoc

setOutputLoc :: OutputLoc -> IOS ()
setOutputLoc outputLoc = do
  ioStore <- get
  put (IOStore (currentQueries ioStore) (stack ioStore) outputLoc)

execProgram :: IOS a -> IO ()
execProgram action = do
  (_, IOStore _ result _) <- runStateT action (IOStore [] emptyStore Terminal)
  putStrLn $ TP.printStore result

hideContext :: IOS a -> IOS a
hideContext action = do
  store <- get
  a <- action
  put store
  return a

-- "./test/test-suite/Base/base.sql"

loadSetupFile :: IOS ()
loadSetupFile = do
  errOScripts <- lift $ SPAR.parseSQLFile (setupPath </> setupSQL -<.> sqlformat)
  filePath <- lift getCurrentDirectory
  lift $ putStrLn filePath
  case errOScripts of
    Left errMsg -> lift $ putStrLn errMsg
    Right qs -> setCurrentQueries qs

loadSetupStore :: IOS ()
loadSetupStore = do
  baseQueries <- loadSetupFile
  (IOStore qs store _) <- get
  setStack $ exec (eval qs) store

putStrLnIOS :: String -> IOS ()
putStrLnIOS = lift . putStrLn

loadSetupTable :: IOS ()
loadSetupTable = do
  mapM_ (loadTable setupPath) setupCSVs

loadSetup :: IOS ()
loadSetup = do
  loadSetupStore
  loadSetupTable

loadScript :: String -> IOS ()
loadScript scriptPath = do
  errOScript <- lift $ SPAR.parseSQLFile scriptPath
  case errOScript of
    Left errMsg -> lift $ putStrLn errMsg
    Right qs -> setCurrentQueries qs

loadTable :: String -> String -> IOS ()
loadTable tableFolder tablename = do
  store <- getStack
  case interp (getTable tablename) store of
    Left errMsg -> putStrLnIOS errMsg
    Right Nothing -> putStrLnIOS "Cannot find table"
    Right (Just (_, Table pk iName _)) ->
      do
        errOTable <- lift $ TPAR.parseCSVFile pk iName (tableFolder </> tablename -<.> csvformat)
        case errOTable of
          Left errMsgTable -> putStrLnIOS errMsgTable
          Right table ->
            case interp (setScope tablename table) store of
              Left errMsgSet -> putStrLnIOS errMsgSet
              Right newStore -> setStack newStore

runQuery :: IOS Table
runQuery = do
  stack <- getStack
  qMaybe <- popCurrentQueries
  case qMaybe of
    Nothing -> return emptyTable
    Just q -> case interp (evalQuery q) stack of
      Left errMsg -> putStrLnIOS errMsg >> return emptyTable
      Right table -> return table

runQueries :: IOS ()
runQueries = do
  stack <- getStack
  qs <- flushCurrentQueries
  setStack $ exec (eval qs) stack

setupEnv :: String -> String -> String -> IOS ()
setupEnv answerFolder answerScript answerName = do
  loadScript (answerFolder </> answerScript -<.> sqlformat)
  runQueries
  loadTable answerFolder answerName

-- >>> loadScript "./test/test-suite/test4/test4/answer.sql"

-- | answerPath should be free of suffix
runTestCase :: String -> String -> String -> String -> IOS ()
runTestCase answerFolder answerScript answerName testScript = do
  putStrLnIOS ("Running test " ++ answerFolder </> testScript)
  setupEnv answerFolder answerScript answerName
  loadScript (answerFolder </> testScript -<.> sqlformat)
  resultTable <- runQuery
  answerMaybe <- getStackTable answerName
  case answerMaybe of
    Nothing -> putStrLnIOS "Answer disappeared!!"
    Just answerTable ->
      lift $
        void
          ( runTestTT
              ( ("running Test " ++ testScript) ~:
                  Table
                    (NE.sort $ primaryKeys resultTable)
                    (List.sort $ indexName resultTable)
                    (tableData resultTable)
                    ~?= Table
                      (NE.sort $ primaryKeys answerTable)
                      (List.sort $ indexName answerTable)
                      (tableData answerTable)
              )
          )

runTestCases :: IOS ()
runTestCases = do
  loadSetup
  mapM_ (\(x1, x2, x3, x4) -> hideContext (runTestCase x1 x2 x3 x4)) testcases

-- loadTest
testcases :: [(FilePath, String, String, String)]
testcases =
  List.map
    (\(x1, x2, x3, x4) -> (testPath </> x1, x2, x3, x4))
    [ ("test1", "answer", "answer", "test1"),
      ("test2", "answer", "answer", "test2"),
      ("test3", "answer", "answer", "test3"),
      ("test4", "answer", "answer", "test4"),
      ("test5", "answer", "answer", "test5"),
      ("test6", "answer", "answer", "test6"),
      ("test7", "answer", "answer", "test7"),
      ("test8", "answer", "answer", "test8")
    ]

-------- Unit Testing
test_evalFrom :: Test
test_evalFrom =
  "evaluate From" ~:
    TestList
      [ interp (evalFrom (TableRef "Students")) sampleStore ~?= Right ("Students", tableSampleStudents),
        interp (evalFrom (TableRef "bla")) sampleStore ~?= Left "No Table for table reference: bla",
        interp (evalFrom (TableAlias "Students" "Student1")) sampleStore ~?= Right ("Student1", tableSampleStudents),
        execState (runExceptT (evalFrom (TableAlias "Students" "Student1"))) sampleStore ~?= Store (scope sampleStore) (alias sampleStore `Map.union` Map.singleton "Student1" "Students"),
        interp (evalFrom (TableAlias "bla" "Student1")) sampleStore ~?= Left "No Table for table alias: bla AS Student1"
      ]

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

test_weakCast :: Test
test_weakCast =
  TestList
    [ interp (weakCast (IntVal 0) (IntType 1)) sampleStore ~?= Right (IntVal 0),
      interp (weakCast (IntVal 8) (IntType 4)) sampleStore ~?= Right (IntVal 8),
      interp (weakCast (BoolVal True) (IntType 3)) sampleStore ~?= Right (IntVal 1),
      interp (weakCast (StringVal "bbb") (IntType 3)) sampleStore ~?= interp (throwCastError (StringVal "bbb") (IntType 3)) sampleStore
    ]

test_evaluateOp2 :: Test
test_evaluateOp2 =
  "evaluate Op2" ~:
    TestList
      [ interp (evalE (Op2 (Val NullVal) Eq (Val NullVal)) Map.empty) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Val $ IntVal 3) Eq (Val (IntVal 3))) Map.empty) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Val $ StringVal "CIS") Eq (Val $ StringVal "CI")) Map.empty) sampleStore ~?= Right (BoolVal False),
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) Map.empty) sampleStore ~?= Right NullVal,
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (StringVal "what"))) sampleStore ~?= Left "Illegal Casting into INT(64): 'what'",
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (IntVal 1))) sampleStore ~?= Right (IntVal 2),
        interp (evalE (Op2 (Var $ VarName "student_id") Plus (Val $ IntVal 1)) (Map.singleton (VarName "student_id") (BoolVal False))) sampleStore ~?= Right (IntVal 1),
        interp (evalE (Op2 (Var $ VarName "student_id") Like (Val $ StringVal "Hi")) (Map.singleton (VarName "student_id") (StringVal "Hi"))) sampleStore ~?= Right (BoolVal True),
        interp (evalE (Op2 (Var $ VarName "student_id") Like (Val $ StringVal "John[1-9]")) (Map.singleton (VarName "student_id") (StringVal "John1"))) sampleStore ~?= Right (BoolVal True)
      ]

test_evalEAgg :: Test
test_evalEAgg =
  TestList
    [ interp (evalEAgg (AggFun Sum All (Var $ VarName "var1")) []) sampleStore ~?= Right (Val $ IntVal 0),
      interp (evalEAgg (AggFun Sum All (Var $ VarName "var2")) [Map.fromList [(VarName "var1", IntVal 1)]]) sampleStore ~?= Right (Val $ IntVal 0)
    ]

test_evalOrderBy :: Test
test_evalOrderBy =
  TestList
    [ interp (evalOrderBy [(VarName "student_id", Just DESC, Nothing)] tableSampleStudents) sampleStore ~?= Right (Table (primaryKeys tableSampleStudents) (indexName tableSampleStudents) (reverse $ tableData tableSampleStudents))
    ]

test_deterPK :: Test
test_deterPK =
  TestList
    [ deterPK Nothing [ColumnName (Var $ VarName "order_id")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Just (NE.fromList [(VarName "order_id", IntType 16)]),
      deterPK Nothing [ColumnName (Var $ VarName "order_id"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Just (NE.fromList [(VarName "order_id", IntType 16)]),
      deterPK Nothing [ColumnName (Var $ VarName "name"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Nothing,
      deterPK (Just $ VarName "name") [ColumnName (Var $ VarName "name"), ColumnName (Var $ VarName "amount")] (NE.fromList [(VarName "order_id", IntType 16)]) ~?= Nothing
    ]

test_extractPosPK :: Test
test_extractPosPK =
  TestList
    [ extractPosPK (ColumnName (Var $ VarName "order_id")) (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order_id", IntType 16))],
      extractPosPK (ColumnAlias (Var $ VarName "order_id") "order") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order", IntType 16))],
      extractPosPK AllVar (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Map.fromList [(VarName "order_id", (VarName "order_id", IntType 16)), (VarName "amount", (VarName "amount", IntType 32))]
    ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_evalFrom,
        test_joinMid,
        test_weakCast,
        test_evaluateOp2,
        test_evalEAgg,
        test_evalOrderBy,
        test_deterPK,
        test_extractPosPK
      ]