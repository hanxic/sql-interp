module TestInterpretation where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import Data.List qualified as List (sort)
import Data.List.NonEmpty qualified as NE (fromList, sort)
import Data.Map qualified as Map (lookup)
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

{- Plan of attack
1. Create a folder with all the test cases
2. For each test cases, fetch the SQL scripts
  2.1 For each test cases, also fetch the resulting table
  2.2 Return the two being the same
-}

{- readStringFromFolder :: FilePath -> FilePath -> IO (Maybe String)
readStringFromFolder folder fileName = do
  let filePath = folder </> fileName
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- readFile filePath
      return (Just content)
    else return Nothing -}

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

testedit = "  "

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

{- loadTestCases :: IOS ()
loadTestCases = undefined -}

runTestCases :: IOS ()
runTestCases = do
  loadSetup
  mapM_ (\(x1, x2, x3, x4) -> hideContext (runTestCase x1 x2 x3 x4)) testcases

{- setupEnv (testPath </> "test1") "answer" "answer" -}

{- test103 = SPAR.parseSQLFile (testPath </> "test3" </> "test3" -<.> sqlformat)

test104 = SPAR.parseSQLFile (testPath </> "test4" </> "answer" -<.> sqlformat)

-- >>> test104
-- Right [CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "answer", idCreate = [("\"COUNT(order_id)\"",IntType 16,True)]})]

test109 = [CreateQuery (CreateCommand {ifNotExists = False, nameCreate = "answer", idCreate = [("\"COUNT(order_id)\"", IntType 16, True)]})]

test108 = exec (eval test10) emptyStore

-- >>> test108
-- Store {scope = fromList [("answer",Table {primaryKeys = (VarName "COUNT(order_id)",IntType 16) :| [], indexName = [], tableData = []})], alias = fromList []}

test107 = TPAR.parseCSVFile (NE.fromList [(VarName "order_id", IntType 16)]) [(VarName "amount + 5", IntType 16), (VarName "customer_id", IntType 16)] (testPath </> "test5" </> "answer" -<.> sqlformat)

-- >>> test107
-- Left "No parses"
 -}
-- loadTest
testcases :: [(FilePath, String, String, String)]
testcases =
  map
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

{- loadSetupFile :: IO (Maybe Queries)
loadSetupFile = do
  errOScripts <- SPAR.parseSQLFile (setupPath ++ setupSQL)
  filePath <- getCurrentDirectory
  putStrLn filePath
  case errOScripts of
    Left errMsg -> putStrLn errMsg >> return Nothing
    Right qs -> return $ Just qs

loadSetupStore :: Store -> IO (Maybe Store)
loadSetupStore store = do
  baseQueries <- loadSetupFile
  case baseQueries of
    Nothing -> return Nothing
    Just qs -> return $ Just $ exec (eval qs) store

loadSetupTable :: Store -> IO Store
loadSetupTable store = do
  foldM go store setupCSVs
  where
    go :: Store -> String -> IO Store
    go store tablename =
      case interp (getTable tablename) store of
        Left errMsg -> putStrLn errMsg >> return store
        Right Nothing -> putStrLn "Cannot find table" >> return store
        Right (Just (_, Table pk iName _)) ->
          do
            errOTable <- SPAR.parseCSVFile pk iName (setupPath ++ tablename ++ csvformat)
            case errOTable of
              Left errMsgTable -> putStrLn errMsgTable >> return store
              Right table ->
                case interp (setScope tablename table) store of
                  Left errMsgSet -> putStrLn errMsgSet >> return store
                  Right newStore -> return newStore

loadSetup :: IO (Maybe Store)
loadSetup = do
  baseStore <- loadSetupStore emptyStore
  case baseStore of
    Nothing -> return Nothing
    Just store -> Just <$> loadSetupTable store -}

{-     go :: Store -> String -> IO Store
go store tablename =
  alterStoreIO (getTable tablename) store (
    \(_, Table pk iName _) ->
  )-}
