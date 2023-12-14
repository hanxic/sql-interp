module CommandLine where

import Control.Monad.State
import Data.List qualified as List
import Interpretation
import SQLParser
import SQLPrinter
import SQLSyntax
import System.Directory (doesPathExist)
import System.IO
import TableParser
import TablePrinter
import TableSyntax
import Text.Read (readMaybe)

-- | A data structure that will refer to the output location
data OutputLoc
  = File FilePath
  | Terminal
  deriving (Eq, Show)

-- | The state of the main IO
data CLMemory = CLMemory
  { stack :: Stack,
    outputLoc :: OutputLoc,
    queriesQueue :: Queries,
    history :: Maybe CLMemory
  }
  deriving (Eq, Show)

type CLIO a = StateT CLMemory IO a

type Stack = Store

getStack :: CLIO Stack
getStack = do gets stack

setStack :: Stack -> CLIO ()
setStack stack = do
  clmemory <- get
  put $ CLMemory stack (outputLoc clmemory) (queriesQueue clmemory) (history clmemory)

getOutputLoc :: CLIO OutputLoc
getOutputLoc = do gets outputLoc

setOutputLoc :: OutputLoc -> CLIO ()
setOutputLoc outputloc = do
  clmemory <- get
  put $ CLMemory (stack clmemory) outputloc (queriesQueue clmemory) (history clmemory)

getQueriesQueue :: CLIO Queries
getQueriesQueue = do gets queriesQueue

setQueriesQueue :: Queries -> CLIO ()
setQueriesQueue qs = do
  clmemory <- get
  put $ CLMemory (stack clmemory) (outputLoc clmemory) qs (history clmemory)

appQueriesQueue :: Queries -> CLIO ()
appQueriesQueue qs = do
  queries <- getQueriesQueue
  setQueriesQueue $ queries ++ qs

getHistory :: CLIO (Maybe CLMemory)
getHistory = do gets history

setHistory :: Maybe CLMemory -> CLIO ()
setHistory hist = do
  clmemory <- get
  put $ CLMemory (stack clmemory) (outputLoc clmemory) (queriesQueue clmemory) hist

initialCLMemory :: CLMemory
initialCLMemory =
  CLMemory
    { stack = emptyStore,
      outputLoc = Terminal,
      queriesQueue = [],
      history = Nothing
    }

execCL :: CLIO a -> IO ()
execCL action = do
  (a, clmemory) <- runStateT action initialCLMemory
  print clmemory

putStrCLIO :: String -> CLIO ()
putStrCLIO str = lift $ putStr str

putStrLnCLIO :: String -> CLIO ()
putStrLnCLIO str = lift $ putStrLn str

loadScript :: String -> CLIO ()
loadScript scriptPath = do
  errOScript <- lift $ SQLParser.parseSQLFile scriptPath
  case errOScript of
    Left errMsg -> lift $ putStrLn errMsg
    Right qs -> appQueriesQueue qs

commandline :: IO ()
commandline = evalStateT loop initialCLMemory

loop :: CLIO ()
loop = do
  prompt
  putStrCLIO "SQL-Interp>"
  str <- lift getLine
  case List.uncons (words str) of
    Just (":load", [fn]) -> do
      errOQueries <- lift $ SQLParser.parseSQLFile fn
      case errOQueries of
        Left errMsg -> do
          putStrLnCLIO errMsg
          loop
        Right qs -> do
          clmemory <- get
          setHistory $ Just clmemory
          appQueriesQueue qs
          loop
    Just (":upload", [fn, tn]) -> do
      stack <- getStack
      case interp (getTable tn) stack of
        Left errMsg -> putStrLnCLIO errMsg >> loop
        Right Nothing -> putStrLnCLIO "Error: Cannot find table" >> loop
        Right (Just (_, Table pk index _)) ->
          do
            errOTable <- lift $ TableParser.parseCSVFile pk index fn
            case errOTable of
              Left errMsg' -> putStrLnCLIO errMsg' >> loop
              Right table ->
                case interp (setScope tn table) stack of
                  Left errMsgSet -> putStrLnCLIO errMsgSet >> loop
                  Right newStack -> do
                    clmemory <- get
                    setHistory $ Just clmemory
                    setStack newStack >> loop
    Just (":quit", _) -> do
      return ()
    Just (":queries", _) -> do
      queriesQueue <- getQueriesQueue
      putStrLnCLIO "Here are the unfinished queues:"
      mapM_ (putStrLnCLIO . SQLPrinter.pretty) queriesQueue
      loop
    Just (":run", strs) -> do
      let numSteps :: Int
          numSteps = case readMaybe (concat strs) of
            Just x -> x
            Nothing -> 1
      multiStepper numSteps
      loop
      where
        multiStepper :: Int -> CLIO ()
        multiStepper n = do
          clmemory <- get
          if n <= 0 || null (queriesQueue clmemory)
            then return ()
            else do
              case queriesQueue clmemory of
                [] -> putStrLnCLIO "Error: Nothing to run"
                (q : qs) -> do
                  setQueriesQueue qs
                  let store = stack clmemory
                   in let (output, store') = run (evalQuery q) store
                       in case output of
                            Left errMsg -> putStrLnCLIO errMsg >> putStrLnCLIO (TablePrinter.pretty store')
                            Right table -> do
                              setHistory $ Just clmemory
                              setStack store'
                              putStrLnCLIO (TablePrinter.pretty table)
                              multiStepper (n - 1)
    Just (":back", strs) -> do
      clmemory <- get
      let numSteps :: Int
          numSteps = case readMaybe (concat strs) of
            Just x -> x
            Nothing -> 1
      multiRetriever numSteps clmemory
      loop
      where
        multiRetriever :: Int -> CLMemory -> CLIO ()
        multiRetriever n clmemory =
          if n <= 0
            then put clmemory
            else case history clmemory of
              Nothing -> put clmemory
              Just clmemory' -> multiRetriever (n - 1) clmemory'
    Just (":cstack", _) -> do
      clmemory <- get
      setHistory $ Just clmemory
      setStack emptyStore
      putStrLnCLIO "Stack is clear"
      loop
    Just (":stack", _) -> do
      stack <- getStack
      putStrLnCLIO (SQLPrinter.pretty stack)
      loop
    Just (":redirect", [fn]) -> do
      pathExists <- lift $ doesPathExist fn
      if pathExists
        then setOutputLoc (File fn) >> putStrLnCLIO ("Redirect successful to " ++ fn) >> loop
        else setOutputLoc Terminal >> putStrLnCLIO "Redirect to Terminal" >> loop
    Just (":output", [tn]) -> do
      stack <- getStack
      case interp (getTable tn) stack of
        Left errMsg -> putStrLnCLIO errMsg >> loop
        Right Nothing -> putStrLnCLIO "Error: Cannot find table" >> loop
        Right (Just (_, table)) -> do
          outputloc <- getOutputLoc
          case outputloc of
            Terminal -> putStrLnCLIO (TablePrinter.pretty table) >> loop
            File fn -> lift (appendFile fn (TablePrinter.pretty table)) >> loop
    _ ->
      case SQLParser.parseSQL str of
        Right queries -> do
          stack <- getStack
          let v = interp (eval queries) stack
           in loop
        Left _s -> do
          putStrCLIO $ "Command Line Error: " ++ _s
          loop

fetchTable :: TableName -> ((TableName, Table) -> CLIO ()) -> CLIO () -> CLIO ()
fetchTable tn clio loop = do
  stack <- getStack
  case interp (getTable tn) stack of
    Left errMsg -> putStrLnCLIO errMsg >> loop
    Right Nothing -> putStrLnCLIO "Error: Cannot find table" >> loop
    Right (Just result) -> clio result

prompt :: CLIO ()
prompt = do
  queriesQueue <- getQueriesQueue
  case queriesQueue of
    [] -> return ()
    q : _ -> putStrLnCLIO $ "Current Queries: " ++ SQLPrinter.pretty q
