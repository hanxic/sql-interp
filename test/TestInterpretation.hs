module TestInterpretation where

import Control.Monad (foldM)
import Control.Monad.State
import Interpretation
import Parser qualified as P
import SQLParser qualified as SPAR
import SQLSyntax
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath
import TableParser qualified as SPAR
import TableSyntax

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

type IOSQLI a = StateT Store IO a

testPath = "./test/test-suite/"

setupPath = testPath ++ "setup/"

setupSQL = "setup.sql"

setupCSVs = ["Customers", "Orders", "Shippings"]

csvformat = ".csv"

-- "./test/test-suite/Base/base.sql"

loadSetupFile' :: IOSQLI (Maybe Queries)
loadSetupFile' = do
  errOScripts <- lift $ SPAR.parseSQLFile (setupPath ++ setupSQL)
  filePath <- lift getCurrentDirectory
  lift $ putStrLn filePath
  case errOScripts of
    Left errMsg -> lift $ putStrLn errMsg >> return Nothing
    Right qs -> return $ Just qs

loadSetupFile :: IO (Maybe Queries)
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
    Just store -> Just <$> loadSetupTable store

{-     go :: Store -> String -> IO Store
go store tablename =
  alterStoreIO (getTable tablename) store (
    \(_, Table pk iName _) ->
  )-}

alterStoreIO :: SQLI (Maybe a) -> Store -> (a -> IO Store) -> IO Store
alterStoreIO sqli store f =
  let errOSucc = interp sqli store
   in case errOSucc of
        Left errMsg -> putStrLn errMsg >> return store
        Right Nothing -> putStrLn "Store alter unsuccessful" >> return store
        Right (Just a) -> f a

-- loadTest
testcases = ["test1"]