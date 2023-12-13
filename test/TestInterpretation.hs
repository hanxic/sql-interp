module TestInterpretation where

import Interpretation
import Parser qualified as P
import SQLParser qualified as SPAR
import SQLSyntax
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath
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

basePath = "./test/test-suite/Base/base.sql"

loadBaseFile :: IO (Maybe Queries)
loadBaseFile = do
  errOBlock <- SPAR.parseSQLFile basePath
  filePath <- getCurrentDirectory
  putStrLn filePath
  case errOBlock of
    Left errMsg -> putStrLn errMsg >> return Nothing
    Right qs -> return $ Just qs

loadBaseStore :: Store -> IO (Maybe Store)
loadBaseStore store = do
  baseQueries <- loadBaseFile
  case baseQueries of
    Nothing -> return Nothing
    Just qs -> return $ Just $ exec (eval qs) store