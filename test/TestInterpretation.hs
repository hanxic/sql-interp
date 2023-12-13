module TestInterpretation where

import Interpretation
import Parser qualified as P
import SQLSyntax
import System.Directory (doesFileExist)
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

loadBaseFile :: IO (Maybe Store)
loadBaseFile = do
  errOBlock <- 
