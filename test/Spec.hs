import Interpretation qualified as INTERP
import SQLPrinter as SP
import SQLPrinter qualified as SP
import TablePrinter qualified as TP
import Test.HUnit
import Test.QuickCheck
import TestInterpretation

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  baseQueries <- loadBaseFile
  case baseQueries of
    Nothing -> return ()
    Just qs -> putStrLn $ SP.printQueries qs
  baseStore <- loadBaseStore INTERP.emptyStore
  case baseStore of
    Nothing -> return ()
    Just store -> putStrLn $ TP.printTable store
