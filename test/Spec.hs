import Interpretation qualified as INTERP
import SQLPrinter as SP
import SQLPrinter qualified as SP
import TablePrinter qualified as TP
import Test.HUnit
import Test.QuickCheck
import TestInterpretation
import TestOptimization
import TestSQL
import TestTable

main :: IO ()
main = do
  putStrLn "Unit Testing for SQL Parser / Printer"
  TestSQL.test_all
  putStrLn "Property-Based Testing for SQL Parser / Printer"
  TestSQL.qc
  putStrLn "Unit Testing for Table Parser / Printer"
  TestTable.test_all
  putStrLn "Property-Based Testing for Table Parser / Printer"
  TestTable.qc
  putStrLn "Unit Testing for Interpretation"
  TestInterpretation.test_all
  putStrLn "Property-Based Testing for Interpretation"
  TestInterpretation.qc
  putStrLn "Unit Testing for Optimization"
  TestOptimization.test_all
  putStrLn "Property-Based Testing for Optimization"
  TestOptimization.qc
  putStrLn "SQL-Interp Test suites "
  testProgram

testProgram :: IO ()
testProgram =
  execProgram runTestCases

{- noneOSetupStore <- loadSetup
case noneOSetupStore of
  Nothing -> return ()
  Just setupStore -> putStrLn $ TP.printStore setupStore -}
