import Interpretation qualified as INTERP
import SQLPrinter as SP
import SQLPrinter qualified as SP
import TablePrinter qualified as TP
import Test.HUnit
import Test.QuickCheck
import TestInterpretation

main :: IO ()
main = do
  putStrLn "SQL-Interp Test suites "
  testProgram

testProgram :: IO ()
testProgram =
  execProgram runTestCases

{- noneOSetupStore <- loadSetup
case noneOSetupStore of
  Nothing -> return ()
  Just setupStore -> putStrLn $ TP.printStore setupStore -}
