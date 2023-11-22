module TestParser where

import Parser
import Test.HUnit
import Test.QuickCheck

prop_roundtrip_val :: DValue -> Bool
prop_roundtrip_val v = P.parse valueP (SPP.pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (SPP.pretty e) == Right e

main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"
