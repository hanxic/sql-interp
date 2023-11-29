module TestOptimization where

import Optimization
import Parser

instance Arbitrary AST where
  arbitrary :: Gen AST
  arbitrary = QC.listOf (arbitrary :: Gen Query) >>= \xs -> return (fromList xs)

  shrink :: AST -> [AST]
  shrink (Node l _ r) = [l, r]
  shrink _ = []

prop_valid :: TableMap -> Bool
prop_valid = undefined

prop_optimizeSelectAll :: AST -> Bool
prop_optimizeSelectAll t = optimizeSelectAll t == t

prop_optimizeCascSelect :: AST -> Bool
prop_optimizeCascSelect t = optimizeSelectAll t == t

prop_optimizeCommSelect :: AST -> Bool
prop_optimizeCommSelect t = optimizeSelectAll t == t

prop_optimizeCascProjection :: AST -> Bool
prop_optimizeCascProjection t = optimizeSelectAll t == t

prop_combineSelectJoin :: AST -> Bool
prop_combineSelectJoin t = optimizeSelectAll t == t

prop_runtime :: AST -> Bool
prop_runtime t = undefined

-- prop_runtime t = evalRunTime (optimize t) <= (evalRunTime t) + 0.0001

-- Notes:
--   - count the number of operations as a proxy for runtime
--   -
