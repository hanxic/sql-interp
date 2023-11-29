module Optimization where

import Interpretation
import SQLSyntax
import Test.QuickCheck as QC
import Test.QuickCheck qualified as QC

data AST
  = Node AST Command AST
  | Empty
  deriving (Eq, Show)

-- Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/

emptyAST :: AST
emptyAST = Empty

singleton :: Command -> AST
singleton c = insert c emptyAST

insert :: Command -> AST -> AST
insert c t = undefined

fromList :: [Command] -> AST
fromList = foldr insert emptyAST

evalAST :: AST -> TableMap
evalAST t = undefined

optimizeSelectAll :: AST -> AST
optimizeSelectAll = undefined

optimizeCascSelect :: AST -> AST
optimizeCascSelect = undefined

optimizeCommSelect :: AST -> AST
optimizeCommSelect = undefined

optimizeCascProjection :: AST -> AST
optimizeCascProjection = undefined

combineSelectJoin :: AST -> AST
combineSelectJoin = undefined

instance Arbitrary AST where
  arbitrary :: Gen AST
  arbitrary = QC.listOf (arbitrary :: Gen Command) >>= \xs -> return (fromList xs)

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
