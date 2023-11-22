module Optimization where

import Interpretation
import SQLSyntax
import Test.QuickCheck as QC
import Test.QuickCheck qualified as QC

data AST
  = Node AST Command AST
  | Empty

-- deriving (Eq, Ord, Show)

-- Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/

emptyAST :: AST
emptyAST = Empty

singleton :: Command -> AST
singleton c = insert c emptyAST

insert :: Command -> AST -> AST
insert c t = undefined

fromList :: [Command] -> AST
fromList = foldr insert emptyAST

optimizeSelectAll :: TableMap -> TableMap
optimizeSelectAll = undefined

optimizeCascSelect :: TableMap -> TableMap
optimizeCascSelect = undefined

optimizeCommSelect :: TableMap -> TableMap
optimizeCommSelect = undefined

optimizeCascProjection :: TableMap -> TableMap
optimizeCascProjection = undefined

combineSelectJoin :: TableMap -> TableMap
combineSelectJoin = undefined

instance Arbitrary AST where
  arbitrary :: Gen AST
  arbitrary = QC.listOf (arbitrary :: Gen Command) >>= \xs -> return (fromList xs)

  shrink :: AST -> [AST]
  shrink (Node l _ r) = [l, r]
  shrink _ = []

prop_valid :: TableMap -> Bool
prop_valid = undefined

prop_optimizeSelectAll :: TableMap -> Bool
prop_optimizeSelectAll t = optimizeSelectAll t == t

prop_optimizeCascSelect :: TableMap -> Bool
prop_optimizeCascSelect t = optimizeSelectAll t == t

prop_optimizeCommSelect :: TableMap -> Bool
prop_optimizeCommSelect t = optimizeSelectAll t == t

prop_optimizeCascProjection :: TableMap -> Bool
prop_optimizeCascProjection t = optimizeSelectAll t == t

prop_combineSelectJoin :: TableMap -> Bool
prop_combineSelectJoin t = optimizeSelectAll t == t

prop_runtime :: TableMap -> Bool
prop_runtime t = undefined
-- prop_runtime t = evalRunTime (optimize t) <= (evalRunTime t) + 0.0001
