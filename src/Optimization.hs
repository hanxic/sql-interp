module Optimization where

import Interpretation
import SQLSyntax
import TableSyntax
import Test.QuickCheck as QC

data AST
  = Node AST Query AST
  | Empty
  deriving (Eq, Show)

-- Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/

emptyAST :: AST
emptyAST = Empty

singleton :: Query -> AST
singleton c = insert c emptyAST

insert :: Query -> AST -> AST
insert c t = undefined

fromList :: [Query] -> AST
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

instance Arbitrary Query where
  arbitrary :: Gen Query
  arbitrary = undefined

  shrink :: Query -> [Query]
  shrink = undefined

instance Eq Query where
  (==) :: Query -> Query -> Bool
  _ == _ = undefined

instance Show Query where
  show :: Query -> String
  show = undefined