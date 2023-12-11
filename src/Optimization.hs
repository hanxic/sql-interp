module Optimization where

import Data.List as List
import Data.Map as Map
import Interpretation
import SQLSyntax
import TableSyntax
import Test.QuickCheck as QC

-- Helper identities and functions

emptyTableRef :: FromExpression
emptyTableRef = TableRef "emptyTableRef"

emptySelect :: SelectCommand
emptySelect =
  SelectCommand
    { exprsSelect = [],
      fromSelect = emptyTableRef,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

idSelect :: FromExpression -> SelectCommand
idSelect f =
  SelectCommand
    { exprsSelect = [(All, AllVar)],
      fromSelect = f,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

singletonSelect :: CountStyle -> ColumnExpression -> SelectCommand
singletonSelect st ce = insertSelect st ce emptySelect

insertSelect :: CountStyle -> ColumnExpression -> SelectCommand -> SelectCommand
insertSelect st ce sc =
  sc
    { exprsSelect = (st, ce) : exprsSelect sc
    }

fromList :: [ColumnExpression] -> SelectCommand
fromList = List.foldr (insertSelect All) emptySelect

getColExprs :: SelectCommand -> [ColumnExpression]
getColExprs = List.map snd . exprsSelect

-- Command Checkers

isBottomSelect :: SelectCommand -> Bool
isBottomSelect = isBottomFrom . fromSelect

isBottomFrom :: FromExpression -> Bool
isBottomFrom (TableRef _) = True
isBottomFrom (TableAlias _ _) = True
isBottomFrom _ = False

hasAllVar :: SelectCommand -> Bool
hasAllVar = any hasAllVarCol . getColExprs

hasAllVarCol :: ColumnExpression -> Bool
hasAllVarCol AllVar = True
hasAllVarCol _ = False

hasJoin :: SelectCommand -> Bool
hasJoin sc = hasJoinFrom (fromSelect sc)

hasJoinFrom :: FromExpression -> Bool
hasJoinFrom Join {} = True
hasJoinFrom _ = False

-- Build map of Selected columns

mapVarUses :: SelectCommand -> [Var]
mapVarUses sc = List.concatMap mapVarUsesCol (getColExprs sc)

mapVarUsesCol :: ColumnExpression -> [Var]
mapVarUsesCol (ColumnName e) = mapVarUsesExpr e
mapVarUsesCol (ColumnAlias e _) = mapVarUsesExpr e
mapVarUsesCol AllVar = []

mapVarUsesExpr :: Expression -> [Var]
mapVarUsesExpr (Var v) = [v]
mapVarUsesExpr (Op1 _ e) = mapVarUsesExpr e
mapVarUsesExpr (Op2 e1 _ e2) = mapVarUsesExpr e1 ++ mapVarUsesExpr e2
mapVarUsesExpr (AggFun _ _ e) = mapVarUsesExpr e
mapVarUsesExpr (SQLSyntax.Fun _ e) = mapVarUsesExpr e
mapVarUsesExpr _ = []

-- Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/

optimizeWhereJoin :: SelectCommand -> SelectCommand
optimizeWhereJoin = undefined

optimizeMultiJoin :: SelectCommand -> SelectCommand
optimizeMultiJoin = undefined

prop_optimizeWhereJoin :: SelectCommand -> Bool
prop_optimizeWhereJoin sc = undefined

-- prop_optimizeWhereJoin sc = evalSelect sc == (evalSelect . optimizeWhereJoin) sc

prop_optimizeMultiJoin :: SelectCommand -> Bool
prop_optimizeMultiJoin sc = undefined

-- prop_optimizeMultiJoin sc = evalSelect sc == (evalSelect . optimizeMultiJoin) sc