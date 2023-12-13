module Optimization where

import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Interpretation
import SQLSyntax
import TableSyntax
import Test.HUnit
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

hasWhere :: SelectCommand -> Bool
hasWhere = isJust . whSelect

hasJoin :: SelectCommand -> Bool
hasJoin = hasJoinFrom . fromSelect

hasJoinFrom :: FromExpression -> Bool
hasJoinFrom Join {} = True
hasJoinFrom (SubQuery sc) = hasJoin sc
hasJoinFrom _ = False

joinRefs :: SelectCommand -> [TableName]
joinRefs = joinRefsFrom . fromSelect

joinRefsFrom :: FromExpression -> [TableName]
joinRefsFrom (Join f1 _ f2 _) = joinRefsInner f1 ++ joinRefsInner f2
joinRefsFrom (SubQuery sc) = joinRefs sc
joinRefsFrom _ = []

joinRefsInner :: FromExpression -> [TableName]
joinRefsInner (TableRef n) = [n]
joinRefsInner (TableAlias n _) = [n]
joinRefsInner (Join f1 _ f2 _) = joinRefsInner f1 ++ joinRefsInner f2
joinRefsInner (SubQuery sc) = []

-- Build map of Selected columns

mapTableVars :: Scope -> Map TableName Var
mapTableVars sc = undefined

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

optimizeFromId :: SelectCommand -> SelectCommand
optimizeFromId sc = sc {fromSelect = checkFromId (fromSelect sc)}

checkFromId :: FromExpression -> FromExpression
checkFromId (SubQuery sc) | sc == idSelect (fromSelect sc) = fromSelect sc
checkFromId (SubQuery sc) = checkFromId (fromSelect sc)
checkFromId f = f

optimizeWhereJoin :: SelectCommand -> SelectCommand
optimizeWhereJoin sc
  | hasJoin sc && hasWhere sc =
      sc
        { whSelect = Nothing
        }
optimizeWhereJoin sc = sc

optimizeMultiJoin :: SelectCommand -> SelectCommand
optimizeMultiJoin sc =
  let joinRefsList = joinRefs sc
   in if length joinRefsList > 2
        then sc
        else sc

-- Main optimization

runOptimization :: SelectCommand -> SelectCommand
runOptimization = optimizeMultiJoin . optimizeWhereJoin . optimizeFromId

-- Property-based Testing

checkEval :: SelectCommand -> SelectCommand -> Bool
s1 `checkEval` s2 = interp (evalSelect s1) sampleStore == interp (evalSelect s2) sampleStore

prop_optimizeFromId :: SelectCommand -> Bool
prop_optimizeFromId sc = optimizeFromId sc `checkEval` sc

prop_optimizeWhereJoin :: SelectCommand -> Bool
prop_optimizeWhereJoin sc = optimizeWhereJoin sc `checkEval` sc

prop_optimizeMultiJoin :: SelectCommand -> Bool
prop_optimizeMultiJoin sc = optimizeMultiJoin sc `checkEval` sc

-- Unit-tests

sampleQuery :: SelectCommand
sampleQuery =
  SelectCommand
    { exprsSelect = [(All, AllVar)],
      fromSelect = TableRef "Students",
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleQueryUnopt :: SelectCommand
sampleQueryUnopt =
  SelectCommand
    { exprsSelect = [(All, AllVar)],
      fromSelect = SubQuery sampleQuery,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

test_optimizeFromId :: Test
test_optimizeFromId =
  "optimize From Id"
    ~: TestList
      [ interp (evalSelect sampleQueryUnopt) sampleStore ~?= interp (evalSelect $ optimizeFromId sampleQueryUnopt) sampleStore,
        interp (evalSelect sampleQueryUnopt) sampleStore ~?= interp (evalSelect $ optimizeFromId sampleQueryUnopt) sampleStore
      ]
