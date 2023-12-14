module Optimization where

import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Maybe
import GenVSQL
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
    { exprsSelect = (All, []),
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
    { exprsSelect = (All, [AllVar]),
      fromSelect = f,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

singletonSelect :: ColumnExpression -> SelectCommand
singletonSelect ce = insertSelect ce emptySelect

insertSelect :: ColumnExpression -> SelectCommand -> SelectCommand
insertSelect ce sc =
  let newColExpr = ce : snd (exprsSelect sc)
   in sc
        { exprsSelect = (fst (exprsSelect sc), newColExpr)
        }

fromList :: [ColumnExpression] -> SelectCommand
fromList = List.foldr insertSelect emptySelect

getColExprs :: SelectCommand -> [ColumnExpression]
getColExprs = snd . exprsSelect

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
hasJoin = hasJoinFrom . fromSelect

hasJoinFrom :: FromExpression -> Bool
hasJoinFrom Join {} = True
hasJoinFrom _ = False

-- Build map of Selected columns

mapTableVars :: Scope -> Map TableName IndexName
mapTableVars = Map.map indexName

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
checkFromId f = f

optimizeWhereJoin :: SelectCommand -> SelectCommand
optimizeWhereJoin sc
  | hasJoin sc =
      sc
        { fromSelect = unpackPushWhere (whSelect sc) (fromSelect sc)
        }
optimizeWhereJoin sc = sc

unpackPushWhere :: Maybe Expression -> FromExpression -> FromExpression
unpackPushWhere Nothing f = f
unpackPushWhere (Just e) f = pushDownWhere (mapVarUsesExpr e) e f

pushDownWhere :: [Var] -> Expression -> FromExpression -> FromExpression
pushDownWhere [] _ f = f
pushDownWhere vs e (TableRef n)
  | varsMatchName n vs =
      SubQuery $
        (idSelect (TableRef n))
          { whSelect = Just e
          }
pushDownWhere vs e (TableAlias n a)
  | varsMatchName a vs =
      SubQuery $
        (idSelect (TableAlias n a))
          { whSelect = Just e
          }
pushDownWhere vs e (SubQuery sc) = pushDownWhere vs e (fromSelect sc)
pushDownWhere vs e (Join f1 s f2 n) = Join (pushDownWhere vs e f1) s (pushDownWhere vs e f2) n
pushDownWhere _ _ f = f

varsMatchName :: Name -> [Var] -> Bool
varsMatchName n = all (varMatchName n)

varMatchName :: Name -> Var -> Bool
varMatchName n (Dot t v) = n == t
varMatchName _ _ = False

-- Main optimization

runOptimization :: (SelectCommand -> SelectCommand) -> SelectCommand -> SelectCommand
runOptimization opt sc = opt $ sc {fromSelect = runFromExprOpt opt (fromSelect sc)}

runFromExprOpt :: (SelectCommand -> SelectCommand) -> FromExpression -> FromExpression
runFromExprOpt opt (SubQuery sc) = SubQuery $ runOptimization opt sc
runFromExprOpt _ f = f

optimizationList :: [SelectCommand -> SelectCommand]
optimizationList = [optimizeFromId, optimizeWhereJoin]

optimizationMain :: SelectCommand -> SelectCommand
optimizationMain = List.foldl (.) id optimizationList