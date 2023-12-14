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
        { whSelect = Nothing,
          fromSelect = unpackPushWhere (whSelect sc) (fromSelect sc)
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

-- Property-based Testing

storeId :: Scope -> Store
storeId sc =
  Store
    { scope = sc,
      alias = Map.empty
    }

interpEquality :: (SelectCommand -> SelectCommand) -> QC.Property
interpEquality opt = QC.forAll genSelect $ \(scope, query) ->
  interp (evalSelectCommand query) (storeId scope) == interp (evalSelectCommand $ opt query) (storeId scope)

prop_optimizeFromId :: QC.Property
prop_optimizeFromId = interpEquality optimizeFromId

prop_optimizeWhereJoin :: QC.Property
prop_optimizeWhereJoin = interpEquality optimizeWhereJoin

-- Unit-tests

checkSampleEval :: SelectCommand -> SelectCommand -> Bool
s1 `checkSampleEval` s2 = interp (evalSelectCommand s1) sampleStore == interp (evalSelectCommand s2) sampleStore

sampleQuery :: SelectCommand
sampleQuery =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "last_name"
          ]
        ),
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
    { exprsSelect = (All, [AllVar]),
      fromSelect = SubQuery sampleQuery,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleQuerySuperUnopt :: SelectCommand
sampleQuerySuperUnopt =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "last_name"
          ]
        ),
      fromSelect = SubQuery sampleQueryUnopt,
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleNoOpt :: SelectCommand
sampleNoOpt =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "last_name"
          ]
        ),
      fromSelect = TableRef "Students",
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
      [ interp (evalSelectCommand sampleQueryUnopt) sampleStore ~?= interp (evalSelectCommand $ optimizeFromId sampleQueryUnopt) sampleStore,
        interp (evalSelectCommand sampleQuerySuperUnopt) sampleStore ~?= interp (evalSelectCommand $ optimizeFromId sampleQuerySuperUnopt) sampleStore,
        interp (evalSelectCommand sampleNoOpt) sampleStore ~?= interp (evalSelectCommand $ optimizeFromId sampleNoOpt) sampleStore
      ]

sampleJoinUnOpt1 :: SelectCommand
sampleJoinUnOpt1 =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "grade"
          ]
        ),
      fromSelect =
        Join
          (TableRef "Students")
          InnerJoin
          (TableRef "Grades")
          [ ( Dot "Students" (VarName "student_id"),
              Dot "Grades" (VarName "student_id")
            )
          ],
      whSelect = Just $ Op2 (Var $ VarName "grade") Ge (Val $ IntVal 90),
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleJoinUnOpt2 :: SelectCommand
sampleJoinUnOpt2 =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "grade"
          ]
        ),
      fromSelect =
        Join
          (TableRef "Students")
          InnerJoin
          (TableRef "Grades")
          [ ( Dot "Students" (VarName "student_id"),
              Dot "Grades" (VarName "student_id")
            )
          ],
      whSelect = Just $ Op2 (Var $ VarName "gender") Is (Val $ StringVal "Female"),
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleJoinNoOpt1 :: SelectCommand
sampleJoinNoOpt1 =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "grade"
          ]
        ),
      fromSelect =
        Join
          (TableRef "Students")
          InnerJoin
          (TableRef "Grades")
          [ ( Dot "Students" (VarName "student_id"),
              Dot "Grades" (VarName "student_id")
            )
          ],
      whSelect = Nothing,
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

sampleJoinNoOpt2 :: SelectCommand
sampleJoinNoOpt2 =
  SelectCommand
    { exprsSelect =
        ( All,
          [ ColumnName $ Var $ VarName "first_name",
            ColumnName $ Var $ VarName "grade"
          ]
        ),
      fromSelect =
        Join
          (TableRef "Students")
          InnerJoin
          (TableRef "Grades")
          [ ( Dot "Students" (VarName "student_id"),
              Dot "Grades" (VarName "student_id")
            )
          ],
      whSelect =
        Just $
          Op2
            (Op2 (Var $ Dot "Grades" (VarName "grade")) Ge (Val $ IntVal 90))
            And
            (Op2 (Var $ Dot "Students" (VarName "gender")) Is (Val $ StringVal "Female")),
      groupbySelect = [],
      orderbySelect = [],
      limitSelect = Nothing,
      offsetSelect = Nothing
    }

test_optimizeWhereJoin :: Test
test_optimizeWhereJoin =
  "optimize Where Join"
    ~: TestList
      [ interp (evalSelectCommand sampleJoinUnOpt1) sampleStore ~?= interp (evalSelectCommand $ optimizeWhereJoin sampleJoinUnOpt1) sampleStore,
        interp (evalSelectCommand sampleJoinUnOpt2) sampleStore ~?= interp (evalSelectCommand $ optimizeWhereJoin sampleJoinUnOpt2) sampleStore,
        interp (evalSelectCommand sampleJoinNoOpt1) sampleStore ~?= interp (evalSelectCommand $ optimizeWhereJoin sampleJoinNoOpt1) sampleStore,
        interp (evalSelectCommand sampleJoinNoOpt2) sampleStore ~?= interp (evalSelectCommand $ optimizeWhereJoin sampleJoinNoOpt2) sampleStore
      ]

-- >>> interp (evalSelectCommand sampleQueryUnopt) sampleStore

-- >>> show $ interp (evalSelectCommand $ optimizeWhereJoin sampleQueryUnopt) sampleStore

test_optimizationMain :: Test
test_optimizationMain =
  "optimize Main"
    ~: TestList
      [ interp (evalSelectCommand sampleQueryUnopt) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleQueryUnopt) sampleStore,
        interp (evalSelectCommand sampleQuerySuperUnopt) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleQuerySuperUnopt) sampleStore,
        interp (evalSelectCommand sampleNoOpt) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleNoOpt) sampleStore,
        interp (evalSelectCommand sampleJoinUnOpt1) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleJoinUnOpt1) sampleStore,
        interp (evalSelectCommand sampleJoinUnOpt2) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleJoinUnOpt2) sampleStore,
        interp (evalSelectCommand sampleJoinNoOpt1) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleJoinNoOpt1) sampleStore,
        interp (evalSelectCommand sampleJoinNoOpt2) sampleStore ~?= interp (evalSelectCommand $ optimizationMain sampleJoinNoOpt2) sampleStore
      ]
