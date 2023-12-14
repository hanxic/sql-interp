module TestOptimization where

import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Maybe
import GenVSQL
import Interpretation
import Optimization
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck as QC

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
            (Op2 (Var $ Dot "Students" (VarName "gender")) Is (Val $ StringVal "Male")),
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

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_optimizeFromId,
        test_optimizeWhereJoin,
        test_optimizationMain
      ]

qc :: IO ()
qc = do
  putStrLn "optimize_fromid"
  QC.quickCheck prop_optimizeFromId
  putStrLn "optimize_wherejoin"
  QC.quickCheck prop_optimizeFromId
