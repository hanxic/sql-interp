module TestSQL where

import Control.Applicative
import Parser (Parser)
import Parser qualified as P
import SQLParser
import SQLPrinter qualified as SP
import SQLSyntax
import Test.HUnit
import Test.QuickCheck qualified as QC
import TestUtils

prop_roundtrip_val :: DValue -> Bool
prop_roundtrip_val v = P.parse dvalueP (SP.pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (SP.pretty e) == Right e

prop_roundtrip_select :: SelectCommand -> Bool
prop_roundtrip_select sc = P.parse scP (SP.pretty sc) == Right sc

prop_roundtrip_create :: CreateCommand -> Bool
prop_roundtrip_create cc = P.parse ccP (SP.pretty cc) == Right cc

prop_roundtrip_delete :: DeleteCommand -> Bool
prop_roundtrip_delete dc = P.parse dcP (SP.pretty dc) == Right dc

prop_roundtrip_queries :: Queries -> Bool
prop_roundtrip_queries qs = P.parse sqlP (SP.printQueries qs) == Right qs

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
      P.parse (many (stringP "a")) "a  a  " ~?= Right [(), ()]
    ]

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
      P.parse (many (constP "&" 'a')) "&   &     " ~?= Right "aa"
    ]

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\'a\'" ~?= Right (StringVal "a"),
      P.parse stringValP "\'a\\\'\'" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\'a\'   \'b\'"
        ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\' a\'   \'b\'"
        ~?= Right [StringVal " a", StringVal "b"]
    ]

test_functionP :: Test
test_functionP =
  TestList
    [ P.parse functionP "LOWER" ~?= Right Lower,
      P.parse functionP "UPPER" ~?= Right Upper,
      P.parse functionP "LENGTH" ~?= Right Len,
      P.parse functionP "*" ~?= errorMsgUnitTest
    ]

test_aggFunctionP :: Test
test_aggFunctionP =
  TestList
    [ P.parse aggFunctionP "AVG" ~?= Right Avg,
      P.parse aggFunctionP "COUNT " ~?= Right Count,
      P.parse aggFunctionP "MAX " ~?= Right Max,
      P.parse aggFunctionP "MIN " ~?= Right Min,
      P.parse aggFunctionP "SUM " ~?= Right Sum,
      P.parse functionP "*" ~?= errorMsgUnitTest
    ]

test_uopP :: Test
test_uopP =
  TestList
    [ P.parse uopP "-" ~?= Right Neg,
      P.parse uopP "NOT" ~?= Right Not,
      P.parse uopP "NOTN" ~?= Left "No parses",
      P.parse (many uopP) "- NOT" ~?= Right [Neg, Not],
      P.parse uopP "+" ~?= Left "No parses" -- "+" nor a unary operator
    ]

test_bopP :: Test
test_bopP =
  TestList
    [ P.parse bopP "+" ~?= Right Plus,
      P.parse bopP "-" ~?= Right Minus,
      P.parse bopP "*" ~?= Right Times,
      P.parse bopP "//" ~?= Right Divide,
      P.parse bopP "%" ~?= Right Modulo,
      P.parse bopP "=" ~?= Right Eq,
      P.parse bopP ">" ~?= Right Gt,
      P.parse bopP ">=" ~?= Right Ge,
      P.parse bopP "<" ~?= Right Lt,
      P.parse bopP "<=" ~?= Right Le,
      P.parse bopP "AND" ~?= Right And,
      P.parse bopP "OR" ~?= Right Or,
      P.parse bopP "LIKE" ~?= Right Like,
      P.parse bopP "IS" ~?= Right Is,
      P.parse bopP "ISs" ~?= Left "No parses",
      P.parse (many bopP) "+ - * // %" ~?= Right [Plus, Minus, Times, Divide, Modulo],
      P.parse bopP "!" ~?= Left "No parses" -- "!" not a binary operator
    ]

test_varP :: Test
test_varP =
  TestList
    [ P.parse varP "*" ~?= errorMsgUnitTest,
      P.parse varP "1st" ~?= errorMsgUnitTest,
      P.parse varP "st1" ~?= Right (VarName "st1"),
      P.parse varP "\"\"" ~?= Right (VarName ""),
      -- TODO: not dealing with "\t" here. Maybe we should?
      P.parse varP "st.st1" ~?= Right (Dot "st" $ VarName "st1")
    ]

test_aggFunP :: Test
test_aggFunP =
  TestList
    [ P.parse aggFunP "AVG(*)" ~?= errorMsgUnitTest,
      P.parse aggFunP "SUM(1 + 2)" ~?= Right (AggFun Sum All (Op2 (Val $ IntVal 1) Plus (Val $ IntVal 2))),
      P.parse aggFunP "COUNT(DISTINCT 1)" ~?= Right (AggFun Count Distinct (Val $ IntVal 1)),
      P.parse aggFunP "COUNT(DISTINCT a)" ~?= Right (AggFun Count Distinct (Var $ VarName "a"))
    ]

test_countStyleP :: Test
test_countStyleP =
  TestList
    [ P.parse countStyleP "*" ~?= Right All,
      P.parse countStyleP "DISTINCT" ~?= Right Distinct,
      P.parse countStyleP "    DISTINCT" ~?= Right Distinct,
      P.parse countStyleP "    DISTINCT      " ~?= Right Distinct,
      P.doParse countStyleP "DISTINCT s" ~?= Just (Distinct, "s"),
      P.doParse countStyleP "DISTIN " ~?= Just (All, "DISTIN ")
    ]

test_exprsSelectP :: Test
test_exprsSelectP =
  TestList
    [ P.parse exprsSelectP "SELECT A" ~?= Right (All, [ColumnName $ Var $ VarName "A"]),
      P.parse exprsSelectP "SELECT A AS Something" ~?= Right (All, [ColumnAlias (Var $ VarName "A") "Something"]),
      P.parse exprsSelectP "SELECT DISTINCT A AS Something" ~?= Right (Distinct, [ColumnAlias (Var $ VarName "A") "Something"]),
      P.parse exprsSelectP "SELECT A A" ~?= Right (All, [ColumnName $ Var $ VarName "A"]),
      P.parse exprsSelectP "SELECT A AS Something, B AS C" ~?= Right (All, [ColumnAlias (Var $ VarName "A") "Something", ColumnAlias (Var $ VarName "B") "C"]),
      P.parse exprsSelectP "SELECT" ~?= errorMsgUnitTest
    ]

test_joinNamesPAux :: Test
test_joinNamesPAux =
  TestList
    [ P.parse joinNamesPAux " A = B" ~?= Right (VarName "A", VarName "B"),
      P.parse joinNamesPAux "A.C = B.C" ~?= Right (Dot "A" $ VarName "C", Dot "B" $ VarName "C")
    ]

test_joinNamesP :: Test
test_joinNamesP =
  TestList
    [ P.parse joinNamesP "ON A = B, C = D" ~?= Right [(VarName "A", VarName "B"), (VarName "C", VarName "D")],
      P.parse joinNamesP "ON A.C = B.C, E.F   = G.H" ~?= Right [(Dot "A" $ VarName "C", Dot "B" $ VarName "C"), (Dot "E" $ VarName "F", Dot "G" $ VarName "H")]
    ]

test_fakeFromExpressionP :: Test
test_fakeFromExpressionP =
  TestList
    [ P.parse fakeFromExpressionP "A JOIN B" ~?= Right (FakeJoin (Fake "A") InnerJoin (Fake "B")),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y" ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y, A.X = B.Y" ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN B ON X = Y, A.X = B.Y JOIN C ON Z = W" ~?= Right (FakeJoin (FakeJoin (Fake "A") InnerJoin (FakeOn (Fake "B") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])) InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")])),
      P.parse fakeFromExpressionP "A JOIN (B JOIN C ON Z = W) ON X = Y, A.X = B.Y " ~?= Right (FakeJoin (Fake "A") InnerJoin (FakeOn (FakeJoin (Fake "B") InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")])) [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")])),
      P.parse fakeFromExpressionP "A JOIN (B JOIN C ON Z = W) ON X = Y, A.X = B.Y "
        ~?= Right
          ( FakeJoin
              (Fake "A")
              InnerJoin
              ( FakeOn
                  (FakeJoin (Fake "B") InnerJoin (FakeOn (Fake "C") [(VarName "Z", VarName "W")]))
                  [ (VarName "X", VarName "Y"),
                    ( Dot "A" $ VarName "X",
                      Dot "B" $ VarName "Y"
                    )
                  ]
              )
          ),
      P.parse fakeFromExpressionP "A AS C JOIN B AS D ON X = Y, A.X = B.Y " ~?= Right (FakeJoin (FakeAlias "A" "C") InnerJoin (FakeOn (FakeAlias "B" "D") [(VarName "X", VarName "Y"), (Dot "A" $ VarName "X", Dot "B" $ VarName "Y")]))
    ]

test_fromSelectP :: Test
test_fromSelectP =
  TestList
    [ P.parse fromSelectP "FROM A" ~?= Right (TableRef "A"),
      P.parse fromSelectP "FROM A JOIN B" ~?= Right (Join (TableRef "A") InnerJoin (TableRef "B") []),
      P.parse fromSelectP "FROM A JOIN B LEFT JOIN C" ~?= Right (Join (Join (TableRef "A") InnerJoin (TableRef "B") []) LeftJoin (TableRef "C") []),
      P.parse fromSelectP "FROM A JOIN B ON C = D" ~?= Right (Join (TableRef "A") InnerJoin (TableRef "B") [(VarName "C", VarName "D")])
    ]

test_groupbySelectP :: Test
test_groupbySelectP =
  TestList
    [ P.parse groupbySelectP "GROUP BY" ~?= Right [],
      P.parse groupbySelectP "GROUP" ~?= Right [],
      P.parse groupbySelectP "" ~?= Right [],
      P.parse groupbySelectP "GROUP BY S" ~?= Right [VarName "S"]
    ]

test_orderbySelectP :: Test
test_orderbySelectP =
  TestList
    [ P.parse orderbySelectP "ORDER" ~?= Right [],
      P.parse orderbySelectP "ORDER BY" ~?= Right [],
      P.parse orderbySelectP "" ~?= Right [], -- Not the right test cases
      P.parse orderbySelectP "ORDER BY S" ~?= Right [(VarName "S", Nothing, Nothing)],
      P.parse orderbySelectP "ORDER BY S ASC, A DESC" ~?= Right [(VarName "S", Just ASC, Nothing), (VarName "A", Just DESC, Nothing)],
      P.parse orderbySelectP "ORDER BY S ASC, A NULLS FIRST " ~?= Right [(VarName "S", Just ASC, Nothing), (VarName "A", Nothing, Just NULLSFIRST)]
    ]

test_limitSelectP :: Test
test_limitSelectP =
  TestList
    [ P.parse limitSelectP "LIMIT" ~?= Right Nothing,
      P.parse limitSelectP "LIMIT 1" ~?= Right (Just 1),
      P.parse limitSelectP "    " ~?= Right Nothing,
      P.parse limitSelectP "    ;" ~?= Right Nothing
    ]

test_offsetSelectP :: Test
test_offsetSelectP =
  TestList
    [ P.parse offsetSelectP "OFFSET" ~?= Right Nothing,
      P.parse offsetSelectP "OFFSET 1" ~?= Right (Just 1),
      P.parse offsetSelectP "   " ~?= Right Nothing,
      P.parse offsetSelectP " ;" ~?= Right Nothing
    ]

test_ccPrefixP :: Test
test_ccPrefixP =
  TestList
    [ P.parse ccPrefixP "CREATE" ~?= errorMsgUnitTest,
      P.parse ccPrefixP "CREATE  TABLE" ~?= Right False,
      P.parse ccPrefixP "CREATE TABLE IF" ~?= Right False,
      P.parse ccPrefixP " CREATE TABLE IF NOT EXISTS" ~?= Right True
    ]

test_idCreateP :: Test
test_idCreateP =
  TestList
    [ P.parse idCreateP "id BIGINT PRIMARY KEY" ~?= Right ("id", IntType 32, True),
      P.parse idCreateP "id BOOLEAN PRIMARY KEY" ~?= Right ("id", BoolType, True),
      P.parse idCreateP "Var4 INT(26) PRIMARY KEY" ~?= Right ("Var4", IntType 26, True)
    ]

test_ccP :: Test
test_ccP =
  TestList
    [ P.parse ccP "CREATE TABLE Grades (student_id INTEGER)" ~?= Right (CreateCommand False "Grades" [("student_id", IntType 16, False)]),
      P.parse ccP "CREATE TABLE Grades (student_id INTEGER PRIMARY KEY, grades INTEGER)" ~?= Right (CreateCommand False "Grades" [("student_id", IntType 16, True), ("grades", IntType 16, False)])
    ]

test_sqlP :: Test
test_sqlP =
  TestList
    [ P.parse sqlP "CREATE TABLE Grades (student_id INTEGER); CREATE TABLE Students (student_id INTEGER PRIMARY KEY, name VARCHAR(255));"
        ~?= Right
          [ CreateQuery $
              CreateCommand
                False
                "Grades"
                [("student_id", IntType 16, False)],
            CreateQuery $
              CreateCommand
                False
                "Students"
                [ ("student_id", IntType 16, True),
                  ("name", StringType 255, False)
                ]
          ]
    ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_wsP,
        test_stringP,
        test_constP,
        test_stringValP,
        test_functionP,
        test_aggFunctionP,
        test_uopP,
        test_bopP,
        test_varP,
        test_aggFunP,
        test_countStyleP,
        test_exprsSelectP,
        test_joinNamesPAux,
        test_joinNamesP,
        test_fakeFromExpressionP,
        test_fromSelectP,
        test_groupbySelectP,
        test_orderbySelectP,
        test_limitSelectP,
        test_offsetSelectP,
        test_ccPrefixP,
        test_idCreateP,
        test_ccP,
        test_sqlP
      ]

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_select"
  QC.quickCheck prop_roundtrip_select
  putStrLn "roundtrip_create"
  QC.quickCheck prop_roundtrip_create
  putStrLn "roundtrip_delete"
  QC.quickCheck prop_roundtrip_delete
  putStrLn "roundtrip_queries"
  QC.quickCheck prop_roundtrip_queries