module TestTable where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import GenSQL
import Parser qualified as P
import SQLSyntax
import TableParser
import TablePrinter
import TablePrinter qualified as TP
import TableSyntax
import Test.HUnit
import Test.QuickCheck qualified as QC
import TestUtils
import Text.PrettyPrint

prop_roundtrip_header :: Header -> Bool
prop_roundtrip_header h = P.parse headerP (Text.PrettyPrint.render $ TP.ppHeader h) == Right h

prop_roundtrip_valT :: QC.Property
prop_roundtrip_valT = QC.forAll (QC.arbitrary :: QC.Gen DType) $ \dtype ->
  QC.forAll (genValTC dtype) $ \dvalue ->
    P.parse (dvalueTP dtype) (TP.pretty dvalue) == Right dvalue

prop_roundtrip_row :: QC.Property
prop_roundtrip_row = QC.forAll genAH $ \ah ->
  QC.forAll (genRowFromAH ah) $ \row ->
    P.parse (rowP ah) (render $ TP.ppRow ah row) == Right row

test103 = [(VarName "var0", StringType 249), (VarName "var1", StringType 24), (VarName "var3", StringType 172)]

test104 = Map.fromList [(VarName "var0", StringVal ""), (VarName "var1", NullVal), (VarName "var3", StringVal "/")]

test105 = render $ TP.ppRow test103 test104

-- >>> test105
-- "\"\",NULL,/"

prop_roundtrip_table :: QC.Property
prop_roundtrip_table = QC.forAll genPKIN $ \(pk, iName) ->
  let ah = NE.toList pk ++ iName
   in QC.forAll (genTableData ah) $ \td ->
        let table = Table pk iName td
         in P.parse (tableP pk iName) (TP.pretty table) == Right table

test_ppRow :: Test
test_ppRow =
  TestList
    [ pretty (Map.fromList [(VarName "a", StringVal "c"), (VarName "b", IntVal 2)]) ~?= "c,2",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "c")]) ~?= "TRUE,2,c",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "c d")]) ~?= "TRUE,2,'c d'",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "NULL")]) ~?= "TRUE,2,'NULL'",
      pretty (Map.fromList [(VarName "a", StringVal "Hello"), (VarName "b", StringVal "THERE")]) ~?= "Hello,THERE"
    ]

test_ppPrimaryKeys :: Test
test_ppPrimaryKeys =
  TestList
    [ render (ppPrimaryKeys (NE.fromList [(VarName "a", StringType 255), (VarName "b", IntType 32), (VarName "c", BoolType)])) ~?= "a,b,c"
    ]

test_ppTable :: Test
test_ppTable =
  TestList
    [ pretty
        ( Table
            (NE.fromList [(VarName "a", StringType 255), (VarName "b", BoolType)])
            [(VarName "c", IntType 32), (VarName "d", BoolType)]
            [ Map.fromList
                [(VarName "a", StringVal "hello"), (VarName "b", BoolVal True), (VarName "c", IntVal 255), (VarName "d", BoolVal False), (VarName "e", StringVal "Not a part")],
              Map.fromList
                [(VarName "a", StringVal "hello"), (VarName "b", BoolVal True), (VarName "c", IntVal 255), (VarName "d", BoolVal False), (VarName "e", StringVal "Not a part")]
            ]
        )
        ~?= "a,b,c,d\nhello,TRUE,255,FALSE\nhello,TRUE,255,FALSE"
    ]

test_varTP :: Test
test_varTP =
  TestList
    [ P.parse varTP "*" ~?= errorMsgUnitTest,
      P.parse varTP "1st" ~?= errorMsgUnitTest,
      P.parse varTP "st1" ~?= Right (VarName "st1"),
      P.parse varTP "\"\"" ~?= errorMsgUnitTest,
      -- TODO: not dealing with "\t" here. Maybe we should?
      P.parse varTP "st.st1" ~?= Right (Dot "st" $ VarName "st1"),
      P.doParse varTP "st\n.st1" ~?= Just (VarName "st", "\n.st1")
    ]

test_headerP :: Test
test_headerP =
  TestList
    [ P.parse headerP "a,b,c" ~?= Right [VarName "a", VarName "b", VarName "c"],
      P.doParse headerP "a,b,c\nd,e,f" ~?= Just ([VarName "a", VarName "b", VarName "c"], "\nd,e,f")
    ]

test_nullValTP :: Test
test_nullValTP =
  TestList
    [ P.doParse nullValTP "," ~?= Just (NullVal, ","),
      P.doParse nullValTP "NULL\n" ~?= Just (NullVal, "\n"),
      P.parse nullValTP "\t\n" ~?= Right NullVal,
      P.parse nullValTP "Something" ~?= errorMsgUnitTest
    ]

test_stringValTP :: Test
test_stringValTP =
  TestList
    [ P.parse stringValTP "    ," ~?= errorMsgUnitTest,
      P.doParse stringValTP "this   ," ~?= Just (StringVal "this", ","),
      P.parse stringValTP "\'Ha , Ha\'" ~?= Right (StringVal "Ha , Ha")
    ]

test_boolValTP :: Test
test_boolValTP =
  TestList
    [ P.doParse boolValTP "TRUE    " ~?= Just (BoolVal True, ""),
      P.doParse boolValTP "FALSE    " ~?= Just (BoolVal False, ""),
      P.doParse boolValTP "TRUE   , " ~?= Just (BoolVal True, ", "),
      P.doParse boolValTP "TRUE   \n " ~?= Just (BoolVal True, "\n ")
    ]

test_dvalueTP :: Test
test_dvalueTP =
  TestList
    [ P.parse (dvalueTP (StringType 255)) "\'a\'" ~?= Right (StringVal "a")
    ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_ppRow,
        test_ppPrimaryKeys,
        test_ppTable,
        test_headerP,
        test_nullValTP,
        test_stringValTP,
        test_boolValTP,
        test_dvalueTP
      ]

qc :: IO ()
qc = do
  putStrLn "roundtrip_header"
  QC.quickCheckWith QC.stdArgs {QC.maxSuccess = 1000} prop_roundtrip_header
  putStrLn "roundtrip_valT"
  QC.quickCheckWith QC.stdArgs {QC.maxSuccess = 1000} prop_roundtrip_valT
  putStrLn "roundtrip_row"
  QC.quickCheckWith QC.stdArgs {QC.maxSuccess = 1000} prop_roundtrip_row
  putStrLn "roundtrip_table"
  QC.quickCheckWith QC.stdArgs {QC.maxSuccess = 1000} prop_roundtrip_table
