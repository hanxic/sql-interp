module TableParser where

import Control.Applicative
import Control.Monad
import Data.Bits (Bits (bitSize), FiniteBits (finiteBitSize))
import Data.Char qualified as Char
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map as Map (Map, empty, fromList, insert)
import GHC.Generics (D)
import GenSQL
import Parser (Parser)
import Parser qualified as P
import SQLParser qualified as P
import SQLParser qualified as SP
import SQLSyntax
import TablePrinter qualified as TP
import TableSyntax (Header, IndexName, PrimaryKeys, Row, Table (Table, indexName, primaryKeys, tableData))
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, render)

prop_roundtrip_header :: Header -> Bool
prop_roundtrip_header h = P.parse headerP (Text.PrettyPrint.render $ TP.ppHeader h) == Right h

prop_roundtrip_valT :: DValue -> Bool
prop_roundtrip_valT v = P.parse dvalueTP (TP.pretty v) == Right v

{- prop_roundtrip_row :: AnnotatedHeader -> Bool
prop_roundtrip_row ah =
  let (r :: Row) = undefined
   in P.parse (rowP ah) (TP.pretty r) == Right r -}
prop_roundtrip_row :: QC.Property
prop_roundtrip_row = QC.forAll genAH $ \ah ->
  QC.forAll (genRowFromAH ah) $ \row ->
    P.parse (rowP ah) (render $ TP.ppRow ah row) == Right row

prop_roundtrip_row' :: QC.Property
prop_roundtrip_row' =
  QC.forAll (genRowFromAH test5) $ \row ->
    P.parse (rowP test5) (render $ TP.ppRow test5 row) == Right row

-- TODO:
prop_roundtrip_table :: QC.Property
prop_roundtrip_table = QC.forAll genPKIN $ \(pk, iName) ->
  let ah = NE.toList pk ++ iName
   in QC.forAll (genTableData ah) $ \td ->
        let table = Table pk iName td
         in P.parse (tableP pk iName) (TP.pretty table) == Right table

{- P.parse tableP (TP.pretty t) == Right t -}

prop_roundtrip_table' :: QC.Property
prop_roundtrip_table' =
  let pk = test998
   in let iName = test997
       in let ah = NE.toList pk ++ iName
           in QC.forAll (genTableData ah) $ \td ->
                let table = Table pk iName td
                 in P.parse (tableP pk iName) (TP.pretty table) == Right table

test101 =
  [fromList [(VarName "var0", IntVal 8), (VarName "var1", IntVal 34175), (VarName "var2", BoolVal False), (VarName "var37", BoolVal True), (Dot "table18" (Dot "table9" (VarName "var4")), StringVal ""), (Dot "table4" (Dot "table2" (VarName "var1")), NullVal), (Dot "table9" (Dot "table4" (VarName "var2")), NullVal)], fromList [(VarName "var0", IntVal 8), (VarName "var1", NullVal), (VarName "var2", NullVal), (VarName "var37", NullVal), (Dot "table18" (Dot "table9" (VarName "var4")), NullVal), (Dot "table4" (Dot "table2" (VarName "var1")), BoolVal False), (Dot "table9" (Dot "table4" (VarName "var2")), IntVal 4613)]]

tempTable = Table test998 test997 test101

-- >>> TP.pretty tempTable
-- "var0,var1,var2,table4.table2.var1,table9.table4.var2,table18.table9.var4,var37\n8,34175,FALSE,NULL,NULL,\"\",TRUE\n8,NULL,NULL,FALSE,4613,NULL,NULL"

test103 = "var0,var1,var2,table4.table2.var1,table9.table4.var2,table18.table9.var4,var37\n8,34175,FALSE,NULL,NULL,\"\",TRUE\n8,NULL,NULL,FALSE,4613,NULL,NULL"

test102 = P.doParse (tableP test998 test997) test103

-- >>> test102
-- Just (Table {primaryKeys = (VarName "var0",IntType 3) :| [], indexName = [(VarName "var1",IntType 21),(VarName "var2",BoolType),(Dot "table4" (Dot "table2" (VarName "var1")),BoolType),(Dot "table9" (Dot "table4" (VarName "var2")),IntType 13),(Dot "table18" (Dot "table9" (VarName "var4")),StringType 1),(VarName "var37",BoolType)], tableData = [fromList [(VarName "var0",IntVal 8),(VarName "var1",IntVal 34175),(VarName "var2",BoolVal False),(VarName "var37",BoolVal True),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal ""),(Dot "table4" (Dot "table2" (VarName "var1")),NullVal),(Dot "table9" (Dot "table4" (VarName "var2")),NullVal)],fromList [(VarName "var0",IntVal 8),(VarName "var1",NullVal),(VarName "var2",NullVal),(VarName "var37",NullVal),(Dot "table18" (Dot "table9" (VarName "var4")),NullVal),(Dot "table4" (Dot "table2" (VarName "var1")),BoolVal False),(Dot "table9" (Dot "table4" (VarName "var2")),IntVal 4613)]]},"")

returnTable =
  Table
    { primaryKeys = (VarName "var0", IntType 3) NE.:| [],
      indexName = [(VarName "var1", IntType 21), (VarName "var2", BoolType), (Dot "table4" (Dot "table2" (VarName "var1")), BoolType), (Dot "table9" (Dot "table4" (VarName "var2")), IntType 13), (Dot "table18" (Dot "table9" (VarName "var4")), StringType 1), (VarName "var37", BoolType)],
      tableData = [fromList [(VarName "var0", IntVal 4), (VarName "var1", IntVal 525969), (VarName "var2", BoolVal False), (VarName "var37", NullVal), (Dot "table18" (Dot "table9" (VarName "var4")), StringVal "/"), (Dot "table4" (Dot "table2" (VarName "var1")), NullVal), (Dot "table9" (Dot "table4" (VarName "var2")), IntVal 2977)]]
    }

test10001 = [(VarName "var1", IntType 21), (VarName "var2", BoolType), (Dot "table4" (Dot "table2" (VarName "var1")), BoolType), (Dot "table9" (Dot "table4" (VarName "var2")), IntType 13), (Dot "table18" (Dot "table9" (VarName "var4")), StringType 1), (VarName "var37", BoolType)]

-- >>> TP.ppIndexName test10001
-- var1,var2,table4.table2.var1,table9.table4.var2,table18.table9.var4,var37

test104 = "6,738370,NULL,NULL,NULL,NULL,NULL\nNULL,320619,TRUE,TRUE,NULL,\"\",FALSE\nNULL,NULL,FALSE,FALSE,7916,J,TRUE"

test105 = P.doParse (many (rowP test5)) test104

-- >>> test105
-- Just ([fromList [(VarName "var0",IntVal 6),(VarName "var1",IntVal 738370),(VarName "var2",NullVal),(VarName "var37",NullVal),(Dot "table18" (Dot "table9" (VarName "var4")),NullVal),(Dot "table4" (Dot "table2" (VarName "var1")),NullVal),(Dot "table9" (Dot "table4" (VarName "var2")),NullVal)],fromList [(VarName "var0",NullVal),(VarName "var1",IntVal 320619),(VarName "var2",BoolVal True),(VarName "var37",BoolVal False),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal ""),(Dot "table4" (Dot "table2" (VarName "var1")),BoolVal True),(Dot "table9" (Dot "table4" (VarName "var2")),NullVal)],fromList [(VarName "var0",NullVal),(VarName "var1",NullVal),(VarName "var2",BoolVal False),(VarName "var37",BoolVal True),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal "J"),(Dot "table4" (Dot "table2" (VarName "var1")),BoolVal False),(Dot "table9" (Dot "table4" (VarName "var2")),IntVal 7916)]],"")

test5 =
  [ (VarName "var0", IntType 3),
    (VarName "var1", IntType 21),
    (VarName "var2", BoolType),
    (Dot "table4" (Dot "table2" (VarName "var1")), BoolType),
    (Dot "table9" (Dot "table4" (VarName "var2")), IntType 13),
    (Dot "table18" (Dot "table9" (VarName "var4")), StringType 1),
    (VarName "var37", BoolType)
  ]

test108 = P.doParse (rowP test5) "6,602402,FALSE,FALSE,7959,\"\",TRUE\n"

-- >>> test108
-- Just (fromList [(VarName "var0",IntVal 6),(VarName "var1",IntVal 602402),(VarName "var2",BoolVal False),(VarName "var37",BoolVal True),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal ""),(Dot "table4" (Dot "table2" (VarName "var1")),BoolVal False),(Dot "table9" (Dot "table4" (VarName "var2")),IntVal 7959)],"\n")

-- >>> test105
-- Just (fromList [(VarName "var0",IntVal 6),(VarName "var1",IntVal 602402),(VarName "var2",BoolVal False),(VarName "var37",BoolVal True),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal ""),(Dot "table4" (Dot "table2" (VarName "var1")),BoolVal False),(Dot "table9" (Dot "table4" (VarName "var2")),IntVal 7959)],"\n")

test106 = P.doParse (P.boolValP) "TRUE\n1"

test107 = P.doParse (separatorP) "\n1"

-- >>> test107
-- Just ((),"\n1")

-- >>> test106
-- Just (BoolVal True,"1")

test998 =
  NE.fromList
    [ (VarName "var0", IntType 3)
    ]

test997 =
  [ (VarName "var1", IntType 21),
    (VarName "var2", BoolType),
    (Dot "table4" (Dot "table2" (VarName "var1")), BoolType),
    (Dot "table9" (Dot "table4" (VarName "var2")), IntType 13),
    (Dot "table18" (Dot "table9" (VarName "var4")), StringType 1),
    (VarName "var37", BoolType)
  ]

test999 = QC.sample $ genTableData test5

test1 = QC.sample $ genRowFromAH test5

test2 =
  fromList
    [ (VarName "var0", IntVal 2),
      (VarName "var1", IntVal 1986975),
      (VarName "var2", BoolVal False),
      (VarName "var37", BoolVal False),
      (Dot "table18" (Dot "table9" (VarName "var4")), StringVal "2"),
      (Dot "table4" (Dot "table2" (VarName "var1")), NullVal),
      (Dot "table9" (Dot "table4" (VarName "var2")), IntVal 4021)
    ]

-- >>> dvalueTypeCheck NullVal BoolType
-- True

-- >>> TP.ppRow test5 test2
-- 2,1986975,FALSE,NULL,4021,2,FALSE

test4 = "2,1986975,FALSE,NULL,4021,2,FALSE"

test3 = P.doParse (rowP test5) test4

-- >>> test3
-- Nothing

test6 = "2,1986975,FALSE,NULL,4021,2,FALSE"

test7 =
  P.doParse
    ( rowP
        [ (VarName "var0", IntType 3),
          (VarName "var1", IntType 21),
          (VarName "var2", BoolType),
          (Dot "table4" (Dot "table2" (VarName "var1")), BoolType),
          (Dot "table9" (Dot "table4" (VarName "var2")), IntType 13),
          (Dot "table18" (Dot "table9" (VarName "var4")), StringType 1)
        ]
    )
    test6

-- >>> test7
-- Just (fromList [(VarName "var0",IntVal 2),(VarName "var1",IntVal 1986975),(VarName "var2",BoolVal False),(Dot "table18" (Dot "table9" (VarName "var4")),StringVal "2"),(Dot "table4" (Dot "table2" (VarName "var1")),NullVal),(Dot "table9" (Dot "table4" (VarName "var2")),IntVal 4021)],",FALSE")

-- >>> P.doParse (dvalueTP) test6
-- Just (NullVal,",NULL,*jlz,TRUE")

-- >>> P.doParse (dvalueTP) test4
-- Just (IntVal 55,",105732,TRUE")

type PrimaryKeysList = IndexName

headerP :: Parser Header
headerP = P.sepBy SP.varP P.comma

test_headerP :: Test
test_headerP =
  TestList
    [ P.parse headerP "a,b,c" ~?= Right [VarName "a", VarName "b", VarName "c"],
      P.doParse headerP "a,b,c\nd,e,f" ~?= Just ([VarName "a", VarName "b", VarName "c"], "d,e,f")
    ]

-- | Given a primary key variable list, and a index name variable list, return a pair of primary keys and index names

{- validHeaderP :: Header -> Header -> Parser (Header, Header)
validHeaderP pkVarList iNameVarList = headerP >>= validHeaderPAux pkVarList iNameVarList
  where
    validHeaderPAux :: Header -> Header -> Header -> Parser (Header, Header)
    validHeaderPAux pkVarList iNameVarList header =
      if not $
        checkRepetitive header
          && length header == length pkVarList + length iNameVarList
        then
          let pHeader = filter (`elem` pkVarList) header
           in let iHeader = filter (`elem` iNameVarList) $ filter (`notElem` pkVarList) pHeader
               in if pHeader == pkVarList && iHeader == iNameVarList then return header else empty
        else empty -}

reservedTKeywords :: [String]
reservedTKeywords = [","]

{- Redefine our own value -}
{- NullVal <$ lookAhead (P.comma) -}

spacesP :: Parser [Char]
spacesP = many (P.char ' ' <|> P.char '\t' <|> P.char '\v')

newLineP :: Parser Char
newLineP = P.char '\n' <|> P.char '\r' <|> P.char '\f'

nullValTP :: Parser DValue
nullValTP =
  SP.nullValP
    <|> NullVal <$ P.lookAhead (SP.wsP P.comma <|> spacesP *> P.wsbP newLineP)
    <|> NullVal <$ P.lookAhead (spacesP *> P.wsbP P.eof)

-- >>> P.doParse (spacesP *> newLineP) "\t\n"
-- Just ('\n',"")

-- >>> P.doParse nullValTP "\t\n"
-- Just (NullVal,"\t\n")

test_nullValTP :: Test
test_nullValTP =
  TestList
    [ P.parse nullValTP "," ~?= Right NullVal,
      P.parse nullValTP "NULL" ~?= Right NullVal,
      P.parse nullValTP "\t\n" ~?= Right NullVal,
      P.parse nullValTP "Something" ~?= SP.errorMsgUnitTest
    ]

stringValTP :: Parser DValue
stringValTP =
  SP.stringValP
    <|> ( StringVal
            <$> some (P.satisfy (\x -> x /= ',' && not (Char.isSpace x)))
        )

test_stringValTP :: Test
test_stringValTP =
  TestList
    [ P.parse stringValTP "    ," ~?= SP.errorMsgUnitTest,
      P.parse stringValTP "this," ~?= Right (StringVal "this"),
      P.parse stringValTP "\"Ha , Ha\"" ~?= Right (StringVal "Ha , Ha")
    ]

boolValTP :: Parser DValue
boolValTP = trueP <|> falseP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ P.string "TRUE"
    falseP :: Parser DValue
    falseP = BoolVal False <$ P.string "FALSE"

intValTP :: Parser DValue
intValTP = IntVal <$> P.int

dvalueTP :: Parser DValue
dvalueTP = (intValTP <* separatorP) <|> (boolValTP <* separatorP) <|> nullValTP <|> stringValTP

{- case dtype of
  IntType i -> P.intValP <* separatorP <|> nullValTP
  BoolType -> P.boolValP <* separatorP <|> nullValTP
  StringType n -> nullValTP <|> stringValTP -}

{-     checkBitSize :: Int -> DValue ->  Parser DValue
    checkBitSize n dvalue@(IntVal i) = if finiteBitSize i > n then P.empty else return dvalue
    checkBitSize n dvalue@(StringVal s) = if finiteBitSize s > n then P.empty else return dvalue
    checkBitSize n dvalue@(BoolVal _) = return dvalue -}
{- (P.intValP <* separatorP) <|> (P.boolValP <* separatorP) <|> nullValTP <|> stringValTP -}

separatorP :: Parser ()
separatorP = void (P.lookAhead (P.space <|> P.comma)) <|> P.lookAhead P.eof

test_dvalueTP :: Test
test_dvalueTP =
  TestList
    [ P.parse dvalueTP "\"a\"" ~?= Right (StringVal "a")
    ]

validHeaderP :: PrimaryKeys -> IndexName -> Parser AnnotatedHeader
validHeaderP pkVarList iNameVarList = headerP >>= validHeaderPAux pkVarList iNameVarList

validHeaderPAux :: PrimaryKeys -> IndexName -> Header -> Parser AnnotatedHeader
validHeaderPAux pk iName header =
  if not $ checkRepetitive header && length header == length pk + length iName
    then maybe Control.Applicative.empty return (annotateHeaderMaybe pk iName header)
    else Control.Applicative.empty
  where
    annotateHeaderMaybe :: PrimaryKeys -> IndexName -> Header -> Maybe AnnotatedHeader
    annotateHeaderMaybe pk iName header =
      let pkList = NE.toList pk
       in foldr (\x acc -> (:) <$> annotateColumnMaybe pkList iName x <*> acc) (Just []) header
    annotateColumnMaybe :: [(Var, DType)] -> [(Var, DType)] -> Var -> Maybe (Var, DType)
    annotateColumnMaybe pkList iName headerVar =
      case lookup headerVar pkList of
        Just dtyp -> Just (headerVar, dtyp)
        Nothing ->
          case lookup headerVar iName of
            Just dtyp2 -> Just (headerVar, dtyp2)
            Nothing -> Nothing

{- let pkVarList = map fst pk in
   let iNameVarList = map fst iName
if not $
  checkRepetitive header
    && length header == length pkVarList + length iNameVarList
  then
    let pHeader = filter (`elem` pkVarList) header
     in let iHeader = filter (`elem` iNameVarList) $ filter (`notElem` pkVarList) pHeader
         in if pHeader == pkVarList && iHeader == iNameVarList then return header else empty
  else empty -}

{-
What to do:
If there is a table created that is not
-}

-- | Assume sorted
checkRepetitive :: (Eq a, Ord a) => [a] -> Bool
checkRepetitive xs =
  let sortedXS = sort xs
   in checkRepetitiveAux sortedXS
  where
    checkRepetitiveAux :: (Eq a) => [a] -> Bool
    checkRepetitiveAux [] = False
    checkRepetitiveAux [x] = False
    checkRepetitiveAux (x1 : x2 : xs) =
      x1 == x2 || checkRepetitiveAux (x2 : xs)

rowP :: AnnotatedHeader -> Parser Row
rowP = flip rowPAux Map.empty

rowPAux :: AnnotatedHeader -> Row -> Parser Row
rowPAux ah row =
  case ah of
    [] -> row <$ some spacesP
    [x@(_, dtype)] -> validValP x row =<< (dvalueTP <* spacesP)
    (x@(_, dtype) : xs) -> do
      dvalue <- dvalueTP <* SP.wsP P.comma
      row' <- validValP x row dvalue
      rowPAux xs row'
  where
    validValP :: (Var, DType) -> Row -> DValue -> Parser Row
    validValP (var, dtype) row dvalue =
      if dvalueTypeCheck dvalue dtype
        then return $ Map.insert var dvalue row
        else maybe Control.Applicative.empty (\x -> return $ Map.insert var x row) $ convertMaybe dvalue dtype

convertMaybe :: DValue -> DType -> Maybe DValue
convertMaybe (IntVal i) (StringType n) | n >= length (show i) = Just $ StringVal $ show i
convertMaybe (BoolVal b) (StringType n)
  | n >= 1 =
      Just $ StringVal $ show b
convertMaybe _ _ = Nothing

test = Char.isSpace

tableP :: PrimaryKeys -> IndexName -> Parser Table
tableP pk iName = do
  ah <- validHeaderP pk iName
  tableData <- many (SP.wsbP (rowP ah))
  return (Table pk iName tableData)
