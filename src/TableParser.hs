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

prop_roundtrip_table :: QC.Property
prop_roundtrip_table = QC.forAll genPKIN $ \(pk, iName) ->
  let ah = NE.toList pk ++ iName
   in QC.forAll (genTableData ah) $ \td ->
        let table = Table pk iName td
         in P.parse (tableP pk iName) (TP.pretty table) == Right table

{- P.parse tableP (TP.pretty t) == Right t -}

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
  NullVal <$ (P.string "NULL" <* P.lookAhead (spacesP *> (P.eof <|> void (newLineP <|> P.comma))))
    <|> NullVal <$ P.lookAhead (SP.wsP P.comma <|> spacesP *> P.wsbP newLineP)
    <|> NullVal <$ P.lookAhead (spacesP *> P.wsbP P.eof)

-- >>> P.doParse (spacesP *> newLineP) "\t\n"
-- Just ('\n',"")

-- >>> P.doParse nullValTP "\t\n"
-- Just (NullVal,"\t\n")

test_nullValTP :: Test
test_nullValTP =
  TestList
    [ P.doParse nullValTP "," ~?= Just (NullVal, ","),
      P.doParse nullValTP "NULL\n" ~?= Just (NullVal, "\n"),
      P.parse nullValTP "\t\n" ~?= Right NullVal,
      P.parse nullValTP "Something" ~?= SP.errorMsgUnitTest
    ]

stringValTP :: Parser DValue
stringValTP =
  ( StringVal <$> (SP.stringInP '\"' many <* spacesP)
      <|> ( StringVal
              <$> some (P.satisfy (\x -> x /= ',' && not (Char.isSpace x)))
          )
  )
    <* spacesP

test_stringValTP :: Test
test_stringValTP =
  TestList
    [ P.parse stringValTP "    ," ~?= SP.errorMsgUnitTest,
      P.doParse stringValTP "this   ," ~?= Just (StringVal "this", ","),
      P.parse stringValTP "\"Ha , Ha\"" ~?= Right (StringVal "Ha , Ha")
    ]

boolValTP :: Parser DValue
boolValTP = (trueP <|> falseP) <* spacesP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ P.string "TRUE"
    falseP :: Parser DValue
    falseP = BoolVal False <$ P.string "FALSE"

test_boolValTP :: Test
test_boolValTP =
  TestList
    [ P.doParse boolValTP "TRUE    " ~?= Just (BoolVal True, ""),
      P.doParse boolValTP "FALSE    " ~?= Just (BoolVal False, ""),
      P.doParse boolValTP "TRUE   , " ~?= Just (BoolVal True, ", "),
      P.doParse boolValTP "TRUE   \n " ~?= Just (BoolVal True, "\n ")
    ]

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
rowP ah = rowPAux ah Map.empty

-- How to distinguish a NULL value or the end of file
rowPAux :: AnnotatedHeader -> Row -> Parser Row
rowPAux ah row =
  case ah of
    [] -> row <$ some spacesP
    [x@(_, dtype)] -> validValP x row =<< (dvalueTP <* spacesP <* P.lookAhead (void newLineP <|> P.eof))
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
  tableData <- ([] <$ (many P.space *> P.eof)) <|> P.sepBy (rowP ah <* spacesP) newLineP
  return (Table pk iName tableData)

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_headerP,
        test_nullValTP,
        test_stringValTP,
        test_boolValTP,
        test_dvalueTP
      ]

qc :: IO ()
qc = do
  putStrLn "roundtrip_header"
  QC.quickCheck prop_roundtrip_header
  putStrLn "roundtrip_valT"
  QC.quickCheck prop_roundtrip_valT
  putStrLn "roundtrip_row"
  QC.quickCheck prop_roundtrip_row
  putStrLn "roundtrip_table"
  QC.quickCheck prop_roundtrip_table

test1 =
  ( NE.fromList [(VarName "var0", StringType 177)],
    [ (VarName "var1", BoolType),
      (VarName "var3", IntType 15),
      (VarName "var7", StringType 61),
      (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntType 4),
      (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolType),
      (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntType 14)
    ]
  )

test2 =
  [ fromList [(VarName "var0", StringVal ".e>Po1?>"), (VarName "var1", BoolVal False), (VarName "var3", IntVal 21762), (VarName "var7", StringVal "+>IeGT+?{jhg6y12XlnAqi<D%za1&Ig"), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 2), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal False), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 4685)],
    fromList [(VarName "var0", StringVal "J|sY^?\fM9q%^!6BP\tc+_q344"), (VarName "var1", BoolVal True), (VarName "var3", IntVal 4414), (VarName "var7", NullVal), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 1), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal False), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 1288)],
    fromList [(VarName "var0", StringVal "J~-\f*zYKcCe6P^l/c4\\4#}M|\v2ck</y&l_"), (VarName "var1", BoolVal True), (VarName "var3", IntVal 6810), (VarName "var7", StringVal "[NkQ uv[1GFt^"), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 13), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal True), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 13641)],
    fromList [(VarName "var0", StringVal "p[\t|R%^OpFN\tjb^hOv@r0G0>zlX r.{vHv3*l*pGZaj"), (VarName "var1", BoolVal False), (VarName "var3", IntVal 11479), (VarName "var7", StringVal "#r<1ZXUF\rVl#3:uKnt?J,c}I"), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 15), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal True), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 3615)],
    fromList [(VarName "var0", StringVal ""), (VarName "var1", BoolVal True), (VarName "var3", IntVal 11381), (VarName "var7", StringVal "lBmgz.M&|OF>OsSV?N^%"), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 3), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal False), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 10188)],
    fromList [(VarName "var0", StringVal "2P0+njt>mis?.T-nT4{R?LU\\Lrj&i*NB%C"), (VarName "var1", BoolVal False), (VarName "var3", IntVal 1628), (VarName "var7", StringVal "!|a%xBb5V%I9YX/RGJD_+%7a$giEBF}p,q_"), (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))), IntVal 9), (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1")))), BoolVal False), (Dot "table63" (Dot "table31" (Dot "table15" (Dot "table7" (Dot "table3" (VarName "var1"))))), IntVal 9178)]
  ]
