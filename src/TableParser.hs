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
import TableSyntax (Header, IndexName, PrimaryKeys, Row, Table)
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

test5 =
  [ (VarName "var0", IntType 1),
    (VarName "var1", BoolType),
    (VarName "var2", IntType 3),
    (VarName "var5", StringType 81),
    (Dot "table10" (Dot "table5" (Dot "table2" (VarName "var1"))), BoolType),
    (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2"))), BoolType),
    (Dot "table43" (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2")))), BoolType),
    (Dot "table86" (Dot "table43" (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2"))))), IntType 20)
  ]

test1 = QC.sample $ genRowFromAH test5

test2 =
  fromList
    [ (VarName "var0", IntVal 2),
      (VarName "var1", BoolVal True),
      (VarName "var2", NullVal),
      (VarName "var5", StringVal "0"),
      (Dot "table10" (Dot "table5" (Dot "table2" (VarName "var1"))), BoolVal True),
      (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2"))), BoolVal True),
      (Dot "table43" (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2")))), BoolVal True),
      (Dot "table86" (Dot "table43" (Dot "table21" (Dot "table10" (Dot "table5" (VarName "var2"))))), IntVal 691747)
    ]

-- >>> dvalueTypeCheck NullVal BoolType
-- True

-- >>> TP.ppRow test5 test2
-- 2,TRUE,NULL,0,TRUE,TRUE,TRUE,691747

test4 = "NULL,NULL,TRUE,*jlz"

test3 = P.doParse (rowP test5) test4

-- >>> test3
-- Just (fromList [(VarName "var0",NullVal),(VarName "var1",NullVal),(VarName "var5",StringVal "*jlz"),(Dot "table2" (VarName "var1"),BoolVal True)],"")

test6 = "NULL,NULL,TRUE,*jlz"

test7 = P.doParse (rowP [(VarName "var0", BoolType), (VarName "var1", IntType 20), (Dot "table2" (VarName "var1"), BoolType), (VarName "var5", StringType 92)]) test6

-- >>> test7
-- Just (fromList [(VarName "var0",NullVal),(VarName "var1",NullVal),(VarName "var5",StringVal "*jlz"),(Dot "table2" (VarName "var1"),BoolVal True)],"")

-- >>> P.doParse (dvalueTP) test6
-- Just (NullVal,",NULL,*jlz,TRUE")

-- >>> P.doParse (dvalueTP) test4
-- Just (IntVal 55,",105732,TRUE")

prop_roundtrip_table :: Table -> Bool
prop_roundtrip_table t = P.parse tableP (TP.pretty t) == Right t

type PrimaryKeysList = IndexName

headerP :: Parser Header
headerP = P.sepBy SP.varP P.comma

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

dvalueTP :: Parser DValue
dvalueTP = (P.intValP <* separatorP) <|> (P.boolValP <* separatorP) <|> nullValTP <|> stringValTP
  where
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
    [] -> row <$ many P.space
    [x@(_, dtype)] -> validValP x row =<< SP.wsP dvalueTP
    (x@(_, dtype) : xs) -> do
      dvalue <- SP.wsP dvalueTP <* SP.wsP P.comma
      row' <- validValP x row dvalue
      rowPAux xs row'
  where
    validValP :: (Var, DType) -> Row -> DValue -> Parser Row
    validValP (var, dtype) row dvalue =
      if dvalueTypeCheck dvalue dtype
        then return $ Map.insert var dvalue row
        else maybe Control.Applicative.empty (\x -> return $ Map.insert var x row) $ convertMaybe dvalue dtype

convertMaybe :: DValue -> DType -> Maybe DValue
convertMaybe (IntVal i) (StringType n) | n >= finiteBitSize i = Just $ StringVal $ show i
convertMaybe (BoolVal b) (StringType n)
  | n >= 1 =
      Just $ StringVal $ show b
convertMaybe _ _ = Nothing

tableP :: Parser Table
tableP = undefined
