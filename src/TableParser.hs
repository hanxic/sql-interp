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
import Utils

type PrimaryKeysList = IndexName

nameTP :: Parser String
nameTP = P.filter isValidName (some baseP <* spacesP)
  where
    baseP :: Parser Char
    baseP = P.choice [P.alpha, P.digit, P.underscore]
    isValidName :: String -> Bool
    isValidName [] = True
    isValidName str@(x : _) = notElem str reservedKeyWords && not (Char.isDigit x)

varTP :: Parser Var
varTP =
  (Dot <$> tpFVarName <*> (P.char '.' *> varTP)) <* spacesP
    <|> (VarName <$> tpFVarName)
    <|> (VarName <$> SP.wsP (SP.stringInP '\"' many))

tpFVarName :: Parser String
tpFVarName =
  P.filter
    (`notElem` reservedKeyWords)
    nameTP

-- String name for column should be not empty)

headerP :: Parser Header
headerP = P.sepBy varTP P.comma

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

stringValTP :: Parser DValue
stringValTP =
  ( StringVal <$> (SP.stringInP '\'' many <* spacesP)
      <|> ( StringVal
              <$> some (P.satisfy (\x -> x /= ',' && not (Char.isSpace x)))
          )
  )
    <* spacesP

boolValTP :: Parser DValue
boolValTP = (trueP <|> falseP) <* spacesP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ P.string "TRUE"
    falseP :: Parser DValue
    falseP = BoolVal False <$ P.string "FALSE"

intValTP :: Parser DValue
intValTP = IntVal <$> P.int

dvalueTP :: DType -> Parser DValue
dvalueTP (IntType i) = (intValTP <* separatorP) <|> nullValTP
dvalueTP BoolType = (boolValTP <* separatorP) <|> nullValTP
dvalueTP (StringType n) = nullValTP <|> (stringValTP <* separatorP)

{- (intValTP <* separatorP) <|> (boolValTP <* separatorP) <|> nullValTP <|> stringValTP -}

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

validHeaderP :: PrimaryKeys -> IndexName -> Parser AnnotatedHeader
validHeaderP pkVarList iNameVarList = headerP >>= validHeaderPAux pkVarList iNameVarList

validHeaderPAux :: PrimaryKeys -> IndexName -> Header -> Parser AnnotatedHeader
validHeaderPAux pk iName header =
  if not $ checkRepetitive header && length header == length pk + length iName
    then maybe Control.Applicative.empty return (annotateHeaderMaybe pk iName header)
    else Control.Applicative.empty

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
    [x@(_, dtype)] -> validValP x row =<< (dvalueTP dtype <* spacesP <* P.lookAhead (void newLineP <|> P.eof))
    (x@(_, dtype) : xs) -> do
      dvalue <- dvalueTP dtype <* SP.wsP P.comma
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
  ah <- SP.wsP (validHeaderP pk iName)
  guard (not $ null ah)
  tableData <- ([] <$ (many P.space *> P.eof)) <|> P.sepBy (rowP ah <* spacesP) newLineP
  return (Table pk iName tableData)

parseCSVFile :: PrimaryKeys -> IndexName -> String -> IO (Either P.ParseError Table)
parseCSVFile pk iName = P.parseFromFile (const <$> tableP pk iName <*> P.eof)
