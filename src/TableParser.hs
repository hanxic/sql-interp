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
import TableSyntax
  ( AnnotatedHeader,
    Header,
    IndexName,
    PrimaryKeys,
    Row,
    Table (Table, indexName, primaryKeys, tableData),
  )
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, render)
import Utils

type PrimaryKeysList = IndexName

-- | Parsing name in table. The name should not read extra white space
nameTP :: Parser String
nameTP = P.filter isValidName (some baseP <* spacesP)
  where
    baseP :: Parser Char
    baseP = P.choice [P.alpha, P.digit, P.underscore]
    isValidName :: String -> Bool
    isValidName [] = True
    isValidName str@(x : _) =
      notElem str reservedKeyWords
        && not (Char.isDigit x)

-- | Parsing variable. The variable can be double quoted
varTP :: Parser Var
varTP =
  (Dot <$> tpFVarName <*> (P.char '.' *> varTP)) <* spacesP
    <|> (VarName <$> tpFVarName)
    <|> (VarName <$> SP.wsP (SP.stringInP '\"' many))

-- | Helper function for parsing variable name (i.e. to check that the name is
-- not a reserved keyword)
tpFVarName :: Parser String
tpFVarName =
  P.filter
    (`notElem` reservedKeyWords)
    nameTP

-- | Parsing a header (which may be invalid)
headerP :: Parser Header
headerP = P.sepBy varTP P.comma

-- | An extra reserved keyword in table in comma
reservedTKeywords :: [String]
reservedTKeywords = [","]

-- | Separating space in normal parser to two disjoint sets the one that gives a
-- new line and the one that does not
spacesP :: Parser [Char]
spacesP = many (P.char ' ' <|> P.char '\t' <|> P.char '\v')

newLineP :: Parser Char
newLineP = P.char '\n' <|> P.char '\r' <|> P.char '\f'

-- | Parsing Null. A null can be either NULL or empty space between two comma,
-- following with a comma, or following by the end of file. In addition, for not
-- stepping into infinite loop, the parser should not consume the new line, as
-- suggested in the following example
nullValTP :: Parser DValue
nullValTP =
  NullVal <$ (P.string "NULL" <* P.lookAhead (spacesP *> (P.eof <|> void (newLineP <|> P.comma))))
    <|> NullVal <$ P.lookAhead (SP.wsP P.comma <|> spacesP *> P.wsbP newLineP)
    <|> NullVal <$ P.lookAhead (spacesP *> P.wsbP P.eof)

-- >>> P.doParse nullValTP "\t\n"
-- Just (NullVal,"\t\n")

-- | Parsing a string value, which can be either a single quoted string or a
-- name
stringValTP :: Parser DValue
stringValTP =
  ( StringVal <$> (SP.stringInP '\'' many <* spacesP)
      <|> ( StringVal
              <$> some (P.satisfy (\x -> x /= ',' && not (Char.isSpace x)))
          )
  )
    <* spacesP

-- | Parsing a boolean value. The only difference is not consuming white space
-- that step to next line
boolValTP :: Parser DValue
boolValTP = (trueP <|> falseP) <* spacesP
  where
    trueP :: Parser DValue
    trueP = BoolVal True <$ P.string "TRUE"
    falseP :: Parser DValue
    falseP = BoolVal False <$ P.string "FALSE"

-- | Parsing an integer value. The only difference is not consuming white space
-- that step to next line
intValTP :: Parser DValue
intValTP = IntVal <$> P.int

-- | Parsing a data value. The only difference is not consuming white space
-- that step to next line
dvalueTP :: DType -> Parser DValue
dvalueTP (IntType i) = (intValTP <* separatorP) <|> nullValTP
dvalueTP BoolType = (boolValTP <* separatorP) <|> nullValTP
dvalueTP (StringType n) = nullValTP <|> (stringValTP <* separatorP)

-- | Separator for CSV is comma (duh!) or end of line
separatorP :: Parser ()
separatorP = void (P.lookAhead (P.space <|> P.comma)) <|> P.lookAhead P.eof

-- | Parse a header and check its validity
validHeaderP :: PrimaryKeys -> IndexName -> Parser AnnotatedHeader
validHeaderP pkVarList iNameVarList =
  headerP
    >>= validHeaderPAux pkVarList iNameVarList

-- | A header is valid if the header is a permutation of primary key and index
-- name combined
validHeaderPAux :: PrimaryKeys -> IndexName -> Header -> Parser AnnotatedHeader
validHeaderPAux pk iName header =
  if not $ checkRepetitive header && length header == length pk + length iName
    then
      maybe
        Control.Applicative.empty
        return
        (annotateHeaderMaybe pk iName header)
    else Control.Applicative.empty

-- | Try to annotated the header with types
annotateHeaderMaybe ::
  PrimaryKeys ->
  IndexName ->
  Header ->
  Maybe AnnotatedHeader
annotateHeaderMaybe pk iName header =
  let pkList = NE.toList pk
   in foldr
        ( \x acc ->
            (:)
              <$> annotateColumnMaybe pkList iName x
              <*> acc
        )
        (Just [])
        header

-- | Helper function in annotating header
annotateColumnMaybe ::
  [(Var, DType)] ->
  [(Var, DType)] ->
  Var ->
  Maybe (Var, DType)
annotateColumnMaybe pkList iName headerVar =
  case lookup headerVar pkList of
    Just dtyp -> Just (headerVar, dtyp)
    Nothing ->
      case lookup headerVar iName of
        Just dtyp2 -> Just (headerVar, dtyp2)
        Nothing -> Nothing

-- | A helper function in checking repetitive. This function assume that the
-- list is sorted An alternative is to use group by, in which we will have to
-- iterate through list twice than one time here
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

-- | Given an annotated header, parse a row
rowP :: AnnotatedHeader -> Parser Row
rowP ah = rowPAux ah Map.empty

-- | Parsing an row based on annotated header. The function handles edge cases
-- like the end of file or end of line It will also check whether the header is
-- valid by checking the type of the values
rowPAux :: AnnotatedHeader -> Row -> Parser Row
rowPAux ah row =
  case ah of
    [] -> row <$ some spacesP
    [x@(_, dtype)] ->
      validValP x row
        =<< (dvalueTP dtype <* spacesP <* P.lookAhead (void newLineP <|> P.eof))
    (x@(_, dtype) : xs) -> do
      dvalue <- dvalueTP dtype <* SP.wsP P.comma
      row' <- validValP x row dvalue
      rowPAux xs row'
  where
    validValP :: (Var, DType) -> Row -> DValue -> Parser Row
    validValP (var, dtype) row dvalue =
      if dvalueTypeCheck dvalue dtype
        then return $ Map.insert var dvalue row
        else
          maybe
            Control.Applicative.empty
            (\x -> return $ Map.insert var x row)
            $ convertMaybe dvalue dtype

-- | The function will try to convert between values
convertMaybe :: DValue -> DType -> Maybe DValue
convertMaybe (IntVal i) (StringType n)
  | n >= length (show i) =
      Just $ StringVal $ show i
convertMaybe (BoolVal b) (StringType n)
  | n >= 1 =
      Just $ StringVal $ show b
convertMaybe _ _ = Nothing

-- | Given the signature of the table, parsing an entire table
tableP :: PrimaryKeys -> IndexName -> Parser Table
tableP pk iName = do
  ah <- SP.wsP (validHeaderP pk iName)
  guard (not $ null ah)
  tableData <-
    ([] <$ (many P.space *> P.eof))
      <|> P.sepBy (rowP ah <* spacesP) newLineP
  return (Table pk iName tableData)

-------- Support for IO --------
parseCSVFile ::
  PrimaryKeys ->
  IndexName ->
  String ->
  IO (Either P.ParseError Table)
parseCSVFile pk iName = P.parseFromFile (const <$> tableP pk iName <*> P.eof)
