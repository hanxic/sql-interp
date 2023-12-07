module TableParser where

import Control.Applicative
import Control.Monad
import Data.Char qualified as Char
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map as Map (Map, fromList)
import GHC.Generics (D)
import GenSQL
import Parser (Parser)
import Parser qualified as P
import SQLParser qualified as SP
import SQLSyntax
import TablePrinter qualified as TP
import TableSyntax (Header, IndexName, PrimaryKeys, Row, Table)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, render)

prop_roundtrip_header :: Header -> Bool
prop_roundtrip_header h = P.parse headerP (Text.PrettyPrint.render $ TP.ppHeader h) == Right h

{- prop_roundtrip_row :: Row -> Bool
prop_roundtrip_row r = P.parse rowP (TP.pretty r) == Right r -}

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

type AnnotatedHeader = IndexName

validHeaderP :: PrimaryKeys -> IndexName -> Parser AnnotatedHeader
validHeaderP pkVarList iNameVarList = headerP >>= validHeaderPAux pkVarList iNameVarList

validHeaderPAux :: PrimaryKeys -> IndexName -> Header -> Parser AnnotatedHeader
validHeaderPAux pk iName header =
  if not $ checkRepetitive header && length header == length pk + length iName
    then maybe empty return (annotateHeaderMaybe pk iName header)
    else empty
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
rowP = undefined {- P.sepBy SP.dvalueP P.comma -}

tableP :: Parser Table
tableP = undefined
