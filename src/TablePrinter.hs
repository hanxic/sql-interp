module TablePrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import GenSQL
import SQLPrinter qualified as SPP
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, punctuate, render, (<+>))
import Text.PrettyPrint qualified as PP

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (SPP.PP a) => a -> String
pretty = render . SPP.pp

-------- Helper function --------
-- This sections contains a few helper function for more refined printing
hasWhiteSpace :: String -> Bool
hasWhiteSpace = foldr ((||) . Char.isSpace) False

hasComma :: String -> Bool
hasComma = foldr ((||) . (== ',')) False

-- >>> hasWhiteSpace "c d"
-- True

-------- Printing Table --------

-- | Printing dvalue
ppDVal :: DValue -> Doc
ppDVal (IntVal i) = SPP.pp i
ppDVal (BoolVal b) = SPP.pp b
ppDVal NullVal = PP.text "NULL"
ppDVal (StringVal s) =
  let ppRegularString = PP.quotes $ PP.text s
   in if not (null s)
        && not (hasWhiteSpace s)
        && not (hasComma s)
        && notElem s reservedKeyWords
        then PP.text s
        else ppRegularString

-- A string value, in order to not print the double quote, needs to be not null,
-- does not have white space, does not have comma, and not a reserved keywords.
-- Otherwise it must have double quotation around it.

-- | A row is a list of dvalue separated by comma
instance SPP.PP Row where
  pp :: Row -> Doc
  pp r = PP.cat $ PP.punctuate PP.comma (map (ppDVal . snd) $ Map.toList r)

-- | Printing the primary keys
ppPrimaryKeys :: PrimaryKeys -> Doc
ppPrimaryKeys pk = ppLineCSV SPP.pp $ map fst $ NE.toList pk

-- | Printing one line of csv
ppLineCSV :: (a -> Doc) -> [a] -> Doc
ppLineCSV p xs = PP.hcat $ PP.punctuate PP.comma (map p xs)

-- | Printing the indexname
ppIndexName :: IndexName -> Doc
ppIndexName iName = PP.hcat $ PP.punctuate PP.comma (map (SPP.pp . fst) iName)

-- | Printing a line
ppLine :: Doc -> Doc
ppLine d = d <> PP.text "\n"

-- | Printing a row
ppRow :: AnnotatedHeader -> Row -> Doc
ppRow iName row =
  PP.hcat $
    PP.punctuate
      PP.comma
      ( map
          ( \k ->
              ppDVal $ Map.findWithDefault (StringVal "") (fst k) row
          )
          iName
      )

-- | Printing header
ppHeader :: Header -> Doc
ppHeader = ppLineCSV SPP.pp

-- | Printing a table
instance SPP.PP Table where
  pp (Table pk iName td) =
    let indices = NE.toList pk ++ iName
     in ppLine
          ( ppPrimaryKeys pk
              <> if null iName then PP.empty else PP.comma <> ppIndexName iName
          ) -- Printing the header
          <> PP.hcat (PP.punctuate (PP.text "\n") (map (ppRow indices) td))

-- Then print the rows

-- | Print out the scope of the interpreter
ppScope :: Scope -> Doc
ppScope scope =
  let tntList = Map.toList scope
   in PP.hcat (PP.punctuate (PP.text "\n") (map ppScopeAux tntList))
  where
    ppScopeAux :: (TableName, Table) -> Doc
    ppScopeAux (tn, table) =
      ppHLine
        <> SPP.pp tn
        <> ppHLine
        <> PP.text "\n"
        <> SPP.pp table
        <> PP.text "\n"
        <> ppHLine
        <> SPP.pp tn
        <> ppHLine
    ppHLine :: Doc
    ppHLine = PP.text "--------"

-- | Printing the alias map in the interpreter
ppAlias :: Alias -> Doc
ppAlias alias =
  let tntnList = Map.toList alias
   in PP.text "++++++++"
        <> PP.text "ALIAS"
        <> PP.text "++++++++"
        <> PP.text "\n"
        <> PP.hcat (PP.punctuate (PP.text "\n") (map ppAliasAux tntnList))
  where
    ppAliasAux :: (TableName, TableName) -> Doc
    ppAliasAux (tn1, tn2) = PP.text tn1 <+> PP.text "->" <+> PP.text tn2

-- | Printing the store of the interpreter, which is just scope + alias
instance SPP.PP Store where
  pp (Store scope alias) = ppScope scope <> PP.text "\n" <> ppAlias alias

-- | Printing the store into string for IO
printStore :: Store -> String
printStore = pretty
