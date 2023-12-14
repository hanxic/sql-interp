module TablePrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import GenSQL (AnnotatedHeader)
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

hasWhiteSpace :: String -> Bool
hasWhiteSpace = foldr ((||) . Char.isSpace) False

hasComma :: String -> Bool
hasComma = foldr ((||) . (== ',')) False

-- >>> hasWhiteSpace "c d"
-- True

ppDVal :: DValue -> Doc
ppDVal (IntVal i) = SPP.pp i
ppDVal (BoolVal b) = SPP.pp b
ppDVal NullVal = PP.text "NULL"
ppDVal (StringVal s) =
  let ppRegularString = PP.quotes $ PP.text s
   in if not (null s) && not (hasWhiteSpace s) && not (hasComma s) && notElem s reservedKeyWords then PP.text s else ppRegularString

instance SPP.PP Row where
  pp :: Row -> Doc
  pp r = PP.cat $ PP.punctuate PP.comma (map (ppDVal . snd) $ Map.toList r)

test_ppRow :: Test
test_ppRow =
  TestList
    [ pretty (Map.fromList [(VarName "a", StringVal "c"), (VarName "b", IntVal 2)]) ~?= "c,2",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "c")]) ~?= "TRUE,2,c",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "c d")]) ~?= "TRUE,2,'c d'",
      pretty (Map.fromList [(VarName "a", BoolVal True), (VarName "b", IntVal 2), (VarName "c", StringVal "NULL")]) ~?= "TRUE,2,NULL",
      pretty (Map.fromList [(VarName "a", StringVal "Hello"), (VarName "b", StringVal "THERE")]) ~?= "Hello,THERE"
    ]

test :: Row -> [Doc]
test r = map (SPP.pp . snd) $ Map.toList r

ppPrimaryKeys :: PrimaryKeys -> Doc
ppPrimaryKeys pk = ppLineCSV SPP.pp $ map fst $ NE.toList pk

test_ppPrimaryKeys :: Test
test_ppPrimaryKeys =
  TestList
    [ render (ppPrimaryKeys (NE.fromList [(VarName "a", StringType 255), (VarName "b", IntType 32), (VarName "c", BoolType)])) ~?= "a,b,c"
    ]

ppLineCSV :: (a -> Doc) -> [a] -> Doc
ppLineCSV p xs = PP.hcat $ PP.punctuate PP.comma (map p xs)

ppIndexName :: IndexName -> Doc
ppIndexName iName = PP.hcat $ PP.punctuate PP.comma (map (SPP.pp . fst) iName)

ppLine :: Doc -> Doc
ppLine d = d <> PP.text "\n"

ppRow :: AnnotatedHeader -> Row -> Doc
ppRow iName row = PP.hcat $ PP.punctuate PP.comma (map (\k -> ppDVal $ Map.findWithDefault (StringVal "") (fst k) row) iName)

{-
ppRow :: AnnotatedHeader -> Row -> Doc
ppRow [] row = PP.empty
ppRow [x] row = maybe PP.empty ppDVal (Map.lookup x row)
ppRow (x : xs) row =
  case Map.lookup x row of
    Just dval -> ppDVal <> PP.comma <> ppRow xs row
    Nothing -> PP.empty-}

ppHeader :: Header -> Doc
ppHeader = ppLineCSV SPP.pp

instance SPP.PP Table where
  pp (Table pk iName td) =
    let indices = NE.toList pk ++ iName
     in ppLine (ppPrimaryKeys pk <> if null iName then PP.empty else PP.comma <> ppIndexName iName)
          <> PP.hcat (PP.punctuate (PP.text "\n") (map (ppRow indices) td))

ppScope :: Scope -> Doc
ppScope scope =
  let tntList = Map.toList scope
   in PP.hcat (PP.punctuate (PP.text "\n") (map ppScopeAux tntList))
  where
    ppScopeAux :: (TableName, Table) -> Doc
    ppScopeAux (tn, table) = ppHLine <> SPP.pp tn <> ppHLine <> PP.text "\n" <> SPP.pp table <> PP.text "\n" <> ppHLine <> SPP.pp tn <> ppHLine
    ppHLine :: Doc
    ppHLine = PP.text "--------"

ppAlias :: Alias -> Doc
ppAlias alias =
  let tntnList = Map.toList alias
   in PP.text "++++++++" <> PP.text "ALIAS" <> PP.text "++++++++" <> PP.text "\n" <> PP.hcat (PP.punctuate (PP.text "\n") (map ppAliasAux tntnList))
  where
    ppAliasAux :: (TableName, TableName) -> Doc
    ppAliasAux (tn1, tn2) = PP.text tn1 <+> PP.text "->" <+> PP.text tn2

instance SPP.PP Store where
  pp (Store scope alias) = ppScope scope <> PP.text "\n" <> ppAlias alias

printStore :: Store -> String
printStore = pretty

{- PP.cat
  (map (ppLine . ppRow indices) td) -}

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
        ~?= "a,b,c,d\nhello,TRUE,255,FALSE\nhello,TRUE,255,FALSE\n"
    ]

-- >>> runTestTT test_ppTable
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
