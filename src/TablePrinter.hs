module TablePrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
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

-- >>> noWhiteSpace "c d"
-- False

ppDVal :: DValue -> Doc
ppDVal (IntVal i) = SPP.pp i
ppDVal (BoolVal b) = SPP.pp b
ppDVal NullVal = PP.text "NULL"
ppDVal (StringVal s) =
  let ppRegularString = PP.text ("\'" <> s <> "\'")
   in if not $ hasWhiteSpace s && notElem s reservedKeyWords then PP.text s else ppRegularString

instance SPP.PP Row where
  pp r = PP.cat $ PP.punctuate PP.comma (map (ppDVal . snd) $ Map.toList r)

test_ppRow :: Test
test_ppRow =
  TestList
    [ pretty (Map.fromList [("a", StringVal "c"), ("b", IntVal 2)]) ~?= "c,2",
      pretty (Map.fromList [("a", BoolVal True), ("b", IntVal 2), ("c", StringVal "c")]) ~?= "TRUE,2,c",
      pretty (Map.fromList [("a", BoolVal True), ("b", IntVal 2), ("c", StringVal "c d")]) ~?= "TRUE,2,'c d'",
      pretty (Map.fromList [("a", BoolVal True), ("b", IntVal 2), ("c", StringVal "NULL")]) ~?= "TRUE,2,NULL",
      pretty (Map.fromList [("a", StringVal "Hello"), ("b", StringVal "THERE")]) ~?= "Hello,THERE"
    ]

test :: Row -> [Doc]
test r = map (SPP.pp . snd) $ Map.toList r

ppPrimaryKeys :: PrimaryKeys -> Doc
ppPrimaryKeys pk = ppLineCSV SPP.pp $ map fst $ NE.toList pk

test_ppPrimaryKeys :: Test
test_ppPrimaryKeys =
  TestList
    [ render (ppPrimaryKeys (NE.fromList [("a", StringType 255), ("b", IntType 32), ("c", BoolType)])) ~?= "a,b,c"
    ]

ppLineCSV :: (a -> Doc) -> [a] -> Doc
ppLineCSV p xs = PP.cat $ PP.punctuate PP.comma (map p xs)

ppIndexName :: IndexName -> Doc
ppIndexName = ppLineCSV SPP.pp . map fst

ppLine :: Doc -> Doc
ppLine d = d <> PP.text "\n"

ppRow :: IndexName -> Row -> Doc
ppRow iName row = ppLineCSV ppDVal (map (\k -> Map.findWithDefault (StringVal "") (fst k) row) iName)

instance SPP.PP Table where
  pp (Table pk iName td) =
    let indices = NE.toList pk ++ iName
     in ppLine (ppPrimaryKeys pk <> PP.comma <> ppIndexName iName)
          <> PP.cat
            (map (ppLine . ppRow indices) td)

test_ppTable :: Test
test_ppTable =
  TestList
    [ pretty
        ( Table
            (NE.fromList [("a", StringType 255), ("b", BoolType)])
            [("c", IntType 32), ("d", BoolType)]
            [ Map.fromList
                [("a", StringVal "hello"), ("b", BoolVal True), ("c", IntVal 255), ("d", BoolVal False), ("e", StringVal "Not a part")]
            ]
        )
        ~?= "a,b,c,d\nhello,TRUE,255,FALSE\n"
    ]
