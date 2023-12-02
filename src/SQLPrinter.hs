module SQLPrinter where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard
-- formatting rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

oneLine :: (PP a) => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Bool where
  pp True = PP.text "TRUE"
  pp False = PP.text "FALSE"

instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

instance PP VerbAlter where
  pp :: VerbAlter -> Doc
  pp va =
    case va of
      Add b ->
        PP.text "ADD"
          <+> if b
            then PP.text "IF NOT EXISTS"
            else PP.empty
      DropColumn b ->
        PP.text "Drop Column"
          <+> if b
            then PP.text "IF EXISTS"
            else PP.empty

instance (PP a) => PP (Maybe a) where
  pp Nothing = PP.empty
  pp (Just a) = pp a

instance PP OrderTypeFL where
  pp NULLSFIRST = PP.text "NULLS FIRST"
  pp NULLSLAST = PP.text "NULLS LAST"

instance PP OrderTypeAD where
  pp :: OrderTypeAD -> Doc
  pp = PP.text . show

instance PP DValue where
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (StringVal s) = PP.text ("\'" <> s <> "\'")
  pp NullVal = PP.text "NULL"

instance PP DType where
  pp (StringType i) = PP.text $ "VARCHAR(" <> show i <> ")"
  pp (IntType i) = PP.text "INTEGER"
  pp BoolType = PP.text "BIT(1)"

instance PP Function where
  {-   pp (Avg cs e) = PP.text "AVG" <+> PP.parens (pp cs <+> pp e)
    pp (Count cs e) = PP.text "COUNT" <+> PP.parens (pp cs <+> pp e)
    pp (Max cs e) = PP.text "MAX" <+> PP.parens (pp cs <+> pp e)
    pp (Min cs e) = PP.text "MIN" <+> PP.parens (pp cs <+> pp e)
    pp (Sum cs e) = PP.text "SUM" <+> PP.parens (pp cs <+> pp e) -}
  pp Len = PP.text "LENGTH"
  pp Lower = PP.text "LOWER"
  pp Upper = PP.text "UPPER"

instance PP AggFunction where
  pp Avg = PP.text "AVG"
  pp Count = PP.text "COUNT"
  pp Max = PP.text "MAX"
  pp Min = PP.text "MIN"
  pp Sum = PP.text "SUM"

instance PP Uop where
  pp Not = PP.text "NOT"
  pp Neg = PP.char '-'

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.text "//"
  pp Modulo = PP.text "%"
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "="
  pp And = PP.text "AND"
  pp Or = PP.text "OR"
  pp Like = PP.text "LIKE"
  pp Is = PP.text "IS"

instance PP Var where
  pp (VarName name) = PP.text name
  pp (QuotedName name) = PP.text ("\"" <> name <> "\"")
  pp AllVar = PP.char '*'

instance PP Expression where
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2 {} = ppPrec 0 e
    where
      ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else Prelude.id
  pp (Fun f exp) =
    pp f <> PP.parens (pp exp)
  pp (AggFun f cs exp) =
    pp f <> PP.parens (pp cs <+> pp exp)

instance PP JoinStyle where
  pp LeftJoin = PP.text "LEFT JOIN"
  pp RightJoin = PP.text "RIGHT JOIN"
  pp InnerJoin = PP.text "JOIN"
  pp OuterJoin = PP.text "OUTER JOIN"

-- >>> pp LeftJoin
-- LEFT JOIN

isBaseFromExpression :: FromExpression -> Bool
isBaseFromExpression (Table _) = True
isBaseFromExpression _ = False

instance PP FromExpression where
  pp (Table texp) = pp texp
  {- pp (TableAlias texp var) = pp texp <+> pp var -}
  pp (SubQuery sc) = PP.parens $ ppSelectCommandAux sc
  pp (Join fexp1 js fexp2) =
    let ppExp1 =
          if isBaseFromExpression fexp1
            then pp fexp1
            else PP.parens $ pp fexp1
     in let ppExp2 =
              if isBaseFromExpression fexp2
                then pp fexp2
                else PP.parens $ pp fexp2
         in ppExp1 <+> pp js <+> ppExp2

instance PP CountStyle where
  pp Distinct = PP.text "DISTINCT"
  pp All = PP.empty

instance PP ColumnExpression where
  pp (ColumnName exp) = pp exp
  pp (ColumnAlias exp v) = pp exp <+> PP.text "AS" <+> pp v

isBaseTableExpression :: ColumnExpression -> Bool
isBaseTableExpression (ColumnName e) = isBase e
isBaseTableExpression (ColumnAlias e _) = isBase e

ppNewLine :: Doc
ppNewLine = PP.char '\n'

ppFullQuery :: Doc -> Doc
ppFullQuery d = d <> PP.semi

ppList :: Doc -> [Doc] -> Doc
ppList p l = PP.hsep $ PP.punctuate p l

ppSelectCommandAux :: SelectCommand -> Doc
ppSelectCommandAux (SelectCommand exprs sf swh gb ob li o) =
  ppList
    ppNewLine
    $ filter
      (/= PP.empty)
      [ PP.text "SELECT" <+> ppList PP.comma (map ppSE exprs),
        PP.text "FROM" <+> pp sf,
        if null swh
          then PP.empty
          else PP.text "WHERE" <+> pp swh,
        if null gb
          then PP.empty
          else PP.text "GROUP BY" <+> ppList PP.comma (map pp gb),
        if null ob
          then PP.empty
          else PP.text "ORDER BY" <+> ppList PP.comma (map ppOB ob),
        if null li
          then PP.empty
          else PP.text "LIMIT" <+> pp li,
        if null o
          then PP.empty
          else PP.text "OFFSET" <+> pp o
      ]

ppSE :: (CountStyle, ColumnExpression) -> Doc
ppSE (cs, te@(ColumnName _)) =
  pp cs <+> if isBaseTableExpression te then pp te else PP.parens $ pp te
ppSE (cs, te@(ColumnAlias e v)) =
  pp cs <+> if isBaseTableExpression te then pp te else PP.parens (pp e) <+> PP.text "AS" <+> pp v

test_ppSE :: Test
test_ppSE =
  TestList
    [ ppSE (Distinct, ColumnAlias (Op2 (Fun Len (Val (StringVal "1k"))) Minus (Var (QuotedName "Var1"))) (VarName "Var0")) ~?= PP.text "DISTINCT (LENGTH('1k') - \"Var1\") AS Var0"
    ]

ppOB :: (Var, Maybe OrderTypeAD, Maybe OrderTypeFL) -> Doc
ppOB (v, o1, o2) =
  pp v <+> pp o1 <+> pp o2

instance PP SelectCommand where
  pp = ppFullQuery . ppSelectCommandAux

instance PP CreateCommand where
  pp (CreateCommand name ids) =
    ppFullQuery $
      PP.text "CREATE TABLE"
        <+> pp name
        <+> PP.parens (ppList PP.comma $ map (\(n, i) -> pp n <+> pp i) ids)

instance PP DeleteCommand where
  pp (DeleteCommand fr wh) =
    ppFullQuery $
      PP.text
        "DELETE FROM"
        <+> pp fr
        <+> PP.text "WHERE"
        <+> pp wh -- Probably need a nested here

instance PP UpsertIntoCommand where
  pp :: UpsertIntoCommand -> Doc
  pp (UpsertIntoCommand fr mayCN vals) =
    let cnDoc = case mayCN of
          Nothing -> PP.empty
          Just cn -> PP.parens $ ppList PP.comma $ map pp cn
     in ppFullQuery $
          PP.text "UPSERT INTO"
            <+> (pp fr <> cnDoc)
            <+> (PP.text "VALUES" <> PP.parens (ppList PP.comma $ map pp vals))

instance PP AlterTableCommand where
  pp (AlterTableCommand fr ve co) =
    PP.text "ALTER TABLE" <+> pp fr <+> pp ve <+> pp co --- TODO: This is buggy

-- TODO: Add nested to make sure not over 80 words
