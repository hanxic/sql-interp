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
import Utils

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard
-- formatting rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

-- Render the printing in one line
oneLine :: (PP a) => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

-- Print a new line
ppNewLine :: Doc
ppNewLine = PP.char '\n'

-------- Printing SQL --------

-- | Printing Instance for Boolean
instance PP Bool where
  pp True = PP.text "TRUE"
  pp False = PP.text "FALSE"

-- | Printing Instance for String
instance PP String where
  pp = PP.text

-- | Printing Instance for Integer
instance PP Int where
  pp = PP.int

-- | Printing Instance for Alter verb
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

-- | Printing for maybe
instance (PP a) => PP (Maybe a) where
  pp Nothing = PP.empty
  pp (Just a) = pp a

-- | Printing for Null order parameter for order by clause
instance PP OrderTypeFL where
  pp NULLSFIRST = PP.text "NULLS FIRST"
  pp NULLSLAST = PP.text "NULLS LAST"

-- | Printing for ordering for order by clause
instance PP OrderTypeAD where
  pp :: OrderTypeAD -> Doc
  pp = PP.text . show

-- | Printing for dvalue
instance PP DValue where
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (StringVal s) = PP.quotes $ PP.text s
  pp NullVal = PP.text "NULL"

-- | Printing for dtype
instance PP DType where
  pp :: DType -> Doc
  pp (StringType i) = PP.text $ "VARCHAR(" <> show i <> ")"
  pp (IntType i)
    | i == 16 = PP.text "INTEGER"
    | i == 32 = PP.text "BIGINT"
  pp (IntType i) = PP.text "INT" <> PP.parens (pp i)
  pp BoolType = PP.text "BOOLEAN"

-- | Printing for function signature
instance PP Function where
  pp :: Function -> Doc
  pp Len = PP.text "LENGTH"
  pp Lower = PP.text "LOWER"
  pp Upper = PP.text "UPPER"

-- | Printing for aggregate function signature
instance PP AggFunction where
  pp :: AggFunction -> Doc
  pp Avg = PP.text "AVG"
  pp Count = PP.text "COUNT"
  pp Max = PP.text "MAX"
  pp Min = PP.text "MIN"
  pp Sum = PP.text "SUM"

-- | Printing for Unary operator
instance PP Uop where
  pp :: Uop -> Doc
  pp Not = PP.text "NOT"
  pp Neg = PP.char '-'

-- | Printing for binary operator
instance PP Bop where
  pp :: Bop -> Doc
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

-- | Printing for expression, the expression output will be annotated for any
-- parentheses necessary
instance PP Expression where
  pp :: Expression -> Doc
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

-- | Printinng for join style
instance PP JoinStyle where
  pp :: JoinStyle -> Doc
  pp LeftJoin = PP.text "LEFT JOIN"
  pp RightJoin = PP.text "RIGHT JOIN"
  pp InnerJoin = PP.text "JOIN"
  pp OuterJoin = PP.text "OUTER JOIN"

-- | Printing for variable
instance PP Var where
  pp :: Var -> Doc
  pp (VarName name) = PP.text name
  pp (Dot n v) = pp n <> PP.char '.' <> pp v

-- | Check the name that the from expression is referring to
getTableName :: FromExpression -> TableName
getTableName (TableRef tname) = tname
getTableName _ = ""

-- | Print the join names into several equality
renderJoinNames :: JoinNames -> Doc
renderJoinNames =
  PP.hcat
    . PP.punctuate PP.comma
    . map (\(n1, n2) -> pp n1 <> PP.text "=" <> pp n2)

-- | Check if there is a base
isBaseFromExpression :: FromExpression -> Bool
isBaseFromExpression (TableRef _) = True
isBaseFromExpression _ = False

-- | Printing from clause
instance PP FromExpression where
  pp :: FromExpression -> Doc
  pp (TableRef texp) = pp texp
  pp (TableAlias texp var) = pp texp <+> PP.text "AS" <+> pp var
  pp (SubQuery sc) = PP.parens $ PP.nest 2 $ ppSelectCommandAux sc
  pp (Join fexp1 js fexp2 jns) =
    let ppExp1 =
          if isBaseFromExpression fexp1
            then pp fexp1
            else PP.parens $ pp fexp1
     in let ppExp2 =
              if isBaseFromExpression fexp2
                then pp fexp2
                else PP.parens $ pp fexp2
         in let ppJns = if null jns then PP.empty else PP.text "ON" <+> renderJoinNames jns -- Render join names
             in ppExp1 <+> pp js <+> ppExp2 <+> ppJns

-- | Printing for count
instance PP CountStyle where
  pp :: CountStyle -> Doc
  pp Distinct = PP.text "DISTINCT"
  pp All = PP.empty

-- | Print column expression
instance PP ColumnExpression where
  pp :: ColumnExpression -> Doc
  pp (ColumnName exp) = pp exp
  pp (ColumnAlias exp v) = pp exp <+> PP.text "AS" <+> pp v
  pp AllVar = PP.text "*"

-- | Print the base expression for table
isBaseTableExpression :: ColumnExpression -> Bool
isBaseTableExpression (ColumnName e) = isBase e
isBaseTableExpression (ColumnAlias e _) = isBase e
isBaseTableExpression _ = True

-- A full query also requires a semi column
ppFullQuery :: Doc -> Doc
ppFullQuery d = d <> PP.semi

-- Parse a list with punctuate
ppList :: Doc -> [Doc] -> Doc
ppList p l = PP.hsep $ PP.punctuate p l

-- | Printing for SelectCommand
ppSelectCommandAux :: SelectCommand -> Doc
ppSelectCommandAux (SelectCommand (cs, ce) sf swh gb ob li o) =
  ppList
    ppNewLine
    $ filter
      (/= PP.empty)
      [ PP.text "SELECT"
          <+> (if cs == Distinct then PP.text "DISTINCT" else PP.empty)
          <+> ppList PP.comma (map ppSE ce),
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

-- | Printing the expressions in select clauses
ppSE :: ColumnExpression -> Doc
ppSE te@(ColumnName _) =
  if isBaseTableExpression te then pp te else PP.parens $ pp te
ppSE te@(ColumnAlias e v) =
  if isBaseTableExpression te
    then pp te
    else PP.parens (pp e) <+> PP.text "AS" <+> pp v
ppSE AllVar =
  pp AllVar

-- | Printing order parameter
ppOB :: (Var, Maybe OrderTypeAD, Maybe OrderTypeFL) -> Doc
ppOB (v, o1, o2) =
  pp v <+> pp o1 <+> pp o2

-- | Printing Select Command
instance PP SelectCommand where
  pp :: SelectCommand -> Doc
  pp = ppFullQuery . ppSelectCommandAux

-- | Printing Create Command
instance PP CreateCommand where
  pp :: CreateCommand -> Doc
  pp (CreateCommand ine name ids) =
    ppFullQuery $
      PP.text "CREATE TABLE"
        <+> ( if ine -- Prefix
                then PP.text "IF NOT EXISTS"
                else PP.empty
            )
        <+> pp name -- Name of the table
        <+> PP.parens -- parameters inside
          ( ppList PP.comma $
              map
                ( \(n, t, pk) ->
                    pp n
                      <+> pp t
                      <+> (if pk then PP.text "PRIMARY KEY" else PP.empty)
                )
                ids
          )

-- | Printing delete command
instance PP DeleteCommand where
  pp :: DeleteCommand -> Doc
  pp (DeleteCommand fr wh) =
    ppFullQuery $
      PP.text
        "DELETE FROM"
        <+> pp fr
        <+> if null wh
          then PP.empty
          else
            PP.text "WHERE"
              <+> pp wh

-- | Printing the upsert instruction
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

-- | Printing the alternate table instruction
instance PP AlterTableCommand where
  pp :: AlterTableCommand -> Doc
  pp (AlterTableCommand fr ve co) =
    PP.text "ALTER TABLE" <+> pp fr <+> pp ve <+> pp co

-- | Printing a random query
instance PP Query where
  pp :: Query -> Doc
  pp (SelectQuery sc) = pp sc
  pp (DeleteQuery dc) = pp dc
  pp (CreateQuery cc) = pp cc

-------- Helper for IO --------
prettyPrintQueries :: Queries -> Doc
prettyPrintQueries qs = PP.hsep (map pp qs)

printQueries :: Queries -> String
printQueries = PP.render . prettyPrintQueries
