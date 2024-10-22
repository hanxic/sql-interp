module GenSQL where

{- GenSQL Module contains the QuickCheck generators for testing SQLParser,
SQLPrinter, TablePrinter, and TableParser. This file requires the knowledge of
SQLSyntax and TableSyntax-}

import Control.Monad (liftM2, liftM3, mapM_, replicateM)
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import TableSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Utils (inferAggFunctionType, inferFunctionType)

-- | A function that substitute QuickCheck's default instance for generating
-- string The string literal generator ensures no "incorrect" strings (i.e., the
-- string is not one of the reserved characters or contains any unprintable
-- characters)
genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    escape :: String -> String
    escape = filter (`notElem` reservedChar)
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> Char.isSpace c || Char.isPrint c) ['\NUL' .. '~']

-- | Arbitrary instance for generating DValue
instance Arbitrary DValue where
  arbitrary :: Gen DValue
  arbitrary =
    -- Frequency can be changed for stress-testing
    QC.frequency
      [ (1, IntVal <$> arbitrary),
        (1, BoolVal <$> arbitrary),
        (1, StringVal <$> genStringLit),
        (1, pure NullVal)
      ]

-- | A parameter on the maximum size of any table count
maxSize :: Int
maxSize = 5

-- | Generating random set of variable names, from 1 to maximum size
genNamePool :: Gen [Name]
genNamePool = do
  i <- QC.chooseInt (1, maxSize)
  return ["Var" ++ show j | j <- [0, i]]

-- | Generating random set of table names, from 1 to maximum size
genTablePool :: Gen [TableName]
genTablePool = do
  i <- QC.chooseInt (1, maxSize)
  return ["Table" ++ show j | j <- [0, i]]

-- | A helper function for generating random variable with the given integer
genVar :: Int -> Gen Var
genVar n = return $ VarName ("var" ++ show n)

-- | Arbitrary instance for generating variable
instance Arbitrary Var where
  arbitrary :: Gen Var
  arbitrary = do
    n <- QC.sized (\x -> QC.chooseInt (1, x))
    QC.sized genVar

-- | Generate positive integer
genPos :: Gen Int
genPos = abs <$> arbitrary `QC.suchThat` (> 0)

-- | Helper function that generate value that are typed checked
genValTC :: DType -> Gen DValue
genValTC (IntType i) = do
  n <- genPos
  QC.frequency [(n, IntVal <$> QC.chooseInt (1, 2 ^ i)), (1, pure NullVal)]
genValTC (StringType i) = do
  n <- genPos
  QC.frequency [(n, StringVal <$> (genStringLit `QC.suchThat` (\s -> length s <= i))), (1, pure NullVal)]
genValTC BoolType = do
  n <- genPos
  QC.frequency
    [(n, BoolVal <$> arbitrary), (1, pure NullVal)]

-- | Generating integer type
genIntType :: Gen DType
genIntType = IntType <$> QC.chooseInt (1, 32) -- No Bool type

-- | Generating string type
genStringType :: Gen DType
genStringType = StringType <$> QC.chooseInt (1, 255) --- Temporary value

-- | Arbitrary instance for generating dtype
instance Arbitrary DType where
  arbitrary :: Gen DType
  arbitrary =
    QC.frequency
      [ (1, genStringType),
        (1, genIntType),
        (1, pure BoolType)
      ]

-- | Generate expression within a specific size
genExp :: Int -> Gen Expression
genExp n
  | n <= 0 =
      QC.frequency [(1, Var <$> arbitrary), (1, Val <$> arbitrary)]
genExp n =
  QC.frequency
    [ (1, genExp 0),
      (n, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
      (n, genFun),
      (n, genAggFun)
    ]
  where
    n' = n `div` 2
    -- Need to first infer the function type and generate the needed expression
    genFun :: Gen Expression
    genFun =
      arbitrary
        >>= ( \f ->
                Fun f <$> genExpTC (inferFunctionType f)
            )
    -- Need to first infer the aggregate function type and generate the needed expression
    genAggFun :: Gen Expression
    genAggFun =
      arbitrary
        >>= ( \f ->
                AggFun f
                  <$> arbitrary
                  <*> genExpTC (inferAggFunctionType f)
            )
    -- Base case
    genExpTC :: DType -> Gen Expression
    genExpTC t =
      QC.frequency
        [(1, Var <$> arbitrary), (1, Val <$> genValTC t)]

-- | Arbitrary instance for expression
instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = QC.sized genExp

-- | Generator for from expression
genFromExpression :: Int -> Gen FromExpression
genFromExpression n | n <= 0 = TableRef <$> (QC.elements =<< genTablePool)
genFromExpression n =
  QC.frequency
    [ (n, TableRef <$> (QC.elements =<< genTablePool)),
      (n, Join <$> genFromExpression n' <*> arbitrary <*> genFromExpression n' <*> QC.sized (`constrainSize` arbitrary))
    ]
  where
    n' = n `div` 2

-- | Arbitrary instance for from expression
instance Arbitrary FromExpression where
  arbitrary :: Gen FromExpression
  arbitrary = do
    n <- QC.sized (\x -> QC.chooseInt (1, x))
    QC.sized genFromExpression

-- | Arbitrary instances for column expression
instance Arbitrary ColumnExpression where
  arbitrary :: Gen ColumnExpression
  arbitrary =
    QC.frequency
      [ (1, ColumnName <$> (arbitrary >>= patchWVar)),
        (1, ColumnAlias <$> (arbitrary >>= patchWVar) <*> (QC.elements =<< genTablePool)),
        (1, return AllVar)
      ]

-- | A patching function to make sure that there exists some variable in the provided expression
patchWVar :: Expression -> Gen Expression
patchWVar e@(Var _) = return e
patchWVar (Val _) = Var <$> arbitrary
patchWVar (Op1 u e) = patchWVar e
patchWVar (Op2 e1 u e2) =
  arbitrary
    >>= ( \b ->
            if b
              then Op2 <$> patchWVar e1 <*> return u <*> return e2
              else Op2 e1 u <$> patchWVar e2
        )
patchWVar (AggFun af cs e) = AggFun af cs <$> patchWVar e
patchWVar (Fun f e) = Fun f <$> patchWVar e

-- | Generating SelectCommand
genSelectCommand :: Int -> Gen SelectCommand
genSelectCommand n =
  SelectCommand
    <$> ((,) <$> arbitrary <*> atLeastN 1 arbitrary)
    <*> genFromExpression n'
    <*> arbitrary
    <*> QC.sized (`constrainSize` arbitrary)
    <*> QC.sized (`constrainSize` arbitrary)
    <*> arbitrary
    <*> arbitrary
  where
    n' = n `div` 2

-- | Arbitrary instance for SelectCommand
instance Arbitrary SelectCommand where
  arbitrary :: Gen SelectCommand
  arbitrary =
    QC.sized genSelectCommand

-- | Arbitrary instance for CreateCommand
instance Arbitrary CreateCommand where
  arbitrary :: Gen CreateCommand
  arbitrary =
    CreateCommand
      <$> arbitrary
      <*> (QC.elements =<< genTablePool)
      <*> QC.sized
        ( `constrainSize1`
            ( (,,)
                <$> (QC.elements =<< genNamePool)
                <*> arbitrary
                <*> arbitrary
            )
        )

-- | Arbitrary instance for DeleteCommand
instance Arbitrary DeleteCommand where
  arbitrary :: Gen DeleteCommand
  arbitrary =
    DeleteCommand
      <$> (QC.elements =<< genTablePool)
      <*> arbitrary

-- | Arbitrary instance for Query
instance Arbitrary Query where
  arbitrary :: Gen Query
  arbitrary =
    QC.frequency
      [ (1, SelectQuery <$> arbitrary),
        (1, CreateQuery <$> arbitrary),
        (1, DeleteQuery <$> arbitrary)
      ]

---------- Table Generator --------

-- | Given an annotated header (column name, type), generate a row
genRowFromAH :: AnnotatedHeader -> Gen Row
genRowFromAH [] = return Map.empty
genRowFromAH ((var, dtype) : xs) = do
  dvalue <- genValTC dtype
  rest <- genRowFromAH xs
  return (Map.singleton var dvalue `Map.union` rest)

-- | Generate a list of variable and type as the non-primary keys header of the table
genIndexName :: Gen [(Var, DType)]
genIndexName =
  QC.sized
    ( `constrainSize1`
        ( (,)
            <$> arbitrary
            <*> arbitrary
        )
    )

-- | Generate an annotated
genAH :: Gen AnnotatedHeader
genAH = reverse <$> QC.sized genAHAux
  where
    genAHAux :: Int -> Gen AnnotatedHeader
    genAHAux n | n <= 0 = (\x y -> [(x, y)]) <$> genVar n <*> arbitrary
    genAHAux n = do
      ah' <- genAHAux (n `div` 2)
      dtype <- arbitrary
      var <- genVar n
      return $ (var, dtype) : ah'

-- | Generate the primary keys and index pair
genPKIN :: Gen (PrimaryKeys, IndexName)
genPKIN = do
  ah <- genAH
  i <- QC.chooseInt (1, length ah)
  let (pkList, iName) = splitAt i ah
   in return (NE.fromList pkList, iName)

-- | Given annotated header, generate a table of values with relevant types and values
genTableData :: AnnotatedHeader -> Gen TableData
genTableData ah = QC.sized $ genTableDataAux ah
  where
    genTableDataAux :: AnnotatedHeader -> Int -> Gen [Row]
    genTableDataAux ah n | n <= 0 = return []
    genTableDataAux ah n = do
      row <- genRowFromAH ah
      rest <- genTableDataAux ah (n `div` 2)
      return $ row : rest

-- | Arbitrary instance for generating primary keys (deprecated)
instance Arbitrary PrimaryKeys where
  arbitrary :: Gen PrimaryKeys
  arbitrary = do
    v <- arbitrary :: Gen Var
    t <- arbitrary :: Gen DType
    return $ NE.singleton (v, t)

-- | Arbitrary instance for generating table (deprecated)
instance Arbitrary Table where
  arbitrary :: Gen Table
  arbitrary = Table <$> arbitrary <*> genIndexName <*> arbitrary

-- | Arbitrary instance for generating Store
instance Arbitrary Store where
  arbitrary :: Gen Store
  arbitrary = Store <$> arbitrary <*> arbitrary

--------- Arbitrary bounded enum instances --------
instance Arbitrary OrderTypeFL where
  arbitrary :: Gen OrderTypeFL
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary OrderTypeAD where
  arbitrary :: Gen OrderTypeAD
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Function where
  arbitrary :: Gen Function
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary AggFunction where
  arbitrary :: Gen AggFunction
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Uop where
  arbitrary :: Gen Uop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary JoinStyle where
  arbitrary :: Gen JoinStyle
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary CountStyle where
  arbitrary :: Gen CountStyle
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary IndexAttribute where
  arbitrary :: Gen IndexAttribute
  arbitrary = QC.arbitraryBoundedEnum

-------- Customized Generator --------

-- | A helper function that will generate at most 64 pieces of data (greater than zero)
constrainSize1 :: Int -> Gen a -> Gen [a]
constrainSize1 n g | n <= 0 = (: []) <$> g
constrainSize1 n g = do
  x <- g
  xs <- constrainSize1 n' g
  return $ x : xs
  where
    n' = n `div` 2

-- | A helper function that will generate at most 64 pieces of data (potentially 0)
constrainSize :: Int -> Gen a -> Gen [a]
constrainSize n _ | n <= 0 = return []
constrainSize n g = do
  x <- g
  xs <- constrainSize n' g
  return $ x : xs
  where
    n' = n `div` 2

-- | A helper function for generate at least N (different) copies of data
atLeastN :: Int -> Gen a -> Gen [a]
atLeastN i g = do
  n <- QC.sized (\n -> QC.chooseInt (i, n))
  constrainSize1 n g