module GenSQL where

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

-- | Generate a string literal, being careful about the characters that it may contain

{- genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> notElem c SQLSyntax.reservedChar && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~'] -}

genTest :: Gen String
genTest = return "'"

escape' = filter (`notElem` reservedChar)

test1000 = escape' <$> genTest

genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    escape :: String -> String
    escape = filter (`notElem` reservedChar)
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> Char.isSpace c || Char.isPrint c) ['\NUL' .. '~']

{- Arbitrary instances for nontrivial types -}
instance Arbitrary DValue where
  arbitrary =
    QC.frequency
      [ (1, IntVal <$> arbitrary),
        (1, BoolVal <$> arbitrary),
        (1, StringVal <$> genStringLit),
        (1, pure NullVal)
      ]

maxSize :: Int
maxSize = 5

genNamePool :: Gen [Name]
genNamePool = do
  i <- QC.chooseInt (1, maxSize)
  return ["Var" ++ show j | j <- [0, i]]

genTablePool :: Gen [TableName]
genTablePool = do
  i <- QC.chooseInt (1, maxSize)
  return ["Table" ++ show j | j <- [0, i]]

{- genVarWOAllVar :: Gen Var
genVarWOAllVar =
  QC.frequency
    [ (1, VarName <$> (QC.elements =<< genNamePool)),
      (1, Dot <$> (QC.elements =<< genTablePool) <*> (QC.elements =<< genNamePool))
    ] -}

instance Arbitrary Var where
  arbitrary =
    QC.frequency
      [ (1, VarName <$> (QC.elements =<< genNamePool)),
        (1, Dot <$> (QC.elements =<< genTablePool) <*> (QC.elements =<< genNamePool))
      ]

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

inferAggFunctionType :: AggFunction -> Gen DType
inferAggFunctionType f =
  genIntType -- TODO: Add more types when more functions are added

inferFunctionType :: Function -> Gen DType
inferFunctionType f =
  genStringType -- TODO: Add more types when more functions are added

genIntType :: Gen DType
genIntType = IntType <$> QC.chooseInt (1, 32) -- No Bool type

genStringType :: Gen DType
genStringType = StringType <$> QC.chooseInt (1, 255) --- Temporary value

instance Arbitrary DType where
  arbitrary =
    QC.frequency
      [ (1, genStringType),
        (1, genIntType),
        (1, pure BoolType)
      ]

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
    genFun :: Gen Expression
    genFun =
      arbitrary
        >>= ( \f ->
                Fun f
                  <$> (inferFunctionType f >>= genExpTC)
            )
    genAggFun :: Gen Expression
    genAggFun =
      arbitrary
        >>= ( \f ->
                AggFun f
                  <$> arbitrary
                  <*> (inferAggFunctionType f >>= genExpTC)
            )
    genExpTC :: DType -> Gen Expression
    genExpTC t =
      QC.frequency
        [(1, Var <$> arbitrary), (1, Val <$> genValTC t)]

instance Arbitrary Expression where
  arbitrary = QC.sized genExp

-- Scratch paper for function instance
{- let genExpTC =
      (=<<)
        ( \t ->
            QC.frequency
              [(1, Var <$> arbitrary), (1, Val <$> genValTC t)]
        )
 in QC.frequency
      [ (1, Avg <$> arbitrary <*> genExpTC genIntType),
        (1, Count <$> arbitrary <*> genExpTC genIntType),
        (1, Max <$> arbitrary <*> genExpTC genIntType),
        (1, Min <$> arbitrary <*> genExpTC genIntType),
        (1, Sum <$> arbitrary <*> genExpTC genIntType),
        (1, Len <$> genExpTC genStringType),
        (1, Lower <$> genExpTC genStringType),
        (1, Upper <$> genExpTC genStringType)
      ] -}

genFromExpression :: Int -> Gen FromExpression
genFromExpression n | n <= 0 = TableRef <$> (QC.elements =<< genTablePool)
genFromExpression n =
  QC.frequency
    [ (n, TableRef <$> (QC.elements =<< genTablePool)),
      (n, Join <$> genFromExpression n' <*> arbitrary <*> genFromExpression n' <*> QC.sized (`constrainSize` arbitrary))
    ]
  where
    n' = n `div` 2

{-       (1, SubQuery <$> arbitrary),-}
instance Arbitrary FromExpression where
  arbitrary = do
    n <- QC.sized (\x -> QC.chooseInt (1, x))
    {- QC.frequency [(n, QC.sized genFromExpression), (5, SubQuery <$> arbitrary)] -}
    QC.sized genFromExpression

instance Arbitrary ColumnExpression where
  arbitrary =
    QC.frequency
      [ (1, ColumnName <$> (arbitrary >>= patchWVar)),
        (1, ColumnAlias <$> (arbitrary >>= patchWVar) <*> (QC.elements =<< genTablePool)), -- This will cause some problem if alias is something that is invalid
        (1, return AllVar)
      ]

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

constrainSize1 :: Int -> Gen a -> Gen [a]
constrainSize1 n g | n <= 0 = (: []) <$> g
constrainSize1 n g = do
  x <- g
  xs <- constrainSize1 n' g
  return $ x : xs
  where
    n' = n `div` 2

constrainSize :: Int -> Gen a -> Gen [a]
constrainSize n _ | n <= 0 = return []
constrainSize n g = do
  x <- g
  xs <- constrainSize n' g
  return $ x : xs
  where
    n' = n `div` 2

genSelectCommand :: Int -> Gen SelectCommand
genSelectCommand n =
  SelectCommand
    <$> atLeastN 1 arbitrary
    <*> genFromExpression n'
    <*> arbitrary
    <*> QC.sized (`constrainSize` arbitrary)
    <*> QC.sized (`constrainSize` arbitrary)
    <*> arbitrary
    <*> arbitrary
  where
    n' = n `div` 2

test232 = atLeastN 1 (arbitrary :: Gen (CountStyle, ColumnExpression))

-- >>> QC.sample test232

test17 = show [1, 2, 3]

-- >>> test17
-- "[1,2,3]"

instance Arbitrary SelectCommand where
  arbitrary =
    QC.sized genSelectCommand

atLeastN :: Int -> Gen a -> Gen [a]
atLeastN i g = do
  n <- QC.sized (\n -> QC.chooseInt (i, n))
  constrainSize1 n g

instance Arbitrary CreateCommand where
  arbitrary =
    CreateCommand
      <$> arbitrary
      <*> (QC.elements =<< genTablePool)
      <*> QC.sized
        ( `constrainSize1`
            ( (,,,)
                <$> (QC.elements =<< genNamePool)
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
            )
        )

instance Arbitrary DeleteCommand where
  arbitrary =
    DeleteCommand
      <$> (QC.elements =<< genTablePool)
      <*> arbitrary

{- Generate Table -}

instance Arbitrary IndexName where
  arbitrary = NE.fromList <$> atLeastN 1 ((,) <$> (QC.elements =<< genNamePool) <*> arbitrary)

instance Arbitrary Table where
  arbitrary = Table <$> arbitrary <*> arbitrary

instance Arbitrary Store where
  arbitrary = Store <$> arbitrary <*> arbitrary

{- Arbitrary bounded enum instances -}
instance Arbitrary OrderTypeFL where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary OrderTypeAD where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Function where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary AggFunction where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary JoinStyle where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary CountStyle where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary IndexAttribute where
  arbitrary = QC.arbitraryBoundedEnum
