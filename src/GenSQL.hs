module GenSQL where

import Control.Monad (liftM2, liftM3, mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

{- Arbitrary instances for nontrivial types -}
instance Arbitrary DValue where
  arbitrary =
    QC.frequency
      [ (1, IntVal <$> arbitrary),
        (1, BoolVal <$> arbitrary),
        (1, StringVal <$> arbitrary),
        (1, pure NullVal)
      ]

instance Arbitrary DType where
  arbitrary =
    QC.frequency
      [ (1, StringType <$> arbitrary),
        (1, IntType <$> arbitrary),
        (1, pure BoolType)
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

instance Arbitrary Var where
  arbitrary =
    QC.frequency
      [ (1, Name <$> (QC.elements =<< genNamePool)),
        (1, QuotedName <$> (QC.elements =<< genNamePool)),
        (1, pure AllVar)
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
  QC.frequency [(n, StringVal <$> (arbitrary `QC.suchThat` (\s -> length s <= i))), (1, pure NullVal)]
genValTC BoolType = do
  n <- genPos
  QC.frequency
    [(n, BoolVal <$> arbitrary), (1, pure NullVal)]

inferFunctionType :: Function -> Gen DType
inferFunctionType f =
  case f of
    Avg -> genIntType
    Count -> genIntType
    Max -> genIntType
    Min -> genIntType
    Sum -> genIntType
    _ -> genStringType
  where
    genIntType :: Gen DType
    genIntType = IntType <$> QC.chooseInt (1, 32) -- No Bool type
    genStringType :: Gen DType
    genStringType = return $ StringType 255 --- Temporary value

genExp :: Int -> Gen Expression
genExp n
  | n <= 0 =
      QC.frequency [(1, Var <$> arbitrary), (1, Val <$> arbitrary)]
genExp n =
  QC.frequency
    [ (1, genExp 0),
      (n, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
      (n, genFunExp)
    ]
  where
    n' = n `div` 2
    genFunExp :: Gen Expression -- Make sure generate dvalue such that the function type check
    genFunExp =
      arbitrary
        >>= ( \f ->
                Fun f
                  <$> arbitrary
                  <*> QC.frequency
                    [(1, Var <$> arbitrary), (1, Val <$> (inferFunctionType f >>= genValTC))]
            )

instance Arbitrary Expression where
  arbitrary = QC.sized genExp

instance Arbitrary FromExpression where
  arbitrary =
    QC.frequency
      [ (20, Table <$> (QC.elements =<< genTablePool)),
        (1, SubQuery <$> arbitrary),
        (20, Join <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Arbitrary ColumnExpression where
  arbitrary =
    QC.frequency
      [ (1, ColumnName <$> arbitrary),
        (1, ColumnAlias <$> arbitrary <*> arbitrary) -- This will cause some problem if alias is something that is invalid
      ]

instance Arbitrary SelectCommand where
  arbitrary = SelectCommand <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

{- Arbitrary bounded enum instances -}
instance Arbitrary OrderTypeFL where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary OrderTypeAD where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Function where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary JoinStyle where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary CountStyle where
  arbitrary = QC.arbitraryBoundedEnum