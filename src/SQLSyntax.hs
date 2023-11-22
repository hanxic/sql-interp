module SQLSyntax where

import Test.QuickCheck as QC
import Test.QuickCheck qualified as QC

{-

Commands to Add Later:
  - Alter Table and [add, drop]
  - Update and [set]
  - Create and [table, index, database]
  - Having for GroupBy clauses
  - Joins

Grammar Source: https://www.dataquest.io/blog/sql-commands/
Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/
Relational Algebra: https://byjus.com/gate/relational-algebra-in-dbms-notes/#:~:text=Calculus%20(or%20DRC)-,What%20is%20Relational%20Algebra%20in%20DBMS%3F,unary%20operator%20can%20be%20used
Equivalence Rules: https://www.postgresql.org/message-id/attachment/32513/EquivalenceRules.pdf

Wyatt Jobs:
  - Read up on LuStepper, relational algebra, sql query optimization
  - Given command, update the TableMap
  - Define TableMap and ops [insert, append, singleton, empty, delete, search?]
  - Stretch loose optimization stuff
      - remove duplicates
      - search for col name -> if name never shows up delete col
      - parallelize if col ops are independent of other ops
      - flatten nested select ops

Timeline:
  - TA Checkin #1 wed-fri?
      - parser
      - TableMap datastructure
      - interpretation
      - TESTING for everything (arbitrary + some unit tests)
      - optimization definitions??

-}
data Command = C
  { verb :: Verb, -- none empty
    from :: TableName,
    wh :: Maybe ConditionalStatement
  }
  deriving (Eq, Show)

-- Will make the following size assumptions about table to reduce time
--   - Tables have max five columns
--   - Tables have max twenty rows

instance Arbitrary Command where
  arbitrary :: Gen Command
  arbitrary = do
    v <- (arbitrary :: Gen Verb)
    f <- (arbitrary :: Gen TableName)
    wh <- (arbitrary :: Gen ConditionalStatement)
    return (C v f (Just wh))

  shrink :: Command -> [Command]
  shrink c = undefined

data Verb
  = Select [FieldName]
  | Delete [Expression]
  | Drop
  | Update [Expression]
  | Create [FieldName]

--  | AlterTable [AlterTableCommand]

instance Arbitrary Verb where
  arbitrary :: Gen Verb
  arbitrary = QC.oneof [arbitraryVerbWExp, arbitraryVerbWField]
    where
      arbitraryVerbWField :: Gen Verb
      arbitraryVerbWField = do
        v <- QC.elements [Select, Create]
        xs <- QC.listOf (arbitrary :: Gen FieldName)
        return (v xs)

      arbitraryVerbWExp :: Gen Verb
      arbitraryVerbWExp = do
        v <- QC.elements [Delete, Update]
        xs <- QC.listOf (arbitrary :: Gen Expression)
        return (v xs)

  shrink :: Verb -> [Verb]
  shrink (Select xs) = map Select (shrink xs)
  shrink (Delete xs) = map Delete (shrink xs)
  shrink (Update xs) = map Update (shrink xs)
  shrink (Create xs) = map Create (shrink xs)
  shrink _ = []

-- data AlterTableCommand = Add | Drop

data DType
  = StringType
  | IntType
  | BoolType
  | NullType
  deriving (Eq, Ord)

instance Arbitrary DType where
  arbitrary :: Gen DType
  arbitrary = QC.elements [StringType, IntType, BoolType, NullType]

  shrink :: DType -> [DType]
  shrink _ = []

type TableName = String

newtype FieldName = FieldName (String, DType) deriving (Eq, Ord)

instance Arbitrary FieldName where
  arbitrary :: Gen FieldName
  arbitrary = do
    s <- QC.elements ["a", "b", "c", "d", "e"]
    d <- arbitrary :: Gen DType
    return $ FieldName (s, d)

  shrink :: FieldName -> [FieldName]
  shrink _ = []

data VerbStatement
  = Distinct Expression
  | Into Expression
  | Expr Expression
  | As Expression Expression
  | Set FieldName Expression
  deriving (Eq, Show)

data AggregateFunction
  = Count
  | Sum
  | Min
  | Max
  | Mean
  | Median
  deriving (Eq, Show, Enum)

data Order = Asc | Desc
  deriving (Eq, Show)

data ConditionalStatement
  = Where
  | OrderBy Expression Order
  | Top Expression
  | Offset Expression
  | Fetch Expression

instance Arbitrary ConditionalStatement where
  arbitrary :: Gen ConditionalStatement
  arbitrary = undefined

  shrink :: ConditionalStatement -> [ConditionalStatement]
  shrink _ = []

--  | GroupBy
--  | Join

data Expression
  = Value Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq, Show)

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = undefined

  shrink :: Expression -> [Expression]
  shrink c = undefined

data Uop
  = Neg
  | Not
  | Len
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Gt
  | Ge
  | Lt
  | Le
  | Concat
  | And
  | Or
  | Like
  deriving (Eq, Show, Enum, Bounded)

data Top
  = Between
  deriving (Eq, Show)

data Vop
  = In
  deriving (Eq, Show)

data Value
  = IntVal Int
  | BoolVal Bool
  | StringVal String
  | Null
  deriving (Eq)

{-
What do we want to cover?
SELECT [expression]
FROM expression
WHERE
ORDER BY
GROUP BY
HAVING
JOIN
-}

{-
Must have a select, and must have a from
The rest are optional...Applicative
How about Data definition statement
+ Manipulation statement
+ Use the exception transformer stufff
-}
