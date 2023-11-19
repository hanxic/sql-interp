module SQLSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence.Internal.Sorting (Queue (Q))
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

data Query
  = SelectQuery SelectCommand
  | DeleteQuery DeleteCommand

-- **** Section for DeleteCommand ****

data DeleteCommand = DeleteCommand
  { from :: FromExpression,
    wh :: Maybe [Expression]
  }

-- **** Section for CreateCommand ****

data CreateCommand = CreateCommand
  { name :: TableExpression,
    id :: [Var]
    -- TODO: Haven't finished
  }

-- **** Section for SelectCommand ****

data SelectCommand = SelectCommand
  { exprs :: [(ColumnStyle, TableExpression)],
    selectFrom :: FromExpression,
    selectWh :: Maybe [Expression],
    groupby :: Maybe [Expression],
    orderby :: Maybe [Expression]
  }
  deriving (Eq, Show)

data DType
  = StringType
  | IntType
  | BoolType
  | NullType
  deriving (Eq, Show)

data DValue
  = IntVal Int
  | BoolVal Bool
  | StringVal String
  | Null
  deriving (Eq, Show, Ord)

type Name = String

data FromExpression
  = TableExpression Expression
  | SubQuery SelectCommand
  | Join JoinStyle SelectCommand SelectCommand
  deriving (Eq, Show)

data JoinStyle
  = LeftJoin
  | RightJoin
  | InnerJoin
  | OuterJoin
  deriving (Eq, Show, Enum, Bounded)

data TableExpression
  = TableName Expression
  | TableAlias Expression Var
  deriving (Eq, Show)

data ColumnStyle
  = Distinct
  | All
  deriving (Eq, Show)

data OrderType
  = ASC
  | DESC
  | NULL
  | FIRST
  | LAST
  deriving (Eq, Show)

data Command
  = Select
  | Create
  | Drop
  deriving (Eq, Show)

data Uop
  = Not
  | Neg
  deriving (Eq, Show)

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
  | And
  | Or
  | Like
  deriving (Eq, Show, Enum, Bounded)

data Expression
  = Var Var
  | Value DValue
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  | Fun Function Expression
  | Order Expression OrderType
  deriving (Eq, Show)

data Var
  = Name Name -- Does not quoted, Must start from an alphabet and follow by int or alphabet
  | QuotedName Name -- Quoted, can be anything
  deriving (Eq, Show)

data Function
  = Avg
  | Count
  | Max
  | Min
  | Sum
  | Len
  | Lower
  | Upper
  deriving (Eq, Show)

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