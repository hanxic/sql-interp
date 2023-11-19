module SQLSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

{-

Commands to Add Later:
  - Alter Table and [add, drop]
  - Update and [set]
  - Create and [table, index, database]
  - Having for GroupBy clauses
  - Joins

Awesome grammar source: https://forcedotcom.github.io/phoenix/
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

type Name = String

data Command = Command
  { verb :: Verb, -- none empty
    from :: Command,
    wh :: ConditionalStatement
  }
  deriving (Eq, Show)

data Verb
  = Select [Expression]
  | Insert [Expression]
  | Delete
  | Drop
  deriving (Eq, Show)

--  | Create [FieldName]
--  | Update [Expression]
--  | AlterTable [AlterTableCommand]

-- data AlterTableCommand = Add | Drop

data DType
  = StringType
  | IntType
  | BoolType
  | NullType
  deriving (Eq, Show)

type TableName = String

type FieldName = (String, DType)

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
  = GroupBy
  | Join
  | OrderBy Expression Order
  | Top Expression
  | Offset Expression
  | Fetch Expression
  deriving (Eq, Show)

data Expression
  = Var Var
  | Value Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq, Show)

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

type Var = Name

data Value
  = IntVal Int
  | BoolVal Bool
  | StringVal String
  | Null
  deriving (Eq, Show, Ord)

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