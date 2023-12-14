{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module SQLSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.List (concat)
import Data.List qualified as Listå
import Data.Map
import Data.Sequence.Internal.Sorting (Queue (Q))
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

{-
SELECT [expression]
FROM expression
WHERE
ORDER BY
GROUP BY
HAVING
JOIN
-}

data Query
  = SelectQuery SelectCommand
  | DeleteQuery DeleteCommand
  | CreateQuery CreateCommand
  deriving (Eq, Show)

type Queries = [Query]

-- **** Section for DeleteCommand ****

{- Awesome grammar source: https://forcedotcom.github.io/phoenix/
Grammar Source: https://www.dataquest.io/blog/sql-commands/
Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/
Relational Algebra: https://byjus.com/gate/relational-algebra-in-dbms-notes/#:~:text=Calculus%20(or%20DRC)-,What%20is%20Relational%20Algebra%20in%20DBMS%3F,unary%20operator%20can%20be%20used
Equivalence Rules: https://www.postgresql.org/message-id/attachment/32513/EquivalenceRules.pdf -}

-- Need to add offset into the syntax

-- ******** Construct for SQL Syntax ********

data ColumnExpression
  = ColumnName Expression -- e.g. SELECT A / SELECT (A * 2) / SELECT SUM(A) / SELECT SUM(2)
  | ColumnAlias Expression Name -- e.g. SELECT A AS B
  | AllVar
  deriving (Eq, Show)

data CountStyle
  = Distinct
  | All
  deriving (Eq, Show, Enum, Bounded, Ord)

type TableName = String

data FromExpression
  = TableRef TableName -- e.g. FROM TEST
  | TableAlias TableName TableName -- e.g. FROM A AS B
  | SubQuery SelectCommand -- e.g. FROM (SELECT ...)
  | Join FromExpression JoinStyle FromExpression JoinNames -- e.g. FROM A JOIN B
  deriving (Eq, Show)

data JoinStyle
  = LeftJoin
  | RightJoin
  | InnerJoin
  | OuterJoin
  deriving (Eq, Show, Enum, Bounded)

type JoinNames = [(Var, Var)]

data Expression
  = Var Var -- e.g. A
  | Val DValue
  | Op1 Uop Expression -- e.g. NOT A
  | Op2 Expression Bop Expression -- e.g. A + 2
  | AggFun AggFunction CountStyle Expression -- e.g. SUM / AVG
  | Fun Function Expression
  deriving (Eq, Show, Ord)

data Var
  = VarName Name
  | Dot Name Var
  deriving (Eq, Show, Ord)

data Uop
  = Not
  | Neg
  deriving (Eq, Show, Enum, Bounded, Ord)

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
  | Is
  deriving (Eq, Show, Enum, Bounded, Ord)

data AggFunction
  = Avg
  | Count
  | Max
  | Min
  | Sum
  deriving (Eq, Show, Enum, Bounded, Ord)

data Function
  = Len
  | Lower
  | Upper
  deriving (Eq, Show, Enum, Bounded, Ord)

data DType
  = StringType Int
  | IntType Int
  | BoolType
  deriving (Eq, Show, Ord)

data DValue
  = IntVal Int
  | BoolVal Bool
  | StringVal String
  | NullVal
  deriving (Eq, Show, Ord)

type Name = String

data OrderTypeAD
  = ASC
  | DESC
  deriving (Eq, Show, Enum, Bounded)

data OrderTypeFL
  = NULLSFIRST
  | NULLSLAST
  deriving (Eq, Show, Enum, Bounded)

data VerbAlter
  = Add Bool
  | DropColumn Bool

-- **** Section for AlterTableCommand ****

data AlterTableCommand = AlterTableCommand
  { fromAlterT :: FromExpression,
    verbAlterT :: VerbAlter,
    columnAlterT :: ColumnExpression
  }

-- **** Section for UpsertIntoCommand ****

data UpsertIntoCommand = UpsertIntoCommand
  { fromUpsertI :: FromExpression,
    columnNames :: Maybe [Name],
    valuesUpsertI :: [DValue]
  }

-- **** Section for DeleteCommand ****

data DeleteCommand = DeleteCommand
  { fromDelete :: TableName, -- Not supposed to have subquery in this case
    whDelete :: Maybe Expression
  }
  deriving (Eq, Show)

-- **** Section for CreateCommand ****

data CreateCommand = CreateCommand
  { ifNotExists :: Bool,
    nameCreate :: TableName,
    idCreate :: [(Name, DType, Bool)]
  }
  deriving (Eq, Show)

-- **** Section for SelectCommand ****

data SelectCommand = SelectCommand
  { exprsSelect :: (CountStyle, [ColumnExpression]),
    fromSelect :: FromExpression,
    whSelect :: Maybe Expression,
    groupbySelect :: [Var],
    orderbySelect :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)],
    limitSelect :: Maybe Int,
    offsetSelect :: Maybe Int
  }
  deriving (Eq, Show)

-- ******** Reserved Words section ********

reservedVerb :: [String]
reservedVerb =
  [ "SELECT",
    "FROM",
    "WHERE",
    "ORDER",
    "BY",
    "GROUP",
    "LIMIT",
    "OFFSET",
    "CREATE",
    "DELETE"
  ]

reservedCountStyle :: [String]
reservedCountStyle = ["DISTINCT"]

reservedJoinStyle :: [String]
reservedJoinStyle = ["JOIN", "LEFT", "RIGHT", "INNER", "OUTER"]

reservedAggFunction :: [String]
reservedAggFunction = ["AVG", "COUNT", "MAX", "MIN", "SUM"]

reservedFunction :: [String]
reservedFunction = ["LENGTH", "LOWER", "UPPER"]

reservedBopS :: [String]
reservedBopS = ["+", "-", "*", "//", "%", "=", ">=", "<=", ">", "<"]

reservedBopW :: [String]
reservedBopW = ["AND", "OR", "LIKE", "IS"]

reservedUopS :: [String]
reservedUopS = ["-"]

reservedUopW :: [String]
reservedUopW = ["NOT"]

reservedType :: [String]
reservedType = ["VARCHAR", "INTEGER"]

reservedVal :: [String]
reservedVal = ["TRUE", "FALSE", "NULL"]

reservedOrderTypeAD :: [String]
reservedOrderTypeAD = ["ASC", "DESC"]

reservedOrderTypeFL :: [String]
reservedOrderTypeFL = ["NULLS", "FIRST", "LAST"]

reservedKeyWords :: [String]
reservedKeyWords =
  (concat :: [[String]] -> [String])
    [ reservedVerb,
      reservedCountStyle,
      reservedJoinStyle,
      reservedAggFunction,
      reservedFunction,
      reservedBopS,
      reservedBopW,
      reservedUopS,
      reservedUopW,
      reservedType,
      reservedVal,
      reservedOrderTypeAD,
      reservedOrderTypeFL
    ]

reservedChar :: String
reservedChar = "\"'()`;"