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

{- Awesome grammar source: https://forcedotcom.github.io/phoenix/
Grammar Source: https://www.dataquest.io/blog/sql-commands/
Optimization Source: https://www.analyticsvidhya.com/blog/2021/10/a-detailed-guide-on-sql-query-optimization/
Relational Algebra: https://byjus.com/gate/relational-algebra-in-dbms-notes/#:~:text=Calculus%20(or%20DRC)-,What%20is%20Relational%20Algebra%20in%20DBMS%3F,unary%20operator%20can%20be%20used
Equivalence Rules: https://www.postgresql.org/message-id/attachment/32513/EquivalenceRules.pdf -}

-- Need to add offset into the syntax

-- ******** Construct for SQL Syntax ********

data ColumnExpression
  = ColumnName Expression -- e.g. SELECT A / SELECT (A * 2) / SELECT SUM(A) / SELECT SUM(2)
  | ColumnAlias Expression Var -- e.g. SELECT A AS B
  | AllVar
  deriving (Eq, Show)

data CountStyle
  = Distinct
  | All
  deriving (Eq, Show, Enum, Bounded)

type TableName = String

data FromExpression
  = TableRef TableName -- e.g. FROM TEST
  | -- | TableAlias TableName Var -- e.g. FROM A AS B
    SubQuery SelectCommand -- e.g. FROM (SELECT ...)
  | Join FromExpression JoinStyle FromExpression -- e.g. FROM A JOIN B
  deriving (Eq, Show)

data JoinStyle
  = LeftJoin
  | RightJoin
  | InnerJoin
  | OuterJoin
  deriving (Eq, Show, Enum, Bounded)

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase Fun {} = True
isBase AggFun {} = True
isBase _ = False

level :: Bop -> Int
level Times = 20
level Divide = 20
level Modulo = 20
level Plus = 17
level Minus = 17
level Eq = 15
level Gt = 15
level Ge = 15
level Lt = 15
level Le = 15
level Is = 15
level Like = 15
level And = 8
level Or = 7

data Expression
  = Var Var -- e.g. A
  | Val DValue
  | Op1 Uop Expression -- e.g. NOT A
  | Op2 Expression Bop Expression -- e.g. A + 2
  | AggFun AggFunction CountStyle Expression
  | Fun Function Expression -- e.g. SUM / AVG
  deriving (Eq, Show)

data Var
  = VarName Name -- Does not quoted, Must start from an alphabet and follow by int or alphabet
  | QuotedName Name -- Quoted, can be anything
  deriving (Eq, Show)

data Uop
  = Not
  | Neg
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
  | And
  | Or
  | Like
  | Is
  deriving (Eq, Show, Enum, Bounded)

data AggFunction
  = Avg
  | Count
  | Max
  | Min
  | Sum
  deriving (Eq, Show, Enum, Bounded)

data Function
  = Len
  | Lower
  | Upper
  deriving (Eq, Show, Enum, Bounded)

data DType
  = StringType Int
  | IntType Int
  | BoolType
  deriving (Eq, Show)

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

-- No set

-- **** Section for AlterTableCommand ****

data AlterTableCommand = AlterTableCommand
  { fromAlterT :: TableName,
    verbAlterT :: VerbAlter,
    columnAlterT :: ColumnExpression
  }

-- **** Section for UpsertIntoCommand ****

data UpsertIntoCommand = UpsertIntoCommand
  { fromUpsertI :: TableName,
    columnNames :: Maybe [Name],
    valuesUpsertI :: [DValue]
  }

-- **** Section for DeleteCommand ****

data DeleteCommand = DeleteCommand
  { fromDelete :: FromExpression,
    whDelete :: Maybe Expression
  }
  deriving (Eq, Show)

-- **** Section for CreateCommand ****

data CreateCommand = CreateCommand
  { ifNotExists :: Bool,
    nameCreate :: TableName,
    idCreate :: [(Name, DType, Bool, Bool)]
    -- TODO: Haven't finished
  }
  deriving (Eq, Show)

-- **** Section for SelectCommand ****

data SelectCommand = SelectCommand
  { exprsSelect :: [(CountStyle, ColumnExpression)],
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
  concat
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
