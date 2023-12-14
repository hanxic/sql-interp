{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module TableSyntax where

import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import SQLSyntax

{-
The scope contains the context of available named tables which queries can reference
The alias maps table names the new names altered by the user
-}
data Store = Store
  { scope :: Scope,
    alias :: Map TableName TableName
  }
  deriving (Eq, Show)

type Alias = Map TableName TableName

type Scope = Map TableName Table

{-
A table consists of:
  - An ordered List of Rows (TableData), where a row is a Map from column names to values
  - The implicit order of the rows is based on a nonempty list of columns names (PrimaryKey)
  - The schema of the table in the form of column Name, DType tuples
-}

data Table = Table
  { primaryKeys :: PrimaryKeys,
    indexName :: IndexName,
    tableData :: TableData
  }
  deriving (Eq, Show)

type TableData = [Row]

type IndexName = [(Var, DType)]

type PrimaryKeys = NE.NonEmpty (Var, DType)

data IndexAttribute
  = Regular
  | PrimaryKey
  deriving (Eq, Show, Bounded, Enum)

type Row = Map Var DValue

{-
The output of executing a query yields either an error message or an updated store
-}
type ErrorMsg = String

{-
A valid group as combined using GROUP BY will be var names mapped to a nonempty list of values
-}
data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy a
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a

type GroupByMap = Map Var (GroupBy DValue)

type GroupByVars = NE.NonEmpty Name

{-
Header types utilized in Printer and Parser
-}
type Header = [Var]

type AnnotatedHeader = IndexName