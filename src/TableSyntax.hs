{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module TableSyntax where

import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import SQLSyntax

data Store = Store
  { scope :: Scope,
    alias :: Map TableName TableName
  }
  deriving (Eq, Show)

type Alias = Map TableName TableName

type Scope = Map TableName Table

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

type ErrorMsg = String

data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy a
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a

type GroupByMap = Map Var (GroupBy DValue)

type GroupByVars = NE.NonEmpty Name

-- This is a temporary data structure and not with actual use in parser / printing
type Header = [Var] -- True if it is a primary key, and false if not