{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module TableSyntax where

import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import SQLSyntax

data Store = Store
  { scope :: Scope,
    alias :: Map TableName TableName
  }
  deriving (Eq, Show)

type Scope = Map TableName Table

data Table = Table
  { indexName :: IndexName,
    tableData :: TableData
  }
  deriving (Eq, Show)

type TableData = [Row]

type IndexName = NE.NonEmpty (Name, IndexAttribute)

data IndexAttribute
  = Regular
  | PrimaryKey
  deriving (Eq, Show, Bounded, Enum)

type Row = Map Name DValue

type ErrorMsg = String

data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy a
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a

type GroupByMap = Map Var (GroupBy DValue)

type GroupByVars = NE.NonEmpty Name