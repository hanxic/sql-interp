{-# LANGUAGE GADTs #-}

module TableSyntax where

import Data.Kind
import Data.Map as Map
import SQLSyntax

type Scope = Map TableName Table

data Table = Table
  { indexName :: IndexName,
    orderName :: IndexName,
    tableMap :: TableMap
  }

type TableMap = Map Index Row

type Row = Map Name DValue

type ErrorMsg = String

data IndexName where
  SingleName :: Name -> IndexName
  MultiName :: Name -> IndexName -> IndexName
  deriving (Show, Eq, Ord)

data Index where
  SingleIndex :: DValue -> Index
  MultiIndex :: DValue -> Index -> Index
  deriving (Show, Eq, Ord)

data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy DValue
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a
