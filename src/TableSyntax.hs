{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module TableSyntax where

import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import SQLSyntax

type Scope = Map TableName Table

data Table = Table
  { indexName :: IndexName,
    tableData :: TableData
  }

type TableData = [Row]

type IndexName = NE.NonEmpty Name

type Row = Map Name DValue

type ErrorMsg = String

data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy a
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a

type GroupByMap = Map Var (GroupBy DValue)

type GroupByVars = NE.NonEmpty Name