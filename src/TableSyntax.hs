{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module TableSyntax where

import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered as OMap
import SQLSyntax

type Scope = OMap TableName Table

data Table = Table
  { indexName :: IndexName,
    tableMap :: TableData
  }

type TableData = [Row]

type IndexName = NE.NonEmpty Name

type RowIndex = NE.NonEmpty DValue

type Row = OMap Name DValue

type ErrorMsg = String

data GroupBy a where
  SingleGroupBy :: DValue -> GroupBy a
  MultiGroupBy :: DValue -> GroupBy a -> GroupBy a

type GroupByMap = OMap Var (GroupBy DValue)
