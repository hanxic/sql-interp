module TableSyntax where

import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax

type Id = String

type Table = Map Id Row

{- Id needs to be nonempty variable names (either quoted names or other names)-}

type Column = String

type Row = Map Var DValue

type Store = Map TableName Table