module TableSyntax where

import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax

type Id = String

type Table = Map Id Row

type Column = String

type Row = Map Var DValue