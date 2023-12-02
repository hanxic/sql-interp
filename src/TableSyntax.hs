{-# LANGUAGE GADTs #-}

module TableSyntax where

import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax

type Id = String

type Table = Map Id Row

{- Id needs to be nonempty variable names (either quoted names or other names)-}

{- data PrimaryKeys :: Flag -> Var -> Type where
  Nil :: PrimaryKeys Empty a
  Cons :: a -> PrimaryKey f a -> List NonEmpty a
 -}
type Column = String

type Row = Map Var DValue

type Store = Map TableName Table