module SQLSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

type Name = String

data Command = Command
  { verb :: Verb, -- none empty
    from :: Command,
    wh :: Statement
  }

data Verb
  = Select [Expression]
  | Insert [Expression]
  | Delete [Expression]

data Statement
  = GroupBy
  | OrderBy
  | Join

data Expression
  = Var Var
  | Value Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression

data Uop
  = Neg
  | Not
  | Len

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
  | Concat

type Var = Name

data Value
  = IntVal Int
  | BoolVal Bool
  | StringVal String

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