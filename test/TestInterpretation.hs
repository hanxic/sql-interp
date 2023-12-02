module TestInterpretation where

import Interpretation
import Parser

instance Arbitrary Table where
  arbitrary :: Gen Table
  arbitrary = undefined

  shrink :: Table -> [Table]
  shrink = undefined

prop_where_size :: Table -> Bool
prop_where_size = undefined

-- prop_where_size = size (tableWithWhereClause) <= size (table)

prop_select_all :: Table -> Bool
prop_select_all = undefined

-- prop_select_all = select every column == table
