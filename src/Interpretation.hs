{-# LANGUAGE GADTs #-}

module Interpretation where

{- import Data.Bits (Bits (bitSize), FiniteBits (finiteBitSize)) -}

import Control.Applicative
import Control.Monad
import Control.Monad.Except (Except, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.State qualified as S
import Data.Char (toLower, toUpper)
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List as List
import Data.List.NonEmpty qualified as NE
import Data.Map as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import GenSQL qualified as GSQL
import Parser qualified as PAR
import SQLParser qualified as SPAR
import SQLPrinter qualified as SP
import SQLSyntax
import TableParser qualified as TPAR
import TablePrinter qualified as TP
import TableSyntax
  ( ErrorMsg,
    IndexAttribute (PrimaryKey),
    IndexName,
    PrimaryKeys,
    Row,
    Scope,
    Store (..),
    Table (..),
    TableData,
  )
import Test.HUnit (Counts, Test (..), assertFailure, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen.Unsafe qualified as QCGU
import Text.Printf (FieldFormat (FieldFormat))
import Text.Read (readMaybe)
import Text.Regex.Base qualified as REGEX
import Text.Regex.Posix qualified as POSIX ((=~))
import Utils

{- import Test.QuickCheck.Monadic qualified as QCM -}

type SQLI a = ExceptT String (State Store) a

{- Helper function on monad manipulation -}

getAlias :: SQLI (Map TableName TableName)
getAlias = do S.gets alias

goStEx :: a -> (a -> SQLI a) -> Store -> (Either String a, Store)
goStEx a f store = f a & runExceptT & flip runState store

prop_roundtrip_getAlias :: Store -> Bool
prop_roundtrip_getAlias store = fst (goStEx Map.empty (const getAlias) store) == Right (alias store)

getAliasName :: TableName -> SQLI TableName
getAliasName tn = do Map.findWithDefault tn tn <$> getAlias

setAlias :: TableName -> TableName -> SQLI ()
setAlias tn1 tn2 = do
  store <- S.get
  if Map.member tn1 $ scope store
    then
      let newAlias = Map.insert tn2 tn1 $ alias store
       in let newState = Store (scope store) newAlias
           in S.put newState
    else throwError "Update Alias failure"

delAlias :: TableName -> SQLI ()
delAlias tn = do
  store <- S.get
  let newAlias = Map.delete tn $ alias store
   in let newStore = Store (scope store) newAlias
       in S.put newStore

printAlias :: SQLI String
printAlias = do show <$> getAlias

getFreeNameCount :: SQLI Int
getFreeNameCount = do
  alias <- getAlias
  return $ length alias + 1

getFreeName :: SQLI TableName
getFreeName = do
  i <- getFreeNameCount
  return $ "_Table" ++ show i

prop_roundtrip_getScope :: Store -> Bool
prop_roundtrip_getScope store = fst (goStEx Map.empty (const getScope) store) == Right (scope store)

getScope :: SQLI Scope
getScope = do S.gets scope

getTable :: TableName -> SQLI (Maybe (TableName, Table))
getTable tn = do
  scope <- getScope
  case Map.lookup tn scope of
    Just table -> return $ Just (tn, table)
    _ -> return Nothing

setScope :: TableName -> Table -> SQLI Store
setScope tn t = do
  store <- S.get
  let newScope = Map.insert tn t $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore >> return newStore

delScope :: TableName -> SQLI ()
delScope tn = do
  store <- S.get
  let newScope = Map.delete tn $ scope store
   in let newStore = Store newScope (alias store)
       in S.put newStore

getStore :: SQLI Store
getStore = S.get

hideScope :: SQLI a -> SQLI a
hideScope sqli = do
  store <- S.get
  result <- sqli
  S.put store
  return result

-- @Gary THIS IS THE MAIN WORK HORSE
-- 1. Find the table referenced in FROM
-- 2. Filter out the rows we don't care about
-- 3. Calculate new columns, 1 per Expression
-- 4. Sort the table based on SORT BY
-- 5. Slice the top N rows based on Limit/Offset
-- X. TODO GroupBy, I am thinking of branching

evalFrom :: FromExpression -> SQLI (TableName, Table)
evalFrom (TableRef tn) = do
  mt <- getTable tn
  case mt of
    Just tnTable -> return tnTable
    Nothing -> throwError $ "No Table for table reference: " ++ SP.pretty tn
evalFrom fe@(TableAlias tn1 tn2) = do
  mt <- getTable tn1
  case mt of
    Just tnTable ->
      setAlias tn1 tn2 >> return (tn2, snd tnTable)
    _ -> throwError $ "No Table for table alias: " ++ SP.pretty fe
evalFrom (Join fe1 js fe2 jns) = do
  tnTable1 <- evalFrom fe1
  tnTable2 <- evalFrom fe2
  (newName, newTable) <- evalJoin tnTable1 js tnTable2 jns
  setScope newName newTable
  return (newName, newTable)
evalFrom (SubQuery q) = do
  table <- evalSelectCommand q
  newName <- getFreeName
  return (newName, table)

interp :: SQLI a -> Store -> Either String a
interp = S.evalState . runExceptT

exec :: SQLI a -> Store -> Store
exec = S.execState . runExceptT

run :: SQLI a -> Store -> (Either String a, Store)
run = S.runState . runExceptT

evalJoin :: (TableName, Table) -> JoinStyle -> (TableName, Table) -> JoinNames -> SQLI (TableName, Table)
evalJoin (tn1, table1) js (tn2, table2) [] = do
  freeName <- getFreeName
  let joinSpec = const $ const True
   in (freeName,) <$> evalJoinTable table1 js table2 joinSpec {- evalJoinStyle table1 OuterJoin table2 joinSpec -}
evalJoin (tn1, table1) js (tn2, table2) jns = do
  freeName <- getFreeName
  joinSpec <- getJoinSpec tn1 tn2 jns
  (freeName,) <$> evalJoinTable table1 js table2 joinSpec

{- table <- {- evalJoinStyle table1 js table2 joinSpec -}
return (freeName, table) -}

mergeIndex :: IndexName -> IndexName -> IndexName
mergeIndex in1 in2 =
  let inMap1 = Map.fromList in1
   in let inMap2 = Map.fromList in2
       in Map.toList $ Map.union inMap1 inMap2

mergePrimaryKeys :: PrimaryKeys -> PrimaryKeys -> PrimaryKeys
mergePrimaryKeys pk1 pk2 =
  NE.fromList $ mergeIndex (NE.toList pk1) (NE.toList pk2)

evalJoinTable :: Table -> JoinStyle -> Table -> (Row -> Row -> Bool) -> SQLI Table
evalJoinTable (Table pk1 in1 td1) js (Table pk2 in2 td2) joinSpec =
  let mergedPK = mergePrimaryKeys pk1 pk2
   in let mergedIN = mergeIndex in1 in2 in Table mergedPK mergedIN <$> evalJoinStyle td1 js td2 joinSpec

joinMid :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinMid td1 td2 joinSpec =
  [row1 `Map.union` row2 | row1 <- td1, row2 <- td2, joinSpec row1 row2]

joinLeftEx :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinLeftEx td1 td2 joinSpec =
  let keys2 = keys $ head td2
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys2
       in let tdLeft = [row1 `Map.union` uninitRow | row1 <- td1, (not . any (joinSpec row1)) td2]
           in tdLeft

joinRightEx :: TableData -> TableData -> (Row -> Row -> Bool) -> TableData
joinRightEx td1 td2 joinSpec =
  let keys1 = keys $ head td1
   in let uninitRow = Map.fromList $ List.map (,NullVal) keys1
       in let tdRight = [row2 `Map.union` uninitRow | row2 <- td2, (not . any (joinSpec row2)) td1]
           in tdRight

evalJoinStyle :: TableData -> JoinStyle -> TableData -> (Row -> Row -> Bool) -> SQLI TableData
evalJoinStyle td1 InnerJoin td2 joinSpec =
  return $
    joinMid td1 td2 joinSpec
evalJoinStyle td1 LeftJoin td2 joinSpec =
  if List.null td2
    then return td1
    else
      let tdLeft = joinLeftEx td1 td2 joinSpec
       in let tdMid = joinMid td1 td2 joinSpec
           in return (tdLeft ++ tdMid)
evalJoinStyle td1 RightJoin td2 joinSpec =
  if List.null td1
    then return td2
    else
      let tdRight = joinRightEx td1 td2 joinSpec
       in let tdMid = joinMid td1 td2 joinSpec
           in return (tdRight ++ tdMid)
evalJoinStyle td1 OuterJoin td2 joinSpec = do
  tdLeftMid <- evalJoinStyle td1 LeftJoin td2 joinSpec
  let tdRight = joinRightEx td1 td2 joinSpec
   in return (tdLeftMid ++ tdRight)

{-
Anything in left + anything in middle
1. Get everything in left that are not in middle
  List comprehension -> Get everything form left such that the f function is not true -> Append them with td1'
2. Get everything in middle
  Do the usual evalJoinStyle thing
3. Merge them together. Simply merge based on newtable.

-}

-- | Turn joinnames into a single conditional that must be true for two row to be match
getJoinSpec :: TableName -> TableName -> [(Var, Var)] -> SQLI (Row -> Row -> Bool)
getJoinSpec tn1 tn2 = foldM (getJoinSpecAux tn1 tn2) (const $ const True)
  where
    throwAliases :: Var -> Var -> SQLI (Row -> Row -> Bool)
    throwAliases var1 var2 = do
      {- aliasStr <- printAlias -}
      throwError $ "undefined join names: left is " ++ SP.pretty var1 ++ " right is " ++ SP.pretty var2
    getJoinSpecAux :: TableName -> TableName -> (Row -> Row -> Bool) -> (Var, Var) -> SQLI (Row -> Row -> Bool)
    getJoinSpecAux tn1 tn2 f (var1, var2) = do
      (alias1, jo1) <- case var1 of
        VarName name -> throwError "undefined join name: join name needs to be a dot format"
        Dot name jo -> return (name, jo)
      (alias2, jo2) <- case var2 of
        VarName name -> throwError "undefined join name: join name needs to be a dot format"
        Dot name jo -> return (name, jo)
      if tn1 == alias1 && tn2 == alias2
        then return $ \row1 row2 -> Map.lookup jo1 row1 == Map.lookup jo2 row2 && f row1 row2
        else
          if tn1 == alias2 && tn2 == alias1
            then return $ \row1 row2 -> Map.lookup jo2 row1 == Map.lookup jo1 row2
            else throwAliases var1 var2

evalQuery :: Query -> SQLI Table
evalQuery (SelectQuery q) = evalSelectCommand q
evalQuery (DeleteQuery q) = evalDeleteCommand q
evalQuery (CreateQuery q) = evalCreateCommand q

eval :: Queries -> SQLI ()
eval = mapM_ evalQuery

evalSelectCommand :: SelectCommand -> SQLI Table
evalSelectCommand q = do
  tableFrom <- evalFrom (fromSelect q)
  tableWhere <- evalWhere (whSelect q) tableFrom
  tableGroupBy <- evalGroupBySelect1 (groupbySelect q) (exprsSelect q) tableWhere
  tableOrder <- evalOrderBy (orderbySelect q) tableGroupBy
  tableOffset <- evalOffset (offsetSelect q) tableOrder
  tableResult <- evalLimit (limitSelect q) tableOffset
  freename <- getFreeName
  setScope freename tableResult
  return tableResult

tSC :: String -> Store -> Table -> Test
tSC commandStr store table =
  case PAR.parse SPAR.scP commandStr of
    Left errorMsg -> TestCase (assertFailure errorMsg)
    Right sc -> interp (evalSelectCommand sc) store ~?= Right table

evalWhere :: Maybe Expression -> (TableName, Table) -> SQLI (TableName, Table)
evalWhere Nothing (tn, table) = return (tn, table)
evalWhere (Just expr) (tn, table) = do
  newTD <- evalWhereExpr expr (tableData table)
  return (tn, Table (primaryKeys table) (indexName table) newTD)

evalWhereExpr :: Expression -> TableData -> SQLI TableData
evalWhereExpr expr td = do
  exprNorm <- evalEAgg expr td
  foldrM (\r acc -> evalE exprNorm r >>= getBool >>= (\b -> if b then return (r : acc) else return acc)) [] td

evalEAgg :: Expression -> TableData -> SQLI Expression
evalEAgg v@(Var _) td = return v
evalEAgg v@(Val _) td = return v
evalEAgg (Op1 uop expr) td = Op1 uop <$> evalEAgg expr td
evalEAgg (Op2 expr1 bop expr2) td = do
  expr1' <- evalEAgg expr1 td
  expr2' <- evalEAgg expr2 td
  return $ Op2 expr1' bop expr2'
evalEAgg (Fun fun expr) td = do
  expr' <- evalEAgg expr td
  return $ Fun fun expr'
evalEAgg (AggFun aggfun cs expr) td = do
  expr' <- evalEAgg expr td
  Val <$> evalEAggAux aggfun cs expr' td
  where
    evalEAggAux :: AggFunction -> CountStyle -> Expression -> TableData -> SQLI DValue
    evalEAggAux Sum _ expr td =
      foldM
        (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc >>= (\i2 -> return $ IntVal (i1 + i2))))
        (IntVal 0)
        td
    evalEAggAux Count _ expr td = foldM (\acc r -> evalE expr r >>= (\x -> getInt acc >>= (\i -> return $ IntVal $ if x /= NullVal then i + 1 else i))) (IntVal 0) td
    -- Count non-bool expression not supported / may generate undefined behavior
    evalEAggAux Max _ expr td = foldM (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc Data.Functor.<&> (IntVal . max i1))) (IntVal 0) td
    evalEAggAux Min _ expr td = foldM (\acc r -> evalE expr r >>= getInt >>= (\i1 -> getInt acc Data.Functor.<&> (IntVal . min i1))) (IntVal 0) td
    evalEAggAux Avg cs expr td = do
      s <- evalEAggAux Sum cs expr td
      c <- evalEAggAux Count cs expr td
      case (s, c) of
        (IntVal numerator, IntVal denomenator) -> if denomenator == 0 then return $ IntVal 0 else return $ IntVal $ numerator `div` denomenator
        (_, _) -> throwError "AggFunction Type error in Avg"

{- evalEAggNI :: DValue -> DValue -> ()SQLI DValue -}

-- first do a regularization in expr -> make sure its Aggfunction free
-- Map each row into some DValue
-- Then Aggregate

getInt :: DValue -> SQLI Int
getInt (IntVal i) = return i
getInt b@(BoolVal _) = getInt =<< weakCast b (IntType 32)
getInt NullVal = return 0
getInt v = throwCastError v (IntType 32)

getBool :: DValue -> SQLI Bool
getBool (BoolVal b) = return b
getBool ival@(IntVal _) = getBool =<< weakCast ival BoolType
getBool v = throwCastError v BoolType

evalE :: Expression -> Row -> SQLI DValue
evalE (Var v) r =
  case Map.lookup v r of
    Nothing -> return NullVal
    Just val -> return val
evalE (Val v) r = return v
evalE (Op2 e1 o e2) r = do
  dval1 <- evalE e1 r
  dval2 <- evalE e2 r
  evalOp2Robust o dval1 dval2
{- evalOp2 o <*> evalE e1 r <*> evalE e2 r -}
evalE (Op1 o e1) r = evalOp1 o =<< evalE e1 r
evalE (Fun f e) r = evalFun f =<< evalE e r
evalE e r = throwError $ "Illegal expression: " ++ TP.pretty e

evalOp2 :: Bop -> DValue -> DValue -> SQLI DValue
evalOp2 Plus (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = return NullVal
evalOp2 Divide (IntVal i1) (IntVal i2) = return $ IntVal (i1 `div` i2)
evalOp2 Modulo _ (IntVal i2) | i2 <= 0 = return NullVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = return $ IntVal (i1 `mod` i2)
evalOp2 Eq val1 val2 = return $ BoolVal $ val1 == val2
evalOp2 Gt val1 val2 = return $ BoolVal $ val1 > val2
evalOp2 Ge val1 val2 = return $ BoolVal $ val1 >= val2
evalOp2 Lt val1 val2 = return $ BoolVal $ val1 < val2
evalOp2 Le val1 val2 = return $ BoolVal $ val1 <= val2
evalOp2 And (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 && b2
evalOp2 Or (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 || b2
evalOp2 Like (StringVal str1) (StringVal str2) = return $ BoolVal $ str1 POSIX.=~ str2
evalOp2 Is val1 val2 = evalOp2 Eq val1 val2
evalOp2 bop NullVal _ = return NullVal -- If either is NULL, return NULL
evalOp2 bop _ NullVal = return NullVal
evalOp2 bop dval1 dval2 =
  throwExpressionError (Op2 (Val dval1) bop (Val dval2))

evalOp2Robust :: Bop -> DValue -> DValue -> SQLI DValue
evalOp2Robust bop dval1 dval2 =
  if level bop >= 17
    then
      weakCast dval1 (IntType 64)
        >>= (\c1 -> weakCast dval2 (IntType 64) >>= evalOp2 bop c1)
    else evalOp2 bop dval1 dval2

weakCast :: DValue -> DType -> SQLI DValue
weakCast bval@(BoolVal _) BoolType = return bval
weakCast (BoolVal b) (IntType n) = return $ IntVal $ if b then 1 else 0
weakCast ival@(IntVal i) (IntType n) = return ival
weakCast (IntVal i) BoolType = return $ BoolVal $ i == 1
weakCast sval@(StringVal _) (StringType n) = return sval
weakCast NullVal _ = return NullVal
weakCast dval dtyp = throwCastError dval dtyp

evalOp1 :: Uop -> DValue -> SQLI DValue
evalOp1 Neg (IntVal i) = return $ IntVal (-i)
evalOp1 Neg bval@(BoolVal _) = weakCast bval (IntType 1) >>= evalOp1 Neg
evalOp1 Not (BoolVal b) = return $ BoolVal (not b)
evalOp1 Not ival@(IntVal _) = weakCast ival BoolType >>= evalOp1 Not
evalOp1 uop dval = throwExpressionError $ Op1 uop (Val dval)

evalFun :: Function -> DValue -> SQLI DValue
evalFun Len (StringVal str) = return $ IntVal $ length str
evalFun Lower (StringVal str) = return $ StringVal $ List.map toLower str
evalFun Upper (StringVal str) = return $ StringVal $ List.map toUpper str
evalFun _ NullVal = return NullVal
evalFun fun dval = throwExpressionError (Fun fun (Val dval))

evalOffset :: Maybe Int -> Table -> SQLI Table
evalOffset Nothing td = return td
evalOffset (Just i) table | i >= 0 = return $ Table (primaryKeys table) (indexName table) (List.drop i $ tableData table)
evalOffset _ _ = throwError "Offset cannot be negative"

evalLimit :: Maybe Int -> Table -> SQLI Table
evalLimit Nothing td = return td
evalLimit (Just i) table | i >= 0 = return $ Table (primaryKeys table) (indexName table) (List.take i $ tableData table)
evalLimit _ _ = throwError "Limit cannot be negative"

evalOrderBy :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> Table -> SQLI Table
evalOrderBy [] table = return table
evalOrderBy orders (Table pk iName td) = do
  ordering <- getOrdering orders
  sortedTD <- evalSort ordering td
  return $ Table pk iName sortedTD

evalSort :: (Row -> Row -> Ordering) -> TableData -> SQLI TableData
evalSort ordering td = return $ List.sortBy ordering td

{-
sort on the first variable, and group by -> repeatedly sort on the second and group by

-}

getOrdering :: [(Var, Maybe OrderTypeAD, Maybe OrderTypeFL)] -> SQLI (Row -> Row -> Ordering)
getOrdering = foldrM decideOrdering (const $ const EQ) -- will not happen

-- | A helper function that, given a single order by parameter and a ordering function, returning a new ordering function with the parameter as the first decider
decideOrdering :: (Var, Maybe OrderTypeAD, Maybe OrderTypeFL) -> (Row -> Row -> Ordering) -> SQLI (Row -> Row -> Ordering)
decideOrdering (v, mad, mfl) accF = return $ \r1 r2 ->
  let (dval1, dval2) =
        ( Map.findWithDefault NullVal v r1,
          Map.findWithDefault NullVal v r2
        )
   in case mfl of
        Just NULLSFIRST ->
          case (dval1, dval2) of
            (NullVal, NullVal) -> accF r1 r2
            (NullVal, _) -> LT
            (_, NullVal) -> GT
            _ -> decideOrderingAux dval1 dval2 mad accF r1 r2
        _ ->
          case (dval1, dval2) of
            (NullVal, _) -> compare dval1 dval2
            (_, NullVal) -> compare dval1 dval2
            _ -> decideOrderingAux dval1 dval2 mad accF r1 r2
  where
    decideOrderingAux :: DValue -> DValue -> Maybe OrderTypeAD -> (Row -> Row -> Ordering) -> (Row -> Row -> Ordering)
    decideOrderingAux dval1 dval2 mad accF r1 r2 =
      case mad of
        Just DESC ->
          -- Normal ordering is ascending order
          case compare dval1 dval2 of
            EQ -> accF r1 r2
            LT -> GT
            GT -> LT
        _ -> case compare dval1 dval2 of
          EQ -> accF r1 r2
          o -> o

-- | A helper function that check whether the select expressions have aggregate function or not
hasAggFun :: [ColumnExpression] -> Bool
hasAggFun = List.foldr (\x acc -> acc || hasAggFunCol x) False
  where
    hasAggFunCol :: ColumnExpression -> Bool
    hasAggFunCol (ColumnName expr) = hasAggFunExpr expr
    hasAggFunCol (ColumnAlias expr _) = hasAggFunExpr expr
    hasAggFunCol AllVar = False

hasAggFunExpr :: Expression -> Bool
hasAggFunExpr (AggFun {}) = True
hasAggFunExpr (Op1 _ expr) = hasAggFunExpr expr
hasAggFunExpr (Op2 expr1 _ expr2) = hasAggFunExpr expr1 || hasAggFunExpr expr2
hasAggFunExpr (Fun _ expr) = hasAggFunExpr expr
hasAggFunExpr _ = False

{-
Rethink the workflow for group by
1. For every column expression, annotated it to have a type
2. evalGroupBySelect should be correct
3. The question is about type, if I annotate it first and then find primary keys: Two conditions:
  1. Either the columns cover all primary keys
  2. Or the first group key is covered in some expression as the main dominator

-}

-- | A helper function that, given the first parameter of group by, the select clause, and primary keys and index, return the newer primary keys and index
evalGroupBySelectPKIN :: Maybe Var -> [ColumnExpression] -> PrimaryKeys -> IndexName -> SQLI (PrimaryKeys, IndexName)
evalGroupBySelectPKIN fstGroup ces pk iName =
  case deterPK fstGroup ces pk of -- Determine if there exists a primary key, maybe not possible
    Nothing -> throwError "No primary key is valid"
    Just pks ->
      let newTypeMap = extractAH ces pk iName
       in let pkMap = Map.fromList $ NE.toList pks
           in let newIName =
                    -- index name is anything other than the primary key
                    Map.foldrWithKey
                      ( \var dtype acc ->
                          if var `notMember` pkMap
                            then (var, dtype) : acc
                            else acc
                      )
                      []
                      newTypeMap
               in return (pks, newIName)

annotatedMapCreate :: PrimaryKeys -> IndexName -> Map Var DType
annotatedMapCreate pk iName = Map.fromList (NE.toList pk) `Map.union` Map.fromList iName

-- | The function for grouping the table by certain columns and then select
-- For performance, group by function will essentially leverage sorting functions and groupping
annotatedSelect :: [ColumnExpression] -> PrimaryKeys -> IndexName -> SQLI (Map Expression (Var, DType))
annotatedSelect ces pk iName =
  let ahMap = annotatedMapCreate pk iName
   in foldM (go ahMap) Map.empty ces
  where
    go :: Map Var DType -> Map Expression (Var, DType) -> ColumnExpression -> SQLI (Map Expression (Var, DType))
    go ahMap aeMap AllVar =
      let eMap = Map.mapKeys Var (Map.mapWithKey (,) ahMap)
       in return $ eMap `Map.union` aeMap
    go ahMap aeMap (ColumnName expr) =
      case inferExprType expr ahMap of
        Nothing -> throwError $ "Cannot infer the type of " ++ TP.pretty expr
        Just dtype -> return $ Map.singleton expr (VarName $ TP.pretty expr, dtype) `Map.union` aeMap
    {-         >>= (\x -> return $ x `Map.union` aeMap)
              . (\x -> Map.singleton expr (VarName $ TP.pretty expr, x)) -}
    go ahMap aeMap (ColumnAlias expr alias) =
      case inferExprType expr ahMap of
        Nothing -> throwError $ "Cannot infer  the type of " ++ TP.pretty expr
        Just dtype -> return $ Map.singleton expr (VarName alias, dtype) `Map.union` aeMap

evalGroupBySelect1 :: [Var] -> (CountStyle, [ColumnExpression]) -> (TableName, Table) -> SQLI Table
evalGroupBySelect1 vars (cs, ces) (tn, Table pk iName td) = do
  aeMap <- annotatedSelect ces pk iName
  if Map.null aeMap
    then throwError "Zero valid selection"
    else evalGroupBySelectUlt vars aeMap pk iName tn td

evalGroupBySelectUlt :: [Var] -> Map Expression (Var, DType) -> PrimaryKeys -> IndexName -> TableName -> TableData -> SQLI Table
evalGroupBySelectUlt [] aeMap pk iName tn td =
  let rowGroups = if any hasAggFunExpr (keys aeMap) then [td] else List.map (: []) td
   in evalSelectRowGroups' aeMap pk iName rowGroups
evalGroupBySelectUlt vars aeMap pk iName tn td = do
  ordering <- getOrdering (List.map (,Nothing,Nothing) vars)
  td' <- evalSort ordering td
  let rowGroups = List.groupBy (\r1 r2 -> EQ == ordering r1 r2) td'
   in evalSelectRowGroups' aeMap pk iName rowGroups

-- | The use of this function is that it will actually filter every expressions to form a row.
evalSelectRowGroups' :: Map Expression (Var, DType) -> PrimaryKeys -> IndexName -> [[Row]] -> SQLI Table
evalSelectRowGroups' aeMap pk iName rowGroups =
  let newPK = Map.toList aeMap
   in do
        newTD <- foldrM (\x acc -> (: acc) <$> evalSelectAux (Map.toList aeMap) pk iName x) [] rowGroups
        return $ Table (NE.fromList $ List.map snd newPK) [] newTD
  where
    evalSelectAux :: [(Expression, (Var, DType))] -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalSelectAux ce pk iName td =
      foldM (\acc (expr, (alias, _)) -> evalExpr expr alias pk iName td <&> (acc `Map.union`)) Map.empty ce
    evalExpr :: Expression -> Var -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalExpr expr alias pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton alias resValue

evalGroupBySelect :: [Var] -> (CountStyle, [ColumnExpression]) -> (TableName, Table) -> SQLI Table
-- If no group by parameter is presented
-- If there exists an aggregate function, will only present one row. If not, will present into multiple rows
evalGroupBySelect [] (cs, ces) (tn, Table pk iName td) =
  let rowGroups = if hasAggFun ces then [td] else List.map (: []) td
   in do
        (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
        evalSelectRowGroups (cs, ces) tn pk iName newPK newIName rowGroups
evalGroupBySelect vars selectParams@(_, ces) (tn, Table pk iName td) = do
  (newPK, newIName) <- evalGroupBySelectPKIN Nothing ces pk iName
  ordering <- getOrdering (List.map (,Nothing,Nothing) vars)
  td <- evalSort ordering td
  let rowGroups = List.groupBy (\r1 r2 -> EQ == ordering r1 r2) td
   in evalSelectRowGroups selectParams tn pk iName newPK newIName rowGroups

deterPK :: Maybe Var -> [ColumnExpression] -> PrimaryKeys -> Maybe PrimaryKeys
deterPK fstGroup ces pk =
  let pkMap = Map.fromList $ NE.toList pk
   in let pospkMap = foldMap (`extractPosPK` pkMap) ces
       in let identifiedPK = foldMap (\x -> maybeToList $ Map.lookup (fst x) pospkMap) pk
           in if length identifiedPK == length pk
                then Just $ NE.fromList identifiedPK
                else Just . NE.singleton =<< flip Map.lookup pospkMap =<< fstGroup

extractPosPK :: ColumnExpression -> Map Var DType -> Map Var (Var, DType)
extractPosPK AllVar typeMap = Map.mapWithKey (,) typeMap
extractPosPK (ColumnName expr) typeMap =
  let alias = VarName $ TP.pretty expr
   in case extractPosPKExpr expr alias typeMap of
        Nothing -> Map.empty
        Just pospk -> uncurry Map.singleton pospk
extractPosPK (ColumnAlias expr alias) pk =
  case extractPosPKExpr expr (VarName alias) pk of
    Nothing -> Map.empty
    Just pospk -> uncurry Map.singleton pospk

-- | Given an expression, a variable, and a map for primary key
-- Return a variable, its alias and data type, should it exists in the
-- Primary key map
extractPosPKExpr :: Expression -> Var -> Map Var DType -> Maybe (Var, (Var, DType))
extractPosPKExpr expr@(Var var) alias typeMap =
  (\x -> (var, (alias, x))) <$> inferExprType expr typeMap
extractPosPKExpr (Val _) _ _ = Nothing
extractPosPKExpr (Op1 uop expr1) alias pk = extractPosPKExpr expr1 alias pk
extractPosPKExpr (Op2 expr1 bop expr2) alias pk =
  let pospk1 = extractPosPKExpr expr1 alias pk
   in let pospk2 = extractPosPKExpr expr2 alias pk
       in case (pospk1, pospk2) of
            (Just _, Nothing) -> pospk1
            (Nothing, Just _) -> pospk2
            _ -> Nothing
extractPosPKExpr (AggFun _ _ expr) alias pk = extractPosPKExpr expr alias pk
extractPosPKExpr (Fun _ expr) alias pk = extractPosPKExpr expr alias pk

test_extractPosPKExpr :: Test
test_extractPosPKExpr =
  TestList
    [ extractPosPKExpr (Var $ VarName "order_id") (VarName "order_id") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Just (VarName "order_id", (VarName "order_id", IntType 16)),
      extractPosPKExpr (Val $ IntVal 1) (VarName "order_id") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Nothing,
      extractPosPKExpr (Op1 Neg $ Var $ VarName "order_id") (VarName "order") (Map.fromList [(VarName "order_id", IntType 16), (VarName "amount", IntType 32)]) ~?= Just (VarName "order_id", (VarName "order", IntType 16))
    ]

{- extractpospkexpr expr1 -}
extractAH :: [ColumnExpression] -> PrimaryKeys -> IndexName -> Map Var DType
extractAH ces pk iName =
  let ahMap = Map.fromList (NE.toList pk) `Map.union` Map.fromList iName
   in List.foldr (\x acc -> extractAHAux x ahMap `Map.union` acc) Map.empty ces
  where
    extractAHAux :: ColumnExpression -> Map Var DType -> Map Var DType
    extractAHAux AllVar ahMap = ahMap
    extractAHAux (ColumnName expr) ahMap =
      maybe Map.empty (Map.singleton (VarName $ TP.pretty expr)) $ inferExprType expr ahMap
    extractAHAux (ColumnAlias expr alias) ahMap =
      maybe Map.empty (Map.singleton (VarName $ TP.pretty alias)) $ inferExprType expr ahMap

evalSelectRowGroups :: (CountStyle, [ColumnExpression]) -> TableName -> PrimaryKeys -> IndexName -> PrimaryKeys -> IndexName -> [[Row]] -> SQLI Table
evalSelectRowGroups (cs, ce) tn pk iName newPK newIName rowGroups =
  Table newPK newIName
    <$> foldrM
      (\x acc -> (: acc) <$> evalSelectAux ce pk iName x)
      []
      rowGroups
  where
    evalSelectAux :: [ColumnExpression] -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalSelectAux ce pk iName td =
      foldM (\acc x -> evalColumnExpr x pk iName td <&> (acc `Map.union`)) Map.empty ce
    {- foldM (\acc x -> evalColumnExpr pk iName td x) (Map.empty) ce -}
    evalColumnExpr :: ColumnExpression -> PrimaryKeys -> IndexName -> [Row] -> SQLI Row
    evalColumnExpr (ColumnName expr) pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton (VarName $ TP.pretty expr) resValue
    evalColumnExpr (ColumnAlias expr alias) pk iName td = do
      exprNorm <- evalEAgg expr td
      resValue <- evalE exprNorm (List.head td)
      return $ Map.singleton (VarName alias) resValue
    evalColumnExpr AllVar pk iName td =
      return $ List.head td

evalCreateCommand :: CreateCommand -> SQLI Table
evalCreateCommand (CreateCommand ifnotexists namecreate idcreate) = do
  tableMaybe <- getTable namecreate
  table <- evalIDCreate idcreate
  if ifnotexists && isJust tableMaybe
    then return emptyTable
    else setScope namecreate table >> return table

evalIDCreate :: [(Name, DType, Bool)] -> SQLI Table
evalIDCreate idcreate = do
  pkList <- mapM parseIDCreate (List.filter (\(_, _, isPrimary) -> isPrimary) idcreate)
  iName <- mapM parseIDCreate (List.filter (\(_, _, isPrimary) -> not isPrimary) idcreate)
  return $ Table (NE.fromList pkList) iName []
  where
    parseIDCreate :: (Name, DType, Bool) -> SQLI (Var, DType)
    parseIDCreate (name, dtype, isPrimary) =
      case PAR.parse SPAR.varP name of
        Right var -> return (var, dtype)
        Left _ -> throwError "Fail to parse name"

evalDeleteCommand :: DeleteCommand -> SQLI Table
evalDeleteCommand (DeleteCommand fromdelete whdelete) = do
  delScope fromdelete
  return emptyTable

-- ******** Throw Errors ********

throwExpressionError :: Expression -> SQLI a
throwExpressionError expr = throwError $ "Illegal Op2: " ++ TP.pretty expr

throwCastError :: DValue -> DType -> SQLI a
throwCastError dval dtype = throwError $ "Illegal Casting into " ++ TP.pretty dtype ++ ": " ++ TP.pretty dval

-------- Testing Support --------
-- Empty data types
emptyScope :: Scope
emptyScope = Map.empty

emptyTableData :: TableData
emptyTableData = []

emptyIndexName :: IndexName
emptyIndexName = []

emptyTable :: Table
emptyTable =
  Table
    { primaryKeys = NE.singleton (VarName "default", BoolType),
      indexName = emptyIndexName,
      tableData = emptyTableData
    }

emptyRow :: Row
emptyRow = Map.empty

emptyStore :: Store
emptyStore = Store emptyScope Map.empty

-- Helper functions
tableFMap :: (Row -> Row) -> Table -> Table
tableFMap f t = t {tableData = fmap f (tableData t)}

tableMapEither :: (Row -> Either ErrorMsg Row) -> Table -> Either ErrorMsg Table
tableMapEither f t = do
  tm <-
    List.foldr
      ( \r n -> do
          n' <- n
          r' <- f r
          Right $ r' : n'
      )
      (Right emptyTableData)
      (tableData t)
  Right $ t {tableData = tm}

tableLength :: Table -> Int
tableLength = length . tableData
