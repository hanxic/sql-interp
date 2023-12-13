module Utils where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import SQLSyntax
import TableSyntax

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase Fun {} = True
isBase AggFun {} = True
isBase _ = False

level :: Bop -> Int
level Times = 20
level Divide = 20
level Modulo = 20
level Plus = 17
level Minus = 17
level Eq = 15
level Gt = 15
level Ge = 15
level Lt = 15
level Le = 15
level Is = 15
level Like = 15
level And = 8
level Or = 7

------------ Type Checker --------
dvalueTypeCheck :: DValue -> DType -> Bool
dvalueTypeCheck NullVal _ = True
dvalueTypeCheck (IntVal _) (IntType _) = True
dvalueTypeCheck (BoolVal _) BoolType = True
dvalueTypeCheck (StringVal _) (StringType _) = True
dvalueTypeCheck _ _ = False

typeCheck :: DType -> DType -> Bool
typeCheck (IntType _) (IntType _) = True
typeCheck (StringType _) (StringType _) = True
typeCheck BoolType BoolType = True
typeCheck _ _ = False

inferExprType :: Expression -> Map Var DType -> Maybe DType
inferExprType (Var v) ahMap =
  Map.lookup v ahMap
    <|> Nothing
inferExprType (Val v) _ = inferDValueType v
inferExprType (Op1 uop expr) ahMap =
  let dtype1 = inferExprType expr ahMap
   in inferUopType uop dtype1
inferExprType (Op2 expr1 bop expr2) ahMap =
  let dtype1 = inferExprType expr1 ahMap
   in let dtype2 = inferExprType expr2 ahMap
       in inferBopType dtype1 bop dtype2
inferExprType (AggFun aggF _ expr) ahMap =
  let dtypeAgg = inferAggFunctionType aggF
   in do
        dtypeExpr <- inferExprType expr ahMap
        if typeCheck dtypeAgg dtypeExpr then return dtypeAgg else Nothing
inferExprType (Fun f expr) ahMap =
  let dtypeFun = inferFunctionType f
   in do
        dtypeExpr <- inferExprType expr ahMap
        if typeCheck dtypeFun dtypeExpr then return dtypeFun else Nothing

castable :: DType -> DType -> Bool
castable (IntType _) (StringType _) = False
castable (BoolType) (StringType _) = False
castable (StringType _) (IntType _) = False
castable (StringType _) (BoolType) = False
castable _ _ = True

inferUopType :: Uop -> Maybe DType -> Maybe DType
inferUopType _ mdtype = do
  dtype <- mdtype
  guard (not $ castable dtype (StringType 255))
  return dtype

inferBopType :: Maybe DType -> Bop -> Maybe DType -> Maybe DType
inferBopType mdtype1 bop mdtype2 | level bop >= 17 = do
  dtype1 <- mdtype1
  dtype2 <- mdtype2
  guard (castable dtype1 (IntType 32))
  guard (castable dtype2 (IntType 32))
  return dtype1
inferBopType mdtype1 bop mdtype2 | level bop >= 15 && bop /= Like = do
  dtype1 <- mdtype1
  dtype2 <- mdtype2
  return dtype1
inferBopType mdtype1 Like mdtype2 = do
  dtype1 <- mdtype1
  dtype2 <- mdtype2
  guard (castable dtype1 (StringType 255))
  guard (castable dtype2 (StringType 255))
  return dtype1
inferBopType mdtype1 _ mdtype2 = do
  dtype1 <- mdtype1
  dtype2 <- mdtype2
  guard (castable dtype1 BoolType)
  guard (castable dtype2 BoolType)
  return dtype1

inferDValueType :: DValue -> Maybe DType
inferDValueType (IntVal _) = Just $ IntType 32
inferDValueType (BoolVal _) = Just BoolType
inferDValueType (StringVal _) = Just $ StringType 255
inferDValueType NullVal = Nothing

inferAggFunctionType :: AggFunction -> DType
inferAggFunctionType f =
  IntType 32 -- TODO: Add more types when more functions are added

inferFunctionType :: Function -> DType
inferFunctionType f =
  IntType 32 -- TODO: Add more types when more functions are added
