{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Module : ExprDiff
    Description : Contains instance declarations
                  along with type class definition
                  for differentible expressions
    Copyright : (c) Shanghong Shen @2018
    License : WTFPL
    Maintainer : shens12@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}

module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

class DiffExpr a where
  -- | Evaluate an expression given var values
  eval :: Map.Map String a -> Expr a -> a
  -- | Simplify an expression and sub in values
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- | Perform partial differention w.r.t identifier
  partDiff :: String -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify emptyMap $ Add e1 e2

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify emptyMap $ Mult e1 e2

  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify emptyMap $ Div e1 e2

  val :: a -> Expr a
  val = Const
  var :: String -> Expr a
  var = Var

emptyMap :: (Ord k) => Map.Map k a
emptyMap = Map.fromList []

(!-) :: (Floating a, Eq a) => Expr a -> Expr a -> Expr a
e1 !- e2 = simplify emptyMap $ Add e1 $ negateExpr e2

instance (Floating a, Eq a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Div e1 e2)  = eval vrs e1 / eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                        Just v -> v
                        Nothing -> error "failed lookup in eval"
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Log e) = log (eval vrs e)
  eval vrs (Exp e) = exp (eval vrs e)

  -- | Convert Var to Const if it's in vrs
  simplify vrs v@(Var x) = maybe v Const $ Map.lookup x vrs
  -- | Const is keep unchanged
  simplify _ v@(Const _) = v
  simplify vrs (Mult f g)
    | lhs == Const 0 = Const 0
    | rhs == Const 0 = Const 0
    | lhs == Const 1 = rhs
    | rhs == Const 1 = lhs
    | isConst lhs && isConst rhs = Const $ getConst lhs * getConst rhs
    | otherwise = Mult lhs rhs
    where lhs = simplify vrs f
          rhs = simplify vrs g
  simplify vrs (Div f g)
    | lhs == Const 0 = Const 0
    | isConst lhs && isConst rhs = Const $ getConst lhs / getConst rhs
    | isConst rhs = Const (1 / getConst rhs) !* lhs
    | lhs == rhs = Const 1
    | otherwise = lhs `Div` rhs
    where lhs = simplify vrs f
          rhs = simplify vrs g
  simplify vrs (Add f g)
    | lhs == Const 0 = rhs  -- ^ lhs + 0 == lhs
    | rhs == Const 0 = lhs  -- ^ 0 + rhs == rhs
    | isConst lhs && isConst rhs = Const $ getConst lhs + getConst rhs -- ^ const folding
    | otherwise = Add lhs rhs -- ^ we can't simplify it
    where lhs = simplify vrs f
          rhs = simplify vrs g
  simplify vrs (Cos e)
    | isConst e' = Const $ cos $ getConst e'
    | otherwise = Cos e'
    where e' = simplify vrs e
  simplify vrs (Sin e)
    | isConst e' = Const $ sin $ getConst e'
    | otherwise = Sin e'
    where e' = simplify vrs e
  simplify vrs (Log e)
    | isConst e' = Const $ log $ getConst e'
    | otherwise = Log e'
    where e' = simplify vrs e
  simplify vrs (Exp e)
    | isConst e' = Const $ exp $ getConst e'
    | otherwise = Sin e'
    where e' = simplify vrs e

  partDiff v (Add f g)  = partDiff v f !+ partDiff v g
  partDiff v (Mult f g) = (partDiff v f !* g) !+ (f !* partDiff v g)
  partDiff v (Const c)  = Const 0
  partDiff v (Var x)    = Const $ if x == v then 1 else 0
  partDiff v (Cos e)    = negateExpr (Sin e) !* partDiff v e
  partDiff v (Sin e)    = Cos e !* partDiff v e
  partDiff v (Log e)    = Const 1 !/ e !* partDiff v e
  partDiff v (Exp e)    = Exp e !* partDiff v e
  partDiff v (Div f g)  = ((partDiff v f !* g) !- (partDiff v g !* f)) !/ Mult g g
