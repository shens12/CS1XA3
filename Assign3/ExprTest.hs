{-|
    Module : ExprTest
    Description : Contains tests for the methods defined in
                  "ExprDiff" module
    Copyright : (c) Shanghong Shen @2018
    License : WTFPL
    Maintainer : shens12@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType","ExprDiff","ExprPretty","ExprParser" modules
-}

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


test1 :: Int -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
