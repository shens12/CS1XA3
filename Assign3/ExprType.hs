{--|
Module : ExprType
Description: Contains a typeclass definition which defines some built-in functions specific to the Expr type, as well as a getvars function to retrieve all the variables from an expression
Copyright: (c) Shanghong Shen @2018
License : WTFPL 
Maintainer : shens12@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

module ExprType where

import           Data.List

-- | A datatype for common numeric expression

data Expr a = Add (Expr a) (Expr a)     -- ^ Binary Add
            | Mult (Expr a) (Expr a)    -- ^ Binary Multiplication
            | Div (Expr a) (Expr a)     -- ^ Binary Division
            | Const a                   -- ^ Constant Value
            | Var String                -- ^ Variable Identifier
            | Sin (Expr a)              -- ^ Sin Function
            | Cos (Expr a)              -- ^ Cos Function
            | Log (Expr a)              -- ^ Log Function
            | Exp (Expr a)              -- ^ Exp Function
  deriving Eq


instance Show a => Show (Expr a) where
  -- | show expression in string
  show (Div e1 e2)  = parens (show e1) ++ " / " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " + " ++ parens (show e2)
  show (Var ss)     = ss
  show (Const x)    = show x
  show (Cos e)      = "cos" ++ parens (show e)
  show (Sin e)      = "sin" ++ parens (show e)
  show (Log e)      = "log" ++ parens (show e)
  show (Exp e)      = "exp" ++ parens (show e)

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | get all identifiers in expression
getVars :: Expr a -> [String]
getVars (Add e1 e2) = getVars e1 `union` getVars e2
getVars (Div e1 e2) = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e) = getVars e
getVars (Sin e) = getVars e
getVars (Log e) = getVars e
getVars (Exp e) = getVars e
getVars (Const _) = []
getVars (Var ident) = [ident]

-- | Tell whether a Expr is const value
isConst :: Expr a -> Bool
isConst (Const _) = True
isConst _         = False

-- | Get const value
getConst :: Expr a -> a
getConst (Const a) = a
getConst expr = error "not a const"

-- | negate a expression
negateExpr :: Floating a => Expr a -> Expr a
negateExpr = Mult (Const $ negate 1)

