{-|
    Module : ExprParser
    Description : Contains parsers that take a string and 
                  output an 'Expr' datatype expression 
    Copyright : (c) Shanghong Shen @2018
    License : WTFPL
    Maintainer : shens12@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}

module ExprParser (parseExprD, parseExprF) where

import           ExprType

import           Control.Applicative          hiding (Const, many, (<|>))
import           Text.Parsec
import           Text.Parsec.String
import           Control.Monad (liftM)

-- | Parses a string into an Expr Double type
parseExprD :: String -> Expr Double
parseExprD ss = case parse pExpr "" ss of
                  Left err   -> error $ show err
                  Right pExpr -> pExpr
-- | Parses a string into an Expr Float type
parseExprF :: String -> Expr Float
parseExprF ss = case parse pExpr "" ss of
                  Left err   -> error $ show err
                  Right pExpr -> pExpr

-- | Expr parser. Expr is formed by pTerms seperated by '+' or '-'
pExpr :: (Floating a, Read a) => Parser (Expr a)
pExpr = chainl1 pTerm add
  where add = (char '+' >> return Add)
          <|> (char '-' >> return (\lhs rhs -> negateExpr (Add lhs rhs)))

-- | Term parser. Term is formed by pFacts seperated by '*' or '/'
pTerm :: (Floating a, Read a) => Parser (Expr a)
pTerm = chainl1 pFact mult
  where mult = (char '*' >> return Mult)
           <|> (char '/' >> return Div)

{- | Factor parser.
-}
pFact :: (Floating a, Read a) => Parser (Expr a)
 pFact = pFunc "cos" Cos
     <|> pFunc "sin" Sin
     <|> pFunc "log" Log
     <|> pFunc "exp" Exp
     <|> pVar
     <|> parenExpr
     <|> pFloat
     <|> pInteger

-- | parse a expression surround be paren
parenExpr :: (Floating a, Read a) => Parser (Expr a)
parenExpr = char '(' >> pExpr <* char ')'

-- | parse pFunctions calls
pFunc :: (Floating a, Read a) => String -> (Expr a -> Expr a) -> Parser (Expr a)
pFunc s pFunc = pFunc <$> (string s >> pFact)

-- | Variables consist of letters (at least one letter)
pVar :: Parser (Expr a)
pVar = liftM Var $ many1 letter

-- | Parse a pInteger in format xxx.xxx
pFloat :: (Floating a, Read a) => Parser (Expr a)
pFloat = try (Const . read <$> all)
    where int = minus <|> pInteger
          all = (++) <$> int <*> ((:) <$> char '.' <*> many digit)
          minus  = (:) <$> char '-' <*> pInteger
          pInteger = many1 digit

-- | Parse integral pIntegers and returns Floating
pInteger :: (Floating a, Read a) => Parser (Expr a)
pInteger = Const . read <$> (minus <|> pInteger)
    where minus  = (:) <$> char '-' <*> pInteger
          pInteger = many1 digit
