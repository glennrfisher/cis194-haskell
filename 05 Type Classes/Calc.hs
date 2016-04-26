----------------------------------------------
-- CIS 194, Homework 5
-- Author: Glenn R. Fisher
-- Date: April 9, 2016
----------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import StackVM

import qualified Data.Map as M

-- Evaluate an expression.
--
-- > eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Parse and evaluate a string expression.
--
-- > evalStr "(2+3)*4" == Just 20
-- > evalStr "(2+3)*" == Nothing

evalStr :: String -> Maybe Integer
evalStr str =
    case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
        Just exp -> Just (eval exp)
        Nothing -> Nothing

-- Abstract the calculator operations
-- to operate on various types of data.

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- Support calculator operations with ExprT type.

instance Expr ExprT where
    lit x = ExprT.Lit x
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y

-- Support calculator operations with Integer type.

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

-- Support calculator operations with Bool type.
--     * Literals less than 0 are interpreted as False.
--     * All positive Integers are interpreted as True.
--     * Addition is logical or.
--     * Multiplication is logical and.

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

-- Support calculator operations with MinMax type.
--     * Addition is taken to be the max function.
--     * Multiplication is taken to be the min function.

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

-- Support calculator operations with Mod7 type.
--     * All values are in the range 0...6.
--     * All arithmetic is done modulo 7 (e.g. 5 + 3 = 1).

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- Test and demonstrate the calculator operations.

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Support calculator operations with StackVM.Program type.

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

-- Compile a program by converting a string with arithmetic
-- expressions to instructions for the custom stack-based CPU.
--
-- > compile "2*3+1" == Just [PushI 2,PushI 3,PushI 1,Add,Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Types that are instances of `HasVars`
-- have some notion of named variables.

class HasVars a where
    var :: String -> a

-- VarExprT is similar to ExprT, but with an
-- additional constructor to support variables.

data VarExprT = VLit Integer
              | VVar String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
    deriving (Show, Eq)

-- Support calculator operations with VarExprT type.

instance Expr VarExprT where
    lit x = VLit x
    add x y = VAdd x y
    mul x y = VMul x y

-- Support named variable operations with VarExprT type.

instance HasVars VarExprT where
    var x = VVar x

-- Support named variable operations with mappings.
-- In other words, variables can be interpreted as functions
-- from a mapping of variables to Integer values to (possibly)
-- Integer values.

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- Support calculator operations with mappings.
-- In other words, functions from a mapping of variables to
-- Integer values to (possibly) Integer values can be
-- interpreted as expressions (by passing along the
-- mapping to subexpressions and combining results
-- appropriately).

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = \_ -> Just x
    add f g = \map -> (+) <$> (f map) <*> (g map)
    mul f g = \map -> (*) <$> (f map) <*> (g map)

-- Evaluate an expression using a mapping from variable
-- names to their integer values.
--
-- > withVars [("x", 6)] $ add (lit 3) (var "x") == Just 9
-- > withVars [("x", 6)] $ add (lit 3) (var "y") == Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> (Maybe Integer)
withVars vs exp = exp $ M.fromList vs
