module Evaluator where

import AST
import Control.Monad.State
import Environment

evaluate :: Prog -> StateT Env Maybe Int
evaluate = eval

class Eval a where
    eval :: a -> StateT Env Maybe Int

instance Eval Prog where
    eval (Prog es) = do
        res <- mapM eval es
        return $ last res

instance Eval Expr where
    eval (Var i) = undefined
    eval (Int i) = return i
    eval (Neg e) = eval e
    eval (Add l r) = binop (+) l r
    eval (Sub l r) = binop (-) l r
    eval (Mul l r) = binop (*) l r
    eval (Div l r) = binop div l r

binop :: (Int -> Int -> Int) -> Expr -> Expr -> StateT Env Maybe Int
binop f l r = do
    l' <- eval l
    r' <- eval r
    return (f l' r')
