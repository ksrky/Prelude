module Evaluator where

import AST

evaluate :: Prog -> Int
evaluate = eval

class Eval a where
    eval :: a -> Int

instance Eval Prog where
    eval (Prog ss) = eval (last ss)

instance Eval Stmt where
    eval (Let i e) = eval e
    eval (ExprStmt e) = eval e

instance Eval Expr where
    eval (Var v) = 1
    eval (Int i) = i
    eval (Negation n) = - eval n
    eval (Sum l r) = eval l + eval r
    eval (Subtr l r) = eval l - eval r
    eval (Product l r) = eval l * eval r
    eval (Division l r) = eval l `div` eval r
