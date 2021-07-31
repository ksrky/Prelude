module Evaluator where

import AST

evaluate :: Expr -> Int
evaluate = eval

class Eval a where
    eval :: a -> Int

instance Eval Expr where
    eval (Var v) = 1
    eval (Int i) = i
    eval (Negation n) = - eval n
    eval (Sum l r) = eval l + eval r
    eval (Subtr l r) = eval l - eval r
    eval (Product l r) = eval l * eval r
    eval (Division l r) = eval l `div` eval r
