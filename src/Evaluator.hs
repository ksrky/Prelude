module Evaluator where

import AST
import qualified Data.Map.Strict as M
import Environment

evaluate :: Env -> Prog -> Maybe Int
evaluate = eval

class Eval a where
    eval :: Env -> a -> Maybe Int

instance Eval Prog where
    eval env (Prog ss) = eval env (last ss)

instance Eval Stmt where
    eval env (Let i e) = eval env e -- tmp
    eval env (ExprStmt e) = eval env e

instance Eval Expr where
    eval (Store s) (Var v) = M.lookup v s
    eval env (Int i) = Just i
    eval env (Negation n) = (0 -) <$> eval env n
    eval env (Sum l r) = (+) <$> eval env l <*> eval env r
    eval env (Subtr l r) = (-) <$> eval env l <*> eval env r
    eval env (Product l r) = (*) <$> eval env l <*> eval env r
    eval env (Division l r) = div <$> eval env l <*> eval env r
