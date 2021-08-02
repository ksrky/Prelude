module Evaluator where

import AST
import qualified Data.Map.Strict as M
import Environment

evaluate :: Env -> Prog -> (Maybe Int, Env)
evaluate = eval

class Eval a where
    eval :: Env -> a -> (Maybe Int, Env)

instance Eval Prog where
    eval env (Prog ss) = last (evalProg env ss)

evalProg :: Env -> [Stmt] -> [(Maybe Int, Env)]
evalProg env [] = []
evalProg env (s : ss) = result : evalProg nextEnv ss
  where
    result = eval env s
    nextEnv = snd result

instance Eval Stmt where
    eval env (Let i e) = (fst r, envSet (snd r) i v)
      where
        r = eval env e
        Just v = fst r
    eval env (ExprStmt e) = eval env e

instance Eval Expr where
    eval (Store s) (Var v) = (M.lookup v s, Store s)
    eval env (Int i) = (Just i, env)
    eval env (Negation e) = ((0 -) <$> fst (eval env e), env)
    eval env (Sum l r) = ((+) <$> fst (eval env l) <*> fst (eval env r), env)
    eval env (Subtr l r) = ((-) <$> fst (eval env l) <*> fst (eval env r), env)
    eval env (Product l r) = ((*) <$> fst (eval env l) <*> fst (eval env r), env)
    eval env (Division l r) = (div <$> fst (eval env l) <*> fst (eval env r), env)
