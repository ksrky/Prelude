module Evaluator where

import AST (Expr (..), Number, Prog (..), Stmt (..))
import qualified Data.Map.Strict as M
import Environment
import Object

data Result = Result {environment :: Env, value :: Maybe Object}

evaluate :: Env -> Prog -> Result
evaluate env = eval Result{environment = env, value = Nothing}

class Eval a where
    eval :: Result -> a -> Result

instance Eval Prog where
    eval res (Prog ss) = last (evalProg res ss)

evalProg :: Result -> [Stmt] -> [Result]
evalProg res [] = []
evalProg res (s : ss) = res' : evalProg res ss
  where
    res' = eval res s

instance Eval Stmt where
    eval res (Let i e) = case value $ eval res e of
        Just v -> res{environment = envSet (environment res) i v}
        Nothing -> res
    eval res (ExprStmt e) = eval res e

instance Eval Expr where
    eval res e = res{value = Number <$> evalExpr (environment res) e}

evalExpr :: Env -> Expr -> Maybe Number
evalExpr env (Var i) = getNumber env i
evalExpr env (Int i) = Just i
evalExpr env (Negation e) = (0 -) <$> evalExpr env e
evalExpr env (Sum l r) = (+) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Subtr l r) = (-) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Product l r) = (*) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Division l r) = div <$> evalExpr env l <*> evalExpr env r