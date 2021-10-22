module Evaluator where

import AST (Expr (..), Prog (..), Stmt (..))
import qualified Data.Map.Strict as M
import Environment (Env (..), envSet)

data Result = Result {environment :: Env, value :: Maybe Int}

evaluate :: Env -> Prog -> Result
evaluate env = eval Result{environment = env, value = Nothing}

class Eval a where
    eval :: Result -> a -> Result

instance Eval Prog where
    eval res (Prog ss) = last (evalProg res ss)

evalProg :: Result -> [Stmt] -> [Result]
evalProg res [] = []
evalProg res (s : ss) = res : evalProg res ss
  where
    res' = eval res s

instance Eval Stmt where
    eval res (Let i e) = case value res' of
        Just v -> res{environment = envSet (environment res') i v}
        Nothing -> res'
      where
        res' = eval res e
    eval res (ExprStmt e) = eval res e

instance Eval Expr where
    eval res (Var v) = Result{environment = Store s, value = M.lookup v s}
      where
        Store s = environment res
    eval res (Int i) = res{value = Just i}
    eval res (Negation e) = res{value = (0 -) <$> value (eval res e)}
    eval res (Sum l r) = res{value = (+) <$> value (eval res l) <*> value (eval res r)}
    eval res (Subtr l r) = res{value = (-) <$> value (eval res l) <*> value (eval res r)}
    eval res (Product l r) = res{value = (*) <$> value (eval res l) <*> value (eval res r)}
    eval res (Division l r) = res{value = div <$> value (eval res l) <*> value (eval res r)}
