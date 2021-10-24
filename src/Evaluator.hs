module Evaluator where

import AST
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
    eval res (Return e) = eval res e
    eval res (If e (Block b1) (Block b2)) = case value $ eval res e of
        Just (BoolVal b) ->
            if b
                then evalBlock res b1
                else evalBlock res b2
        _ -> res{value = Nothing}
    eval res (Function p b) = error "not implemented"
    eval res (ExprStmt e) = eval res e

instance Eval Block where
    eval res (Block ss) = evalBlock res ss

evalBlock :: Result -> [Stmt] -> Result
evalBlock res [] = res
evalBlock res (s : ss) = case s of
    Return e -> eval res e
    _ -> evalBlock (eval res s) ss

instance Eval Expr where
    eval res (Str s) = res{value = Just $ StringVal s}
    eval res (Bool b) = res{value = Just $ BoolVal b}
    eval res (Greater e1 e2) = res{value = Just $ BoolVal (evalExpr env e1 > evalExpr env e2)}
      where
        env = environment res
    eval res (Less e1 e2) = res{value = Just $ BoolVal (evalExpr env e1 < evalExpr env e2)}
      where
        env = environment res
    eval res (Equal e1 e2) = res{value = Just $ BoolVal (evalExpr env e1 == evalExpr env e2)}
      where
        env = environment res
    eval res (NotEqual e1 e2) = res{value = Just $ BoolVal (evalExpr env e1 /= evalExpr env e2)}
      where
        env = environment res
    eval res e = res{value = IntVal <$> evalExpr (environment res) e}

evalExpr :: Env -> Expr -> Maybe Int
evalExpr env (Var i) = case envGet env i of
    Just (IntVal i) -> Just i
    _ -> Nothing
evalExpr env (Int i) = Just i
evalExpr env (Negate e) = (0 -) <$> evalExpr env e
evalExpr env (Add l r) = (+) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Subtract l r) = (-) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Multiple l r) = (*) <$> evalExpr env l <*> evalExpr env r
evalExpr env (Divide l r) = div <$> evalExpr env l <*> evalExpr env r
evalExpr env _ = Nothing
