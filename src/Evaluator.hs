module Evaluator where

import AST
import qualified Data.Map.Strict as M
import Environment

data Result = Result {environment :: Env, value :: Maybe Object}

evaluate :: Env -> Prog -> Result
evaluate env = eval Result{environment = env, value = Nothing}

class Eval a where
    eval :: Result -> a -> Result

instance Eval Prog where
    eval res (Prog ss) = last (evalProg res ss)

evalProg :: Result -> [Stmt] -> [Result]
evalProg res [] = []
evalProg res (s : ss) = res' : evalProg res' ss
  where
    res' = eval res s

instance Eval Stmt where
    eval res (LetStmt i e) = case value $ eval res e of
        Just v -> res{environment = envSet (environment res) i v}
        Nothing -> res
    eval res (ReturnStmt e) = eval res e
    eval res (IfStmt e (Block b1) (Block b2)) = case value $ eval res e of
        Just (BoolVal b) ->
            if b
                then evalBlock res b1
                else evalBlock res b2
        _ -> res{value = Nothing}
    eval res (FunctionStmt i p b) = res{environment = envSet (environment res) i (FunctionVal f)}
      where
        f = Function{parameters = p, body = b, local = newEnclosedEnv (environment res)}
    eval res (ExprStmt e) = eval res e

instance Eval Block where
    eval res (Block ss) = evalBlock res ss

evalBlock :: Result -> [Stmt] -> Result
evalBlock res [] = res
evalBlock res (s : ss) = case s of
    ReturnStmt e -> eval res e
    _ -> evalBlock (eval res s) ss

instance Eval BoolExpr where
    eval res (Greater l r) = res{value = Just $ BoolVal (evalExpr env l > evalExpr env r)}
      where
        env = environment res
    eval res (Less l r) = res{value = Just $ BoolVal (evalExpr env l < evalExpr env r)}
      where
        env = environment res
    eval res (Equal l r) = res{value = Just $ BoolVal (evalExpr env l == evalExpr env r)}
      where
        env = environment res
    eval res (NotEqual l r) = res{value = Just $ BoolVal (evalExpr env l /= evalExpr env r)}
      where
        env = environment res
    eval res e = res{value = BoolVal <$> evalBoolExpr (environment res) e}

evalBoolExpr :: Env -> BoolExpr -> Maybe Bool
evalBoolExpr env (Bool b) = Just b
evalBoolExpr env (And l r) = (&&) <$> evalBoolExpr env l <*> evalBoolExpr env r
evalBoolExpr env (Or l r) = (||) <$> evalBoolExpr env l <*> evalBoolExpr env r
evalBoolExpr env (Not e) = not <$> evalBoolExpr env e
evalBoolExpr env _ = Nothing

instance Eval Expr where
    eval res (Str s) = res{value = Just $ StringVal s}
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
evalExpr env (Call i es) = case envGet env i of
    Just (FunctionVal f) -> error "" --evalBlock (environment f) b
    _ -> error "Noth"
evalExpr env _ = Nothing
