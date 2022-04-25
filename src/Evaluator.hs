module Evaluator where

import AST
import Control.Monad.State
import Environment
import Error

evaluate :: Prog -> StateT Env (Either Error) Integer
evaluate = eval

class Eval a where
        eval :: a -> StateT Env (Either Error) Integer

instance Eval Prog where
        eval (Prog es) = do
                res <- mapM eval es
                return $ last res

instance Eval Stmt where
        eval (ExprStmt e) = eval e
        eval (LetStmt i e) = do
                val <- eval e
                env <- get
                put $ envSet env i val
                return val

instance Eval Expr where
        eval (Var i) = StateT $ \env -> do
                case envGet env i of
                        Just val -> return (val, env)
                        Nothing -> returnErr $ "variable not found: " ++ i
        eval (Int i) = return i
        eval (Neg e) = (0 -) <$> eval e
        eval (Add l r) = binop (+) l r
        eval (Sub l r) = binop (-) l r
        eval (Mul l r) = binop (*) l r
        eval (Div l r) = binop div l r

binop :: (Integer -> Integer -> Integer) -> Expr -> Expr -> StateT Env (Either Error) Integer
binop f l r = do
        l' <- eval l
        r' <- eval r
        return (f l' r')
