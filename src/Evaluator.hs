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
        eval (VarDecl n e) = do
                val <- eval e
                env <- get
                put $ store env n val
                return val
        eval (FuncDecl n as b) = undefined
        eval (Assign n e) = do
                val <- eval e
                env <- get
                unless (n `isIn` env) (error $ "variable not found: " ++ n)
                put $ store env n val
                return val
        eval (ExprStmt e) = eval e

instance Eval Expr where
        eval (Var i) = StateT $ \env -> do
                case look env i of
                        Just val -> return (val, env)
                        Nothing -> returnErr $ "variable not found: " ++ i
        eval (Int i) = return i
        eval (UnOp Minus e) = (0 -) <$> eval e
        eval (UnOp _ _) = undefined
        eval (BinOp Plus l r) = binop (+) l r
        eval (BinOp Minus l r) = binop (-) l r
        eval (BinOp Times l r) = binop (*) l r
        eval (BinOp Divide l r) = binop div l r
        eval (Call n as) = undefined

binop :: (Integer -> Integer -> Integer) -> Expr -> Expr -> StateT Env (Either Error) Integer
binop f l r = do
        l' <- eval l
        r' <- eval r
        return (f l' r')
