module Evaluator where

import AST
import Control.Monad.State
import Environment

evaluate :: Prog -> StateT Env Maybe Object
evaluate = eval

class Eval a where
        eval :: a -> StateT Env Maybe Object

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
                return ObjNull

instance Eval Expr where
        eval (Var i) = do
                env <- get
                let Just val = envGet env i
                return val
        eval (Int i) = return $ ObjInt i
        eval (Neg e) = eval e
        eval (Add l r) = binop (+) l r
        eval (Sub l r) = binop (-) l r
        eval (Mul l r) = binop (*) l r
        eval (Div l r) = binop div l r

binop :: (Integer -> Integer -> Integer) -> Expr -> Expr -> StateT Env Maybe Object
binop f l r = do
        l' <- eval l
        r' <- eval r
        return $ case (l', r') of
                (ObjInt l'', ObjInt r'') -> ObjInt (f l'' r'')
                _ -> ObjErr "error"
