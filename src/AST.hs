module AST where

type Name = String

data Expr
        = Var Name
        | Int Integer
        | Neg Expr
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Assign Name Expr
        deriving (Eq, Show)

newtype Prog = Prog [Expr] deriving (Eq, Show)
