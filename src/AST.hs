module AST where

type Ident = String

data Expr
        = Var Ident
        | Int Integer
        | Neg Expr
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        deriving (Show)

newtype Prog = Prog [Expr] deriving (Show)
