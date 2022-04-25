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
        deriving (Eq, Show)

data Stmt
        = ExprStmt Expr
        | LetStmt Ident Expr
        deriving (Eq, Show)

newtype Prog = Prog [Stmt] deriving (Eq, Show)
