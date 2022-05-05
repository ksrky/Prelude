module AST where

type Name = String

data Expr
        = Var Name
        | Int Integer
        | UnOp Op Expr
        | BinOp Op Expr Expr
        | Assign Name Expr
        deriving (Eq, Ord, Show)

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord, Show)

newtype Prog = Prog [Expr] deriving (Eq, Show)
