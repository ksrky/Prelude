module AST where

type Name = String

data Expr
        = Var Name
        | Int Integer
        | UnOp Op Expr
        | BinOp Op Expr Expr
        | Call Name [Expr]
        deriving (Eq, Ord, Show)

data Stmt
        = VarDecl Name Expr
        | FuncDecl Name [Name] Expr
        | Assign Name Expr
        | ExprStmt Expr
        deriving (Eq, Show)

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord, Show)

newtype Prog = Prog [Stmt] deriving (Eq, Show)
