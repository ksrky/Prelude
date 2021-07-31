module AST where

data Expr
    = Var String
    | Int Int
    | Negation Expr
    | Sum Expr Expr
    | Subtr Expr Expr
    | Product Expr Expr
    | Division Expr Expr
    deriving (Eq, Ord, Show)

data Stmt
    = Let String Expr
    | ExprStmt Expr