module AST where

data Expr
    = Var Ident
    | Int Number
    | Negation Expr
    | Sum Expr Expr
    | Subtr Expr Expr
    | Product Expr Expr
    | Division Expr Expr
    deriving (Eq, Ord, Show)

data Stmt
    = Let Ident Expr
    | ExprStmt Expr

newtype Prog = Prog [Stmt]

type Ident = String
type Number = Int