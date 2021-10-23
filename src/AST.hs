module AST where

type Ident = String
type Number = Int

data Expr
    = Var Ident
    | Int Number
    | Not Expr
    | Negation Expr
    | Sum Expr Expr
    | Subtr Expr Expr
    | Product Expr Expr
    | Division Expr Expr
    | Greater Expr Expr
    | Less Expr Expr
    | Equal Expr Expr
    | NotEqual Expr Expr
    | Call Ident [Expr]
    deriving (Eq, Ord, Show)

type Block = [Stmt]
type Parameters = [Ident]

data Stmt
    = Let Ident Expr
    | Return Expr
    | If Expr Block Block
    | Function Parameters Block
    | ExprStmt Expr
    deriving (Show)

newtype Prog = Prog [Stmt] deriving (Show)
