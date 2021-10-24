module AST where

type Ident = String

data Expr
    = Var Ident
    | Int Int
    | Str String
    | Bool Bool
    | Not Expr
    | Negate Expr
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiple Expr Expr
    | Divide Expr Expr
    | Greater Expr Expr
    | Less Expr Expr
    | Equal Expr Expr
    | NotEqual Expr Expr
    | Call Ident [Expr]
    deriving (Eq, Ord, Show)

newtype Block = Block [Stmt] deriving (Show)
type Parameters = [Ident]

data Stmt
    = Let Ident Expr
    | Return Expr
    | If Expr Block Block
    | Function Parameters Block
    | ExprStmt Expr
    deriving (Show)

newtype Prog = Prog [Stmt] deriving (Show)
