module AST where

type Ident = String

data Expr
    = Var Ident
    | Int Int
    | Str String
    | Negate Expr
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiple Expr Expr
    | Divide Expr Expr
    | Call Ident [Expr]
    deriving (Show)

data BoolExpr
    = Bool Bool
    | Greater Expr Expr
    | Less Expr Expr
    | Equal Expr Expr
    | NotEqual Expr Expr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    | Not BoolExpr
    deriving (Show)

newtype Block = Block [Stmt] deriving (Show)
type Parameters = [Ident]

data Stmt
    = LetStmt Ident Expr
    | ReturnStmt Expr
    | IfStmt BoolExpr Block Block
    | FunctionStmt Ident Parameters Block
    | ExprStmt Expr
    deriving (Show)

newtype Prog = Prog [Stmt] deriving (Show)
