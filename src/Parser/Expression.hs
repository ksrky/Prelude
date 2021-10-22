{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import AST (Expr (..))
import Parser.Base (Parser, lexeme, pIdent, symbol)

import Control.Monad.Combinators.Expr (
    Operator (InfixL, Postfix, Prefix),
    makeExprParser,
 )
import Data.Text (Text)
import Text.Megaparsec (between, choice)
import qualified Text.Megaparsec.Char.Lexer as L

pVariable :: Parser Expr
pVariable = Var <$> lexeme pIdent

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm =
    choice
        [ parens pExpr
        , pVariable
        , pInteger
        ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [ prefix "-" Negation
        , prefix "+" id
        ]
    ,
        [ binary "*" Product
        , binary "/" Division
        ]
    ,
        [ binary "+" Sum
        , binary "-" Subtr
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)