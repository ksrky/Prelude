{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import AST (Expr (..))
import Parser.Base (
    Parser,
    boolLiteral,
    lexeme,
    pIdent,
    stringLiteral,
    symbol,
 )

import Control.Monad.Combinators.Expr (
    Operator (InfixL, Postfix, Prefix),
    makeExprParser,
 )
import Data.Text (Text)
import Text.Megaparsec (between, choice, sepBy)
import qualified Text.Megaparsec.Char.Lexer as L

pVariable :: Parser Expr
pVariable = Var <$> lexeme pIdent

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

pString :: Parser Expr
pString = Str <$> stringLiteral

pBool :: Parser Expr
pBool = Bool <$> boolLiteral

pCall :: Parser Expr
pCall = Call <$> lexeme pIdent <*> between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ",")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm =
    choice
        [ parens pExpr
        , pVariable
        , pInteger
        , pCall
        ]

pExpr :: Parser Expr
pExpr = lexeme $ makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [ prefix "!" Not
        , prefix "-" Negate
        , prefix "+" id
        ]
    ,
        [ binary "*" Multiple
        , binary "/" Divide
        ]
    ,
        [ binary "+" Add
        , binary "-" Subtract
        ]
    ,
        [ binary ">" Greater
        , binary "<" Less
        , binary "==" Equal
        , binary "!=" NotEqual
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)