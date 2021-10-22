{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement where

import AST (Ident, Stmt (..))
import Parser.Base (Parser, lexeme, pIdent, symbol)
import Parser.Expression (pExpr)

import Text.Megaparsec (between, choice, sepBy)

pLet :: Parser Stmt
pLet = Let <$> between (symbol "let") (symbol "=") (lexeme pIdent) <*> lexeme pExpr

pIdents :: Parser [Ident]
pIdents = pIdent `sepBy` symbol ","

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> lexeme pExpr

pStmt :: Parser Stmt
pStmt =
    choice
        [ pLet
        , pExprStmt
        ]