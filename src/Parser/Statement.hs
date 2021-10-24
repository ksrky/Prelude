{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement where

import AST
import Parser.Base (Parser, lexeme, pIdent, skip, startBy, symbol)
import Parser.Expression (pExpr)

import Text.Megaparsec (between, choice, sepBy)

pLet :: Parser Stmt
pLet = Let <$> between (symbol "let") (symbol "=") (lexeme pIdent) <*> pExpr

pReturn :: Parser Stmt
pReturn = Return <$> skip (symbol "return") pExpr

pIf :: Parser Stmt
pIf =
    If <$> skip (symbol "if") (between (symbol "(") (symbol ")") pExpr)
        <*> pBlock
        <*> skip (symbol "else") pBlock

pParameters :: Parser [Ident]
pParameters = pIdent `sepBy` symbol ","

pFunction :: Parser Stmt
pFunction = Function <$> skip (symbol "func") (between (symbol "(") (symbol ")") pParameters) <*> pBlock

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> lexeme pExpr

pBlock :: Parser Block
pBlock = Block <$> between (symbol "{") (symbol "}") (pStmt `sepBy` symbol ".")

pStmt :: Parser Stmt
pStmt =
    choice
        [ pLet
        , pReturn
        , pIf
        , pFunction
        , pExprStmt
        ]