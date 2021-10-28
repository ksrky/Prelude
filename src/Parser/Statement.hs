{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement where

import AST (Block (..), Ident, Stmt (..))
import Parser.Base (Parser, lexeme, pIdent, skip, symbol)
import Parser.BoolExpression (pBoolExpr)
import Parser.Expression (pExpr)

import Text.Megaparsec (between, choice, sepBy)

pLetStmt :: Parser Stmt
pLetStmt = LetStmt <$> between (symbol "let") (symbol "=") (lexeme pIdent) <*> pExpr

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$> skip (symbol "return") pExpr

pIfStmt :: Parser Stmt
pIfStmt =
    IfStmt <$> skip (symbol "if") (between (symbol "(") (symbol ")") pBoolExpr)
        <*> pBlock
        <*> skip (symbol "else") pBlock

pFunctionStmt :: Parser Stmt
pFunctionStmt = FunctionStmt <$> pIdent <*> between (symbol "(") (symbol ")") pParameters <*> pBlock

pParameters :: Parser [Ident]
pParameters = lexeme pIdent `sepBy` symbol ","

pBlock :: Parser Block
pBlock = Block <$> between (symbol "{") (symbol "}") (pStmt `sepBy` symbol ".")

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> lexeme pExpr

pStmt :: Parser Stmt
pStmt =
    choice
        [ pLetStmt
        , pReturnStmt
        , pIfStmt
        , pExprStmt
        ]