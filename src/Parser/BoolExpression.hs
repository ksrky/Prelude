{-# LANGUAGE OverloadedStrings #-}

module Parser.BoolExpression where

import AST (BoolExpr (..))
import Parser.Base (
    Parser,
    boolLiteral,
    lexeme,
    parens,
    skip,
    symbol,
 )
import Parser.Expression (pExpr)

import Control.Monad.Combinators.Expr (
    Operator (InfixL, Prefix),
    makeExprParser,
 )
import Data.Text (Text)
import Text.Megaparsec (choice)
import qualified Text.Megaparsec.Char.Lexer as L

pBool :: Parser BoolExpr
pBool = Bool <$> boolLiteral

pGreater :: Parser BoolExpr
pGreater = Greater <$> pExpr <*> skip (symbol ">") pExpr

pLess :: Parser BoolExpr
pLess = Less <$> pExpr <*> skip (symbol "<") pExpr

pEqual :: Parser BoolExpr
pEqual = Equal <$> pExpr <*> skip (symbol "==") pExpr

pNotEqual :: Parser BoolExpr
pNotEqual = NotEqual <$> pExpr <*> skip (symbol "!=") pExpr

pTerm :: Parser BoolExpr
pTerm =
    choice
        [ pBool
        , parens pBoolExpr
        , pGreater
        , pLess
        , pEqual
        , pNotEqual
        ]

pBoolExpr :: Parser BoolExpr
pBoolExpr = lexeme $ makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser BoolExpr]]
operatorTable =
    [
        [ prefix "!" Not
        ]
    ,
        [ binary "&" And
        , binary "|" Or
        ]
    ]

binary :: Text -> (BoolExpr -> BoolExpr -> BoolExpr) -> Operator Parser BoolExpr
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (BoolExpr -> BoolExpr) -> Operator Parser BoolExpr
prefix name f = Prefix (f <$ symbol name)