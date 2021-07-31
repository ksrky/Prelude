{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST (Expr (..), Stmt)
import Control.Monad.Combinators.Expr (
    Operator (InfixL, Postfix, Prefix),
    makeExprParser,
 )
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
    Parsec,
    between,
    choice,
    many,
    manyTill,
    (<?>),
 )
import Text.Megaparsec.Char (
    alphaNumChar,
    char,
    letterChar,
    space1,
 )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Space & Comment
sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- String Literal
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- Numeral
integer :: Parser Integer
integer = lexeme L.decimal

-- Expression
pVariable :: Parser Expr
pVariable =
    Var
        <$> lexeme
            ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

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

-- Statement