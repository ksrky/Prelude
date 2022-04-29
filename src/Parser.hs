{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad.Combinators.Expr (
        Operator (InfixL, Postfix, Prefix),
        makeExprParser,
 )
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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

startBy :: (Alternative m) => m a -> m sep -> m [a]
startBy p sep = many (sep *> p)

pName :: Parser Name
pName = (:) <$> letterChar <*> many alphaNumChar <?> "<identifier>"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pVar :: Parser Expr
pVar = Var <$> lexeme pName

pInt :: Parser Expr
pInt = Int <$> lexeme L.decimal

pTerm :: Parser Expr
pTerm = try (parens pExpr) <|> pVar <|> pInt

pExpr :: Parser Expr
pExpr = lexeme $ try (makeExprParser pTerm operatorTable) <|> pAssign

pAssign :: Parser Expr
pAssign = Assign <$> pName <* symbol "=" <*> pExpr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
        [
                [ prefix "-" Neg
                , prefix "+" id
                ]
        ,
                [ binary "*" Mul
                , binary "/" Div
                ]
        ,
                [ binary "+" Add
                , binary "-" Sub
                ]
        ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

pProg :: Parser Prog
pProg = Prog <$> pExpr `sepEndBy1` symbol ";"

parseProg :: String -> Either (ParseErrorBundle Text Void) Prog
parseProg input = parse pProg "" (pack input)

parseError :: ParseErrorBundle Text Void -> String
parseError = errorBundlePretty
