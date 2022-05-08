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

pName :: Parser Name
pName = (:) <$> letterChar <*> many alphaNumChar <?> "<identifier>"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-------------------------------------------------------------------
-- Expr
-------------------------------------------------------------------
pVar :: Parser Expr
pVar = Var <$> lexeme pName

pInt :: Parser Expr
pInt = Int <$> lexeme L.decimal

pCall :: Parser Expr
pCall = Call <$> pName <*> parens (pExpr `sepBy` symbol ",")

pTerm :: Parser Expr
pTerm = try (parens pExpr) <|> pVar <|> pInt

pExpr :: Parser Expr
pExpr = lexeme $ makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
        [
                [ prefix "-" (UnOp Minus)
                , prefix "+" id
                ]
        ,
                [ binary "*" (BinOp Times)
                , binary "/" (BinOp Divide)
                ]
        ,
                [ binary "+" (BinOp Plus)
                , binary "-" (BinOp Minus)
                ]
        ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-------------------------------------------------------------------
-- Stmt
-------------------------------------------------------------------
pVarDecl :: Parser Stmt
pVarDecl = VarDecl <$> (symbol "var" *> pName) <* symbol "=" <*> pExpr

pFuncDecl :: Parser Stmt
pFuncDecl = FuncDecl <$> (symbol "def" *> pName) <*> parens (pName `sepBy` symbol ",") <*> pExpr

pAssign :: Parser Stmt
pAssign = Assign <$> pName <* symbol "=" <*> pExpr

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr

pStmt :: Parser Stmt
pStmt = pVarDecl <|> pFuncDecl <|> pAssign

-------------------------------------------------------------------
-- Prog
-------------------------------------------------------------------
pProg :: Parser Prog
pProg = Prog <$> pStmt `sepEndBy1` symbol ";"

parseProg :: String -> Either (ParseErrorBundle Text Void) Prog
parseProg input = parse pProg "" (pack input)

parseError :: ParseErrorBundle Text Void -> String
parseError = errorBundlePretty
