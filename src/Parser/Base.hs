{-# LANGUAGE OverloadedStrings #-}

module Parser.Base where

import AST

import Control.Applicative (Alternative (many))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (try),
    Parsec,
    between,
    manyTill,
    sepBy,
    sepEndBy,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (
    alphaNumChar,
    char,
    letterChar,
    space1,
    string,
 )
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

skip :: Applicative m => m open -> m a -> m a
skip open p = open *> p

startBy :: (Alternative m) => m a -> m sep -> m [a]
startBy p sep = many (sep *> p)

pIdent :: Parser Ident
pIdent = (:) <$> letterChar <*> many alphaNumChar <?> "<ident>"

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

boolLiteral :: Parser Bool
boolLiteral =
    True <$ string "True"
        <|> False <$ string "False"