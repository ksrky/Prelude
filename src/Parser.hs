{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST (Prog (..), Stmt (..))
import Parser.Base (Parser, symbol)
import Parser.Statement (pStmt)

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (
    ParseErrorBundle,
    endBy,
    errorBundlePretty,
    parse,
    sepEndBy,
 )

parseProgram :: String -> Either (ParseErrorBundle Text Void) Prog
parseProgram input = parse pProg "" (pack input)

getParseError :: ParseErrorBundle Text Void -> String
getParseError = errorBundlePretty

pProg :: Parser Prog
pProg = Prog <$> pStmt `sepEndBy` symbol ";"
