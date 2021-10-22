{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST (Prog (..), Stmt (..))
import Parser.Base (Parser, symbol)
import Parser.Statement (pStmt)
import Text.Megaparsec (endBy)

pProg :: Parser Prog
pProg = Prog <$> pStmt `endBy` symbol "."
