module REPL where

import Data.Text (Text, pack)
import Evaluator (evaluate)
import Parser (pExpr)
import Text.Megaparsec (runParser)

repl :: IO ()
repl = do
    putStr ">> "
    x <- getLine
    print (evaluate <$> runParser pExpr "" (pack x))
    repl
