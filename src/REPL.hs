module REPL where

import Data.Text (Text, pack)
import Evaluator (evaluate)
import Parser (pProg)
import Text.Megaparsec (runParser)

repl :: IO ()
repl = do
    putStr ">> "
    x <- getLine
    print (evaluate <$> runParser pProg "" (pack x))
    repl
