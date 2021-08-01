module REPL where

import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import Environment
import Evaluator (evaluate)
import Parser (pProg)
import Text.Megaparsec (runParser)

start :: IO ()
start = do
    repl newEnv

repl :: Env -> IO ()
repl env = do
    putStr ">> "
    x <- getLine
    print (evaluate env <$> runParser pProg "" (pack x))
    repl env
