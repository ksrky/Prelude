module REPL where

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import Environment (Env, newEnv)
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
    let result = evaluate env <$> runParser pProg "" (pack x)
        nextEnv
            | isLeft result = env
            | otherwise = nenv
          where
            Right nenv = snd <$> result
    if isLeft result
        then print result
        else print (fst <$> result)
    repl nextEnv
