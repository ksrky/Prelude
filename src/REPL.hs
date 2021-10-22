module REPL where

import Environment (Env, newEnv)
import Evaluator
import Parser (pProg)

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputLine,
    outputStr,
    outputStrLn,
    runInputT,
 )
import Text.Megaparsec (errorBundlePretty, parse)

version :: String
version = "1.0.0"

start :: IO ()
start = do
    putStrLn ("Prelude CLI version " ++ version ++ ".")
    putStrLn "type :h to get help."
    runInputT defaultSettings (repl newEnv)

repl :: Env -> InputT IO ()
repl env = do
    minput <- getInputLine ">> "
    case minput of
        Nothing -> return ()
        Just input -> case input of
            ':' : cmd -> command env cmd
            _ -> repl' env input

repl' :: Env -> String -> InputT IO ()
repl' env input = do
    let evaluated = evaluate env <$> parse pProg "" (pack input)
    case evaluated of
        Left err -> do
            -- Parse Error
            outputStrLn $ errorBundlePretty err
            repl env
        Right res -> do
            case value res of
                Just v -> do
                    outputStrLn $ show v
                    repl $ environment res
                Nothing -> repl env

command :: Env -> String -> InputT IO ()
command env cmd = case cmd of
    "q" -> return ()
    "h" -> do
        printHelp
        repl env
    _ -> do
        outputStrLn ("unknown command ':" ++ cmd ++ "'")
        repl env

printHelp :: InputT IO ()
printHelp = do
    outputStrLn "Commands"
    outputStrLn "  :q      quit"
    outputStrLn "  :h      print usage"