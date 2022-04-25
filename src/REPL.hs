module REPL where

import Control.Monad.State
import Environment
import Evaluator
import Parser

import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputLine,
    outputStr,
    outputStrLn,
    runInputT,
 )

start :: IO ()
start = do
    putStrLn "Prelude CLI"
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
    let p = parseProg input
    case p of
        Left perr -> do
            outputStrLn $parseError perr
            repl env
        Right ast -> do
            let res = evaluate ast `runStateT` env
            case res of
                Nothing -> do
                    outputStrLn "evaluation error"
                    repl env
                Just (out, env') -> do
                    outputStrLn $ show out
                    repl env'
            return undefined

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
    outputStrLn "\t:q\tquit"
    outputStrLn "\t:h\tprint usage"