module Main where

import REPL (repl)

main :: IO ()
main = do
    putStrLn "This is Prelude interpreter"
    repl
