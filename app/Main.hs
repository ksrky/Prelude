module Main where

import REPL (start)

main :: IO ()
main = do
    putStrLn "This is Prelude interpreter"
    start
