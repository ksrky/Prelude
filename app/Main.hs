module Main where

import Compiler (compile)
import Parser (parseError, parseProg)

import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT

import System.Console.Haskeline
import System.Environment
import System.FilePath.Posix (replaceExtension)
import System.IO

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                fname : _ -> process fname

repl :: IO ()
repl = undefined

process :: String -> IO ()
process fname = do
        let src = "examples/" ++ fname
            dist = replaceExtension src ".ll"
        input <- readFile src
        case parseProg input of
                Left err -> putStrLn $ parseError err
                Right exp -> LT.writeFile dist (compile exp)
