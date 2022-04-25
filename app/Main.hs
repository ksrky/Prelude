module Main where

{-
import REPL (start)

main :: IO ()
main = start
-}

import Compiler (compile)
import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Parser (parseError, parseProg)
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension)

main :: IO ()
main = do
        args <- getArgs
        let srcPath = head args
        let distPath = replaceExtension srcPath ".ll"
        src <- T.readFile srcPath
        let result = parseProg (unpack src)
        case result of
                Right ast -> LT.writeFile distPath (compile ast)
                Left e -> putStrLn $ parseError e