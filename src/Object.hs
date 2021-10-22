module Object where

import AST

data Object
    = Number Number
    | String String

instance Show Object where
    show (Number n) = show n
    show (String s) = show s
