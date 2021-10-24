{-# LANGUAGE FlexibleInstances #-}

module Object where

import AST

data Object
    = IntVal Int
    | StringVal String
    | BoolVal Bool

instance Show Object where
    show (IntVal i) = show i
    show (StringVal s) = show s
    show (BoolVal b) = show b