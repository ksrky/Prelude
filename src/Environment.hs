{-# LANGUAGE GADTs #-}

module Environment where

import AST
import Data.Kind (Type)
import qualified Data.Map.Strict as M

newtype Env = Env {store :: M.Map Ident Integer}

newEnv :: Env
newEnv = Env{store = M.empty}

envGet :: Env -> Ident -> Maybe Integer
envGet env name = M.lookup name (store env)

envSet :: Env -> Ident -> Integer -> Env
envSet env name value = env{store = M.insert name value (store env)}
