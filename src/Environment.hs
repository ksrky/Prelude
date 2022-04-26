{-# LANGUAGE GADTs #-}

module Environment where

import AST
import Data.Kind (Type)
import qualified Data.Map.Strict as M

newtype Env = Env {store :: M.Map Name Integer}

newEnv :: Env
newEnv = Env{store = M.empty}

envGet :: Env -> Name -> Maybe Integer
envGet env name = M.lookup name (store env)

envSet :: Env -> Name -> Integer -> Env
envSet env name value = env{store = M.insert name value (store env)}
