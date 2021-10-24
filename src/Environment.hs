module Environment where

import AST (Ident)
import Object

import qualified Data.Map.Strict as M

data Env = Env {outer :: Maybe Env, store :: M.Map Ident Object}

newEnv :: Env
newEnv = Env{outer = Nothing, store = M.empty}

newEnclosedEnv :: Env -> Env
newEnclosedEnv outer = Env{outer = Just outer, store = M.empty}

envGet :: Env -> Ident -> Maybe Object
envGet env name = M.lookup name (store env)

envSet :: Env -> Ident -> Object -> Env
envSet env name value = env{store = M.insert name value (store env)}
