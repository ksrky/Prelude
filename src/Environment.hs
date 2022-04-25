module Environment where

import AST
import qualified Data.Map.Strict as M

newtype Env = Env {store :: M.Map Ident Int}

newEnv :: Env
newEnv = Env{store = M.empty}

envGet :: Env -> Ident -> Maybe Int
envGet env name = M.lookup name (store env)

envSet :: Env -> Ident -> Int -> Env
envSet env name value = env{store = M.insert name value (store env)}
