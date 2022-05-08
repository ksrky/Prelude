{-# LANGUAGE GADTs #-}

module Environment where

import AST
import Data.Kind (Type)
import qualified Data.Map.Strict as M

data Env = Env {table :: M.Map Name Integer, outer :: Env} | Outermost

newEnv :: Env -> Env
newEnv e = Env{table = M.empty, outer = e}

topEnv :: Env
topEnv = newEnv Outermost

look :: Env -> Name -> Maybe Integer
look env name = M.lookup name (table env)

isIn :: Name -> Env -> Bool
isIn name env = case look env name of
    Just _ -> True
    _ -> False

store :: Env -> Name -> Integer -> Env
store env name value = env{table = M.insert name value (table env)}
