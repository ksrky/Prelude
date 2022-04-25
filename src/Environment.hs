{-# LANGUAGE GADTs #-}

module Environment where

import AST
import Data.Kind (Type)
import qualified Data.Map.Strict as M

data Object = ObjInt Integer | ObjNull | ObjErr String deriving (Eq)

instance Show Object where
        show (ObjInt i) = show i
        show ObjNull = show ""
        show (ObjErr s) = s

newtype Env = Env {store :: M.Map Ident Object}

newEnv :: Env
newEnv = Env{store = M.empty}

envGet :: Env -> Ident -> Maybe Object
envGet env name = M.lookup name (store env)

envSet :: Env -> Ident -> Object -> Env
envSet env name value = env{store = M.insert name value (store env)}
