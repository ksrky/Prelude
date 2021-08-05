module Environment where

import AST (Ident)
import qualified Data.Map.Strict as M

newtype Env = Store (M.Map Ident Int) deriving (Show)

newEnv :: Env
newEnv = Store M.empty

envGet :: Env -> Ident -> Maybe Int
envGet (Store s) name = M.lookup name s

envSet :: Env -> Ident -> Int -> Env
envSet (Store s) name value = Store (M.insert name value s)
