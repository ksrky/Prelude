module Environment where

import AST (Block, Ident)

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)

data Function = Function {parameters :: [Ident], body :: Block, local :: Env}

instance Show Function where
    show f = "func (" ++ show params ++ ") {\n" ++ show stmts ++ "\n}"
      where
        params = parameters f
        stmts = body f

data Object
    = IntVal Int
    | StringVal String
    | BoolVal Bool
    | FunctionVal Function

instance Show Object where
    show (IntVal i) = show i
    show (StringVal s) = show s
    show (BoolVal b) = show b
    show (FunctionVal f) = show f

data Env = Env {outer :: Maybe Env, store :: M.Map Ident Object}

newEnv :: Env
newEnv = Env{outer = Nothing, store = M.empty}

newEnclosedEnv :: Env -> Env
newEnclosedEnv outer = Env{outer = Just outer, store = M.empty}

envGet :: Env -> Ident -> Maybe Object
envGet env name = case outer env of
    Just env' ->
        if isNothing mv
            then envGet env' name
            else mv
    Nothing -> mv
  where
    mv = M.lookup name (store env)

envSet :: Env -> Ident -> Object -> Env
envSet env name value = env{store = M.insert name value (store env)}
