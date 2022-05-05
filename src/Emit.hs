{-# LANGUAGE OverloadedStrings #-}

module Emit where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Control.Applicative
import Control.Monad.Except
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as B.Short
import qualified Data.Map.Strict as M

import qualified AST as A
import Codegen

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name $ B.Short.toShort $ C.pack x))

codegenTop :: A.Expr -> LLVM ()
codegenTop (A.Assign name expr) = do
    defineVar double name val
  where
    val = uncons $ evalCodegen $ cgen expr
codegenTop exp = do
    define double "main" [] blks
  where
    blks = createBlocks $
        execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

binops :: M.Map A.Op (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops =
    M.fromList
        [ (A.Plus, fadd)
        , (A.Minus, fsub)
        , (A.Times, fmul)
        , (A.Divide, fdiv)
        -- , ("<", lt)
        ]

cgen :: A.Expr -> Codegen AST.Operand
cgen (A.UnOp op a) = do
    cgen $ A.BinOp op (A.Int 0) a
cgen (A.BinOp op a b) = do
    case M.lookup op binops of
        Just f -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> error "No such operator"
cgen (A.Var x) = getvar x >>= load --call (externf (AST.Name $ B.Short.toShort $ C.pack x)) []
cgen (A.Int n) = return $ cons $ C.Int 0 n
cgen _ = error ""