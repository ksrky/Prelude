{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler where

import qualified AST as A

import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Text.Internal.Lazy

import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.Pretty

import Control.Monad.State
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)

type SymbolTable = M.Map A.Name Operand

compile :: A.Prog -> Text
compile exps = ppllvm $
        buildModule "main" $ mdo
                form <- globalStringPtr "%d\n" "putNumForm"
                printf <- externVarArgs "printf" [ptr i8] i32
                function "main" [] i32 $ \[] -> mdo
                        entry <- block `named` "entry"
                        r <- toOperand exps `evalStateT` M.empty
                        call printf [(ConstantOperand form, []), (r, [])]
                        ret (int32 0)

class LLVMOperand a where
        toOperand :: a -> StateT SymbolTable LLVMBuilder Operand

instance LLVMOperand A.Prog where
        toOperand (A.Prog es) = last <$> mapM toOperand es

instance LLVMOperand Integer where
        toOperand n = return (int32 n)

instance LLVMOperand A.Expr where
        toOperand (A.Var n) = do
                env <- get
                case M.lookup n env of
                        Just oper -> return oper
                        Nothing -> error $ "Unknown variable: " ++ n
        toOperand (A.Int i) = toOperand i
        toOperand (A.UnOp A.Minus e) = binop sub (A.Int 0) e
        toOperand (A.UnOp _ _) = undefined
        toOperand (A.BinOp A.Plus l r) = binop add l r
        toOperand (A.BinOp A.Minus l r) = binop sub l r
        toOperand (A.BinOp A.Times l r) = binop mul l r
        toOperand (A.BinOp A.Divide l r) = binop sdiv l r
        toOperand (A.Assign n e) = do
                e' <- toOperand e
                env <- get
                put $ M.insert n e' env
                return e'

binop :: (Operand -> Operand -> StateT SymbolTable LLVMBuilder Operand) -> A.Expr -> A.Expr -> StateT SymbolTable LLVMBuilder Operand
binop f l r = do
        l' <- toOperand l
        r' <- toOperand r
        f l' r'
