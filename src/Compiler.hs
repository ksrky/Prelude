{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler where

import qualified AST as A

import Data.Functor.Identity
import Data.Text.Internal.Lazy

import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.Pretty

import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)

compile :: A.Prog -> Text
compile expr = ppllvm $
        buildModule "main" $ mdo
                form <- globalStringPtr "%d\n" "putNumForm"
                printf <- externVarArgs "printf" [ptr i8] i32
                function "main" [] i32 $ \[] -> mdo
                        entry <- block `named` "entry"
                        r <- toOperand expr
                        call printf [(ConstantOperand form, []), (r, [])]
                        ret (int32 0)

class LLVMOperand a where
        toOperand :: a -> LLVMBuilder Operand

instance LLVMOperand A.Prog where
        toOperand (A.Prog ss) = toOperand (head ss)

instance LLVMOperand Integer where
        toOperand n = return (int32 n)

instance LLVMOperand A.Expr where
        toOperand (A.Var i) = undefined
        toOperand (A.Int i) = toOperand i
        toOperand (A.Neg e) = binop sub (A.Int 0) e
        toOperand (A.Add l r) = binop add l r
        toOperand (A.Sub l r) = binop sub l r
        toOperand (A.Mul l r) = binop mul l r
        toOperand (A.Div l r) = binop sdiv l r
        toOperand (A.Assign i e) = undefined

binop :: (Operand -> Operand -> LLVMBuilder Operand) -> A.Expr -> A.Expr -> LLVMBuilder Operand
binop f l r = do
        l' <- toOperand l
        r' <- toOperand r
        f l' r'
