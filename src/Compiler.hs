{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler where

import qualified AST

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

compile :: AST.Prog -> Text
compile expr = ppllvm $
        buildModule "main" $ mdo
                form <- globalStringPtr "%d\n" "putNumForm"
                printf <- externVarArgs "printf" [ptr i8] i32
                function "main" [] i32 $ \[] -> mdo
                        entry <- block `named` "entry"
                        mdo
                                r <- toOperand expr
                                call printf [(ConstantOperand form, []), (r, [])]
                                ret (int32 0)

class LLVMOperand a where
        toOperand :: a -> LLVMBuilder Operand

instance LLVMOperand AST.Prog where
        toOperand (AST.Prog ss) = toOperand (head ss)

instance LLVMOperand AST.Stmt where
        toOperand (AST.ExprStmt e) = toOperand e
        toOperand (AST.LetStmt i e) = undefined

instance LLVMOperand Integer where
        toOperand n = return (int32 n)

zero :: AST.Expr
zero = AST.Int 0

instance LLVMOperand AST.Expr where
        toOperand (AST.Var i) = undefined
        toOperand (AST.Int i) = toOperand i
        toOperand (AST.Neg e) = binop sub zero e
        toOperand (AST.Add l r) = binop add l r
        toOperand (AST.Sub l r) = binop sub l r
        toOperand (AST.Mul l r) = binop mul l r
        toOperand (AST.Div l r) = binop sdiv l r

binop :: (Operand -> Operand -> LLVMBuilder Operand) -> AST.Expr -> AST.Expr -> LLVMBuilder Operand
binop f l r = do
        l' <- toOperand l
        r' <- toOperand r
        f l' r'
