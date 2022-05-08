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

type LLVMBuilder = IRBuilderT ModuleBuilder

type SymbolTable = M.Map A.Name Operand

compile :: A.Prog -> Text
compile exps = ppllvm $
        buildModule "main" $ mdo
                form <- globalStringPtr "%d\n" "form"
                printf <- externVarArgs "printf" [ptr i8] i32
                function "main" [] i32 $ \[] -> mdo
                        entry <- block `named` "entry"
                        r <- toOperand exps `evalStateT` M.empty
                        call printf [(ConstantOperand form, []), (r, [])]
                        ret (int32 0)

define :: A.Name -> [(Type, ParameterName)] -> ModuleBuilder Operand
define name args = function (mkName name) args i32 $ \[] -> mdo
        entry <- block `named` "entry"
        ret (int32 0)

codegenTop :: A.Stmt -> ModuleBuilder Operand
codegenTop (A.VarDecl n e) = do
        undefined
codegenTop (A.FuncDecl n as b) = function (mkName n) [] i32 $ \[] -> mdo
        entry <- block `named` "entry"
        undefined
codegenTop (A.Assign n e) = undefined
codegenTop (A.ExprStmt e) = mdo
        form <- globalStringPtr "%d\n" "form"
        printf <- externVarArgs "printf" [ptr i8] i32
        function "main" [] i32 $ \[] -> mdo
                entry <- block `named` "entry"
                r <- toOperand e `evalStateT` M.empty -- tmp
                call printf [(ConstantOperand form, []), (r, [])]
                ret (int32 0)

cgen :: A.Expr -> StateT SymbolTable LLVMBuilder Operand
cgen (A.Var n) = do
        env <- get
        case M.lookup n env of
                Just oper -> return oper
                Nothing -> error $ "Unknown variable: " ++ n
cgen (A.Int i) = return (int32 i)
cgen (A.UnOp A.Minus e) = binop sub (A.Int 0) e
cgen (A.UnOp _ _) = undefined
cgen (A.BinOp A.Plus l r) = binop add l r
cgen (A.BinOp A.Minus l r) = binop sub l r
cgen (A.BinOp A.Times l r) = binop mul l r
cgen (A.BinOp A.Divide l r) = binop sdiv l r
cgen (A.Call n as) = undefined

class LLVMOperand a where
        toOperand :: a -> StateT SymbolTable LLVMBuilder Operand

instance LLVMOperand A.Prog where
        toOperand (A.Prog ss) = undefined --last <$> mapM toOperand ss

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
        toOperand (A.Call n as) = undefined

binop :: (Operand -> Operand -> StateT SymbolTable LLVMBuilder Operand) -> A.Expr -> A.Expr -> StateT SymbolTable LLVMBuilder Operand
binop f l r = do
        l' <- toOperand l
        r' <- toOperand r
        f l' r'
