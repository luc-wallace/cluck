module Codegen where

import Ast
import Sast
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified LLVM.AST as AST hiding (function)
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as AST
import qualified LLVM.IRBuilder.Constant as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import Semant hiding (Env)

newtype Env = Env {operands :: Map Identifier AST.Operand}

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

convType :: MonadState Env m => Type -> m AST.Type
convType t = case t of
  Void -> pure AST.void
  Short -> pure AST.i16
  Int -> pure AST.i32
  Long -> pure AST.i64
  Char -> pure AST.i8
  Float -> pure AST.float
  Double -> pure AST.double
  _ -> error $ "invalid type: " ++ show t

registerOperand :: MonadState Env m => Identifier -> AST.Operand -> m ()
registerOperand ident op = modify $ \env -> env {operands = Map.insert ident op (operands env)}

-- codegenProgram :: SProgram -> AST.Module
-- codegenProgram (SProgram decls) =
--   flip evalState (Env {operands = Map.empty}) $
--     L.buildModuleT "cluck" $
--       do
--         mapM_ codegenDecl decls
--         L.function "main" [] AST.i32 $ \_ -> do
--           expr <- codegenExpr (Int, SNumberLiteral 5)
--           L.ret expr

-- codegenDecl :: SDecl -> LLVM ()
-- codegenDecl (SFunctionDecl t ident args body) = mdo
--   registerOperand ident function

--   function <- do
--     rett <- convType t
--     body <- codegenStmt body
--     L.function (AST.mkName $ cs ident) (map mkArg args) rett $ codegenStmt body
--   pure ()
--   where
--     mkArg (t', ident') = (,) <$> convType t' *> pure (L.ParameterName (cs ident'))

codegenExpr :: SExpr -> Codegen AST.Operand
codegenExpr (Int, SNumberLiteral n) = pure $ L.int32 $ round n
codegenExpr (Long, SNumberLiteral n) = pure $ L.int64 $ round n
codegenExpr (Float, SNumberLiteral n) = pure $ L.double n
codegenExpr (_, SVariableExpr ident) = gets ((Map.! ident) . operands)
codegenExpr (Bool, SBoolLiteral b) = pure $ L.bit $ if b then 1 else 0
codegenExpr (Char, SCharLiteral c) = pure $ L.int8 $ fromIntegral $ fromEnum c
codegenExpr (t, SBinaryOp op lex rex) = do
  lhs <- codegenExpr lex
  rhs <- codegenExpr rex
  case op of
    Add -> case (fst lex, fst rex) of
      (Int, Int) -> L.add lhs rhs
      (Float, Float) -> L.fadd lhs rhs
      _ -> error "internal error: semant failed"
    Sub -> case (fst lex, fst rex) of
      (Int, Int) -> L.sub lhs rhs
      (Float, Float) -> L.fsub lhs rhs
      _ -> error "internal error: semant failed"
    Mul -> case t of
      Int -> L.mul lhs rhs
      Float -> L.fmul lhs rhs
      _ -> error "internal error: semant failed"
    Div -> case t of
      Int -> L.sdiv lhs rhs
      Float -> L.fdiv lhs rhs
      _ -> error "internal error: semant failed"
    EqTo -> case t of
      Int -> L.icmp IP.EQ lhs rhs
      Char -> L.icmp IP.EQ lhs rhs
      Float -> L.fcmp FP.OEQ lhs rhs
      _ -> error "internal error: semant failed"
    NtEqTo -> case t of
      Int -> L.icmp IP.NE lhs rhs
      Char -> L.icmp IP.NE lhs rhs
      Float -> L.fcmp FP.ONE lhs rhs
      _ -> error "internal error: semant failed"
    Lt -> case t of
      Int -> L.icmp IP.SLT lhs rhs
      Char -> L.icmp IP.ULT lhs rhs
      Float -> L.fcmp FP.OLT lhs rhs
      _ -> error "internal error: semant failed"
    LtOrEqTo -> case t of
      Int -> L.icmp IP.SLE lhs rhs
      Char -> L.icmp IP.ULE lhs rhs
      Float -> L.fcmp FP.OLT lhs rhs
      _ -> error "internal error: semant failed"
    Gt -> case t of
      Int -> L.icmp IP.SGT lhs rhs
      Char -> L.icmp IP.UGT lhs rhs
      Float -> L.fcmp FP.OGT lhs rhs
      _ -> error "internal error: semant failed"
    GtOrEqTo -> case t of
      Int -> L.icmp IP.SGE lhs rhs
      Char -> L.icmp IP.UGE lhs rhs
      Float -> L.fcmp FP.OGE lhs rhs
      _ -> error "internal error: semant failed"
    And -> L.and lhs rhs
    Or -> L.or lhs rhs
    _ -> error "internal error: semant failed"
codegenExpr (t, SUnaryOp op ex) = do
  ex' <- codegenExpr ex
  case op of
    Neg -> case t of
      Int -> L.sub (L.int32 0) ex'
      Float -> L.sub (L.double 0) ex'
      _ -> error "internal error: semant failed"
    Not -> case t of
      Bool -> L.xor ex' (L.bit 1)
      _ -> error "internal error: semant failed"
    _ -> error "internal error: semant failed"
codegenExpr _ = error "internal error: semant failed"

-- codegenStmt :: SStmt -> Codegen ()
-- codegenStmt (SExprStmt expr) = void $ codegenExpr expr
-- codegenStmt (SReturnStmt expr) = case expr of
--   (Void, _) -> L.retVoid
--   _ -> do
--     expr' <- codegenExpr expr
--     L.ret expr'
-- codegenStmt (SBlockStmt stmts) = mapM_ codegenStmt stmts
