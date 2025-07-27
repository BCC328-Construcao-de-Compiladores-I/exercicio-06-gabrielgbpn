module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import V.V1.Instr (Instr(..), Code) 
import Utils.Value (Value(..))
import Utils.Var (Var(..))

v1Codegen :: L2 -> Code
v1Codegen (L2 stmts) = concatMap genStmt stmts

genStmt :: S2 -> Code
genStmt stmt = case stmt of
  Def var expr block -> genExpr expr ++ [Store var] ++ genBlock block
  LAssign var expr -> genExpr expr ++ [Store var]
  LRead prompt var -> genExpr prompt ++ [Print, Store var]
  LPrint expr -> genExpr expr ++ [Print]

genBlock :: [S2] -> Code
genBlock = concatMap genStmt

genExpr :: E2 -> Code
genExpr expr = case expr of
  LVal (VInt n) -> [Push (VInt n)] 
  LVal (VStr s) -> [Push (VStr s)] 
  LVar var -> [Load var]
  LAdd e1 e2 -> genExpr e1 ++ genExpr e2 ++ [Add]
  LMinus e1 e2 -> genExpr e1 ++ genExpr e2 ++ [Sub]
  LMul e1 e2 -> genExpr e1 ++ genExpr e2 ++ [Mul]
  LDiv e1 e2 -> genExpr e1 ++ genExpr e2 ++ [Div]