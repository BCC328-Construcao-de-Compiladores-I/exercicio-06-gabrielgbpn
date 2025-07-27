module L.L2.Interpreter.Interp where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

type ImmutableEnv = Map Var Value
type MutableEnv = Map Var Value

valueToString :: Value -> String
valueToString (VInt n) = show n
valueToString (VStr s) = s

interpretAST :: L2 -> IO ()
interpretAST (L2 stmts) = do
  (_, _) <- interpBlock Map.empty Map.empty stmts
  return ()

interpBlock :: ImmutableEnv -> MutableEnv -> [S2] -> IO (ImmutableEnv, MutableEnv)
interpBlock phi sigma stmts = foldM interpStmt (phi, sigma) stmts

interpStmt :: (ImmutableEnv, MutableEnv) -> S2 -> IO (ImmutableEnv, MutableEnv)
interpStmt envs@(phi, sigma) stmt = case stmt of
  Def var expr block -> do
    val <- interpExpr envs expr
    let phi' = Map.insert var val phi
    (_, sigma') <- interpBlock phi' sigma block
    return (phi, sigma')

  LRead promptExpr var -> do
    promptVal <- interpExpr envs promptExpr
    case promptVal of
      VStr s -> do
        putStr s
        hFlush stdout
        input <- getLine
        case reads input of
          [(n, "")] -> return (phi, Map.insert var (VInt n) sigma)
          _ -> error $ "Entrada inválida: " ++ input
      _ -> error "Prompt deve ser uma string"

  LPrint expr -> do
    val <- interpExpr envs expr
    putStrLn (valueToString val)
    return (phi, sigma)

  LAssign var expr -> do
    val <- interpExpr envs expr
    return (phi, Map.insert var val sigma)

interpExpr :: (ImmutableEnv, MutableEnv) -> E2 -> IO Value
interpExpr (phi, sigma) expr = case expr of
  LVal v -> return v
  LVar var -> case Map.lookup var phi of
    Just val -> return val
    Nothing -> case Map.lookup var sigma of
      Just val -> return val
      Nothing -> error $ "Variável não definida: " ++ show var
  LAdd e1 e2 -> binOp (+) e1 e2
  LMinus e1 e2 -> binOp (-) e1 e2
  LMul e1 e2 -> binOp (*) e1 e2
  LDiv e1 e2 -> binOp div e1 e2
  where
    binOp op e1 e2 = do
      v1 <- interpExpr (phi, sigma) e1
      v2 <- interpExpr (phi, sigma) e2
      case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 `op` i2)
        _ -> error "Erro de tipo: operandos devem ser inteiros"