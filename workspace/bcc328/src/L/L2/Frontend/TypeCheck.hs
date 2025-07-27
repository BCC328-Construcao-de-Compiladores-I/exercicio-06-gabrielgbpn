module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List ((\\))
import qualified Data.Set as Set

import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value (Value(..)) 

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv
  = TcEnv {
      context :: Set.Set Var  
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv Set.empty

insertVar :: Var -> TcM ()
insertVar v = modify (\env -> env { context = Set.insert v (context env) })

removeVar :: Var -> TcM ()
removeVar v = modify (\env -> env { context = Set.delete v (context env) })

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m = runIdentity (runStateT (runWriterT (runExceptT m)) env)

typeCheck :: L2 -> Either String L2
typeCheck l2@(L2 stmts) = case runTcM initTcEnv (checkBlock stmts) of
  ((Left err, _), _) -> Left err
  ((Right (), _), _) -> Right l2

checkBlock :: [S2] -> TcM ()
checkBlock stmts = mapM_ checkStmt stmts

checkStmt :: S2 -> TcM ()
checkStmt stmt = do
  env <- gets context
  case stmt of
    Def var expr block -> do
      checkExpr expr
      insertVar var
      checkBlock block
      removeVar var

    LAssign var expr -> do
      if Set.member var env
        then throwError $ "Tentativa de reatribuir variável imutável: " ++ show var
        else checkExpr expr

    LRead prompt var -> do
      checkExpr prompt
      case prompt of
        LVal (VStr _) -> return () 
        _ -> throwError "Prompt de read deve ser uma string"
      if Set.member var env
        then throwError $ "Tentativa de reatribuir variável imutável: " ++ show var
        else return ()

    LPrint expr -> checkExpr expr

checkExpr :: E2 -> TcM ()
checkExpr expr = case expr of
  LVal _ -> return ()
  LVar _ -> return ()  
  LAdd e1 e2 -> checkExpr e1 >> checkExpr e2
  LMinus e1 e2 -> checkExpr e1 >> checkExpr e2
  LMul e1 e2 -> checkExpr e1 >> checkExpr e2
  LDiv e1 e2 -> checkExpr e1 >> checkExpr e2