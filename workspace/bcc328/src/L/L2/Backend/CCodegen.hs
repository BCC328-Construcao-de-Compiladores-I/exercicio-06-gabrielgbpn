module L.L2.Backend.CCodegen where

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var
import System.FilePath
import System.Process
import Utils.Pretty
import Data.List (intercalate)

cCodegen :: FilePath -> L2 -> IO ()
cCodegen file (L2 stmts) = do
  let cCode = genCCode stmts
  let cFile = replaceExtension file "c"
  let exeFile = dropExtension file
  writeFile cFile cCode
  callCommand $ "gcc -o " ++ exeFile ++ " " ++ cFile

genCCode :: [S2] -> String
genCCode stmts = unlines
  [ "#include <stdio.h>"
  , "#include <stdlib.h>"
  , "int main() {"
  , "    char input[32];"
  , concatMap (indent . genStmt) stmts
  , "    return 0;"
  , "}"
  ]


indent :: String -> String
indent s = unlines [ "    " ++ line | line <- lines s ]

genStmt :: S2 -> String
genStmt stmt = case stmt of
  Def var expr block -> 
      "int " ++ varName var ++ " = " ++ genExpr expr ++ ";\n" ++ genBlock block
  LAssign var expr -> 
      "int " ++ varName var ++ " = " ++ genExpr expr ++ ";\n"  -- Added newline
  LRead prompt var -> 
      "printf(\"%s\", " ++ genExpr prompt ++ ");\n" ++
      "fgets(input, 32, stdin);\n" ++
      "int " ++ varName var ++ " = atoi(input);\n" 
  LPrint expr -> 
      "printf(\"%d\\n\", " ++ genExpr expr ++ ");\n" 


genBlock :: [S2] -> String
genBlock = intercalate "\n" . map genStmt

genExpr :: E2 -> String
genExpr expr = case expr of
  LVal (VInt n) -> show n
  LVal (VStr s) -> "\"" ++ s ++ "\""
  LVar var -> varName var
  LAdd e1 e2 -> "(" ++ genExpr e1 ++ " + " ++ genExpr e2 ++ ")"
  LMinus e1 e2 -> "(" ++ genExpr e1 ++ " - " ++ genExpr e2 ++ ")"
  LMul e1 e2 -> "(" ++ genExpr e1 ++ " * " ++ genExpr e2 ++ ")"
  LDiv e1 e2 -> "(" ++ genExpr e1 ++ " / " ++ genExpr e2 ++ ")"

varName :: Var -> String
varName (Var v) = v