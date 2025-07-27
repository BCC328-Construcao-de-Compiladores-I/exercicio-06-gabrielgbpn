module Main where

import L.L2.Frontend.Lexer (lexer, Token(..), Lexeme(..))
import L.L2.Frontend.LALR (parseL2)
import L.L2.Frontend.Parser (l2Parser)
import L.L2.Frontend.TypeCheck (typeCheck)
import L.L2.Frontend.Syntax (L2(..), S2(..), E2(..))
import L.L2.Interpreter.Interp (interpretAST)
import L.L2.Backend.V1Codegen (v1Codegen)
import L.L2.Backend.CCodegen (cCodegen)
import Utils.Pretty (render, ppr, Pretty(..), text, vcat, (<+>), comma, semi, integer, parens, ($+$))
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import System.Environment
import System.FilePath
import System.Process
import System.IO


instance Pretty L2 where
  ppr (L2 stmts) = vcat (map ppr stmts)

instance Pretty S2 where
  ppr (Def var expr stmts) =
    text "def" <+> ppr var <+> text ":=" <+> ppr expr <+> text "in" $+$
    vcat (map ppr stmts) $+$
    text "end"
    
  ppr (LRead prompt var) =
    (text "read" <+> parens (ppr prompt <> comma <> ppr var)) <> semi
    
  ppr (LPrint expr) =
    (text "print" <+> parens (ppr expr)) <> semi
    
  ppr (LAssign var expr) =
    (ppr var <+> text ":=" <+> ppr expr) <> semi

instance Pretty E2 where
  ppr (LVal (VInt n)) = integer (fromIntegral n)
  ppr (LVal (VStr s)) = text (show s)
  ppr (LVar var) = ppr var
  ppr (LAdd e1 e2) = ppr e1 <+> text "+" <+> ppr e2
  ppr (LMinus e1 e2) = ppr e1 <+> text "-" <+> ppr e2
  ppr (LMul e1 e2) = ppr e1 <+> text "*" <+> ppr e2
  ppr (LDiv e1 e2) = ppr e1 <+> text "/" <+> ppr e2

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file]      -> lexerOnly file
  [Recursive file]  -> recursiveOnly file  
  [LALR file]       -> lalrOnly file 
  [Interpret file]  -> interpret file
  [VM file]         -> v1Compiler file
  [C file]          -> cCompiler file
  _                 -> helpMessage

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ printToken tokens
  where
    printToken (Token (line, col) lexeme) =
      putStrLn $ formatLexeme lexeme ++ " Linha:" ++ show line ++ " Coluna:" ++ show col

formatLexeme :: Lexeme -> String
formatLexeme lexeme = case lexeme of
  TIdent s   -> "Identificador " ++ s
  TNumber n  -> "Número " ++ show n
  TString s  -> "String " ++ s
  TDef       -> "Palavra reservada def"
  TIn        -> "Palavra reservada in"
  TEnd       -> "Palavra reservada end"
  TAssign    -> "Atribuição :="
  TPlus      -> "Operador +"
  TMinus     -> "Operador -"
  TTimes     -> "Operador *"
  TDiv       -> "Operador /"
  TLParen    -> "Parêntesis ("
  TRParen    -> "Parêntesis )"
  TSemicolon -> "Ponto e vírgula ;"
  TComma     -> "Vírgula ,"
  TRead      -> "Palavra reservada read"
  TPrint     -> "Palavra reservada print"
  TEOF       -> "Fim de arquivo"

recursiveOnly :: FilePath -> IO ()
recursiveOnly file = do
  content <- readFile file
  case l2Parser content of
    Left err  -> putStrLn $ "Erro de parsing (recursive): " ++ err
    Right ast -> putStrLn $ render (ppr ast)

lalrOnly :: FilePath -> IO ()
lalrOnly file = do
  content <- readFile file
  let tokens = lexer content
  case parseL2 tokens of
    Left err  -> putStrLn $ "Erro de parsing (LALR): " ++ err
    Right ast -> putStrLn $ render (ppr ast)

interpret :: FilePath -> IO ()
interpret file = do
  content <- readFile file
  case l2Parser content of
    Left err  -> putStrLn $ "Erro de parsing: " ++ err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn $ "Erro semântico: " ++ err
        Right checkedAst -> interpretAST checkedAst

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  content <- readFile file
  case l2Parser content of
    Left err  -> putStrLn $ "Erro de parsing: " ++ err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn $ "Erro semântico: " ++ err
        Right checkedAst -> do
          let code = v1Codegen checkedAst
          writeFile (replaceExtension file "v1") (render (ppr code))

cCompiler :: FilePath -> IO ()
cCompiler file = do
  content <- readFile file
  case l2Parser content of
    Left err  -> putStrLn $ "Erro de parsing: " ++ err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn $ "Erro semântico: " ++ err
        Right checkedAst -> cCodegen file checkedAst

helpMessage :: IO ()
helpMessage = putStrLn $ unlines
  [ "L2 language"
  , "Usage: l2 [--lexer-only | --recursive | --lalr | --interpret | --v1 | --c | --help]"
  , "--lexer-only: faz análise léxica e exibe os tokens"
  , "--recursive: faz análise sintática recursiva e exibe a AST"
  , "--lalr: faz análise sintática LALR e exibe a AST"
  , "--interpret: interpreta o programa após análises"
  , "--v1: gera código para a máquina virtual V1"
  , "--c: gera código C e compila com GCC"
  , "--help: mostra esta mensagem"
  ]

data Option
  = Help
  | Lexer FilePath
  | Recursive FilePath  
  | LALR FilePath  
  | Interpret FilePath
  | VM FilePath
  | C FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = case args of
  ("--lexer-only":file:_) -> [Lexer file]
  ("--recursive":file:_)  -> [Recursive file] 
  ("--lalr":file:_)       -> [LALR file]     
  ("--interpret":file:_)  -> [Interpret file]
  ("--v1":file:_)         -> [VM file]
  ("--c":file:_)          -> [C file]
  _                       -> [Help]