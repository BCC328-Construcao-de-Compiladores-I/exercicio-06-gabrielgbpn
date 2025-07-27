{
module L.L2.Frontend.LALR where

import L.L2.Frontend.Lexer (Token(..), Lexeme(..))
import L.L2.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
}

%name parseL2
%tokentype { Token }
%monad { Either String } { (>>=) } { return }
%error { parseError }

%token
  IDENT   { Token _ (TIdent $$) }
  NUMBER  { Token _ (TNumber $$) }
  STRING  { Token _ (TString $$) }
  DEF     { Token _ TDef }
  IN      { Token _ TIn }
  END     { Token _ TEnd }
  ASSIGN  { Token _ TAssign }
  PLUS    { Token _ TPlus }
  MINUS   { Token _ TMinus }
  TIMES   { Token _ TTimes }
  DIV     { Token _ TDiv }
  LPAREN  { Token _ TLParen }
  RPAREN  { Token _ TRParen }
  SEMI    { Token _ TSemicolon }
  COMMA   { Token _ TComma }
  READ    { Token _ TRead }
  PRINT   { Token _ TPrint }

%left PLUS MINUS
%left TIMES DIV
%nonassoc LPAREN RPAREN
%expect 0

%%

L2 
  : Statements                     { L2 $1 }

Statements 
  : Statement Statements           { $1 : $2 }
  |                                { [] }

Statement 
  : Def                           { $1 }
  | LAssign                        { $1 }
  | LRead                          { $1 }
  | LPrint                         { $1 }

Def 
  : DEF IDENT ASSIGN Expr IN Statements END { Def (Var $2) $4 $6 }

LAssign 
  : IDENT ASSIGN Expr SEMI         { LAssign (Var $1) $3 }

LRead 
  : READ LPAREN Expr COMMA IDENT RPAREN SEMI  { LRead $3 (Var $5) }

LPrint 
  : PRINT LPAREN Expr RPAREN SEMI  { LPrint $3 }

Expr 
  : Expr PLUS Expr                 { LAdd $1 $3 }
  | Expr MINUS Expr                { LMinus $1 $3 }
  | Expr TIMES Expr                { LMul $1 $3 }
  | Expr DIV Expr                  { LDiv $1 $3 }
  | LPAREN Expr RPAREN             { $2 }
  | NUMBER                         { LVal (VInt $1) }
  | STRING                         { LVal (VStr $1) }
  | IDENT                          { LVar (Var $1) }

{
parseError :: [Token] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError ((Token (line, col) lexeme) : _) = 
  Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ show lexeme
}
