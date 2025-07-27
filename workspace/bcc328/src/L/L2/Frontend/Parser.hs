module L.L2.Frontend.Parser where

import Control.Applicative hiding (many)  
import Control.Monad.Combinators (many)   
import Control.Monad.Combinators.Expr
import Data.Void
import L.L2.Frontend.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils.Value (Value(..))
import Utils.Var (Var(..))

type Parser = Parsec Void String

slexer :: Parser ()
slexer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol slexer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme slexer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdent :: Parser String
pIdent = lexeme $ (:) <$> letterChar <*> many alphaNumChar 

pNumber :: Parser Int
pNumber = lexeme L.decimal

pString :: Parser String
pString = lexeme $ between (char '"') (char '"') (many (noneOf ['"']))  -- Fixed

pTerm :: Parser E2
pTerm = choice
  [ LVal . VInt <$> pNumber
  , LVal . VStr <$> pString
  , LVar . Var <$> pIdent
  , parens pExpr
  ]

operatorTable :: [[Operator Parser E2]]
operatorTable =
  [ [ InfixL (LDiv <$ symbol "/") ]
  , [ InfixL (LMul <$ symbol "*") ]
  , [ InfixL (LAdd <$ symbol "+")
    , InfixL (LMinus <$ symbol "-") ]
  ]

pExpr :: Parser E2
pExpr = makeExprParser pTerm operatorTable

pDef :: Parser S2
pDef = do
  _ <- symbol "def"
  var <- Var <$> pIdent
  _ <- symbol ":="
  expr <- pExpr
  _ <- symbol "in"
  block <- many pStatement 
  _ <- symbol "end"
  return $ Def var expr block

pRead :: Parser S2
pRead = do
  _ <- symbol "read"
  _ <- symbol "("
  prompt <- pExpr
  _ <- symbol ","
  var <- Var <$> pIdent
  _ <- symbol ")"
  return $ LRead prompt var

pPrint :: Parser S2
pPrint = do
  _ <- symbol "print"
  _ <- symbol "("
  expr <- pExpr
  _ <- symbol ")"
  return $ LPrint expr

pAssign :: Parser S2
pAssign = do
  var <- Var <$> pIdent
  _ <- symbol ":="
  expr <- pExpr
  return $ LAssign var expr

pStatement :: Parser S2
pStatement = choice [pDef, try pRead, pPrint, pAssign] <* symbol ";"

pProgram :: Parser L2
pProgram = L2 <$> many pStatement <* eof  -- Fixed

l2Parser :: String -> Either String L2
l2Parser input = case parse pProgram "" input of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast