{
module L.L2.Frontend.Lexer where
}

%wrapper "posn"

$digit = 0-9
$letter = [a-zA-Z]

tokens :-
  $white+           ;
  "def"             { \p _ -> Token (posnLine p, posnCol p) TDef }
  "in"              { \p _ -> Token (posnLine p, posnCol p) TIn }
  "end"             { \p _ -> Token (posnLine p, posnCol p) TEnd }
  "read"            { \p _ -> Token (posnLine p, posnCol p) TRead }
  "print"           { \p _ -> Token (posnLine p, posnCol p) TPrint }
  ":="              { \p _ -> Token (posnLine p, posnCol p) TAssign }
  "+"               { \p _ -> Token (posnLine p, posnCol p) TPlus }
  "-"               { \p _ -> Token (posnLine p, posnCol p) TMinus }
  "*"               { \p _ -> Token (posnLine p, posnCol p) TTimes }
  "/"               { \p _ -> Token (posnLine p, posnCol p) TDiv }
  "("               { \p _ -> Token (posnLine p, posnCol p) TLParen }
  ")"               { \p _ -> Token (posnLine p, posnCol p) TRParen }
  ";"               { \p _ -> Token (posnLine p, posnCol p) TSemicolon }
  ","               { \p _ -> Token (posnLine p, posnCol p) TComma }
  $letter [$letter $digit]*   { \p s -> Token (posnLine p, posnCol p) (TIdent s) }
  [\-]? $digit+     { \p s -> Token (posnLine p, posnCol p) (TNumber (read s)) }
  \" [^\"]* \"      { \p s -> Token (posnLine p, posnCol p) (TString (init (tail s))) }
  <0> \0            { \p _ -> Token (posnLine p, posnCol p) TEOF }

{
data Token = Token (Int, Int) Lexeme
  deriving (Eq, Ord, Show)

data Lexeme
  = TIdent String
  | TNumber Int
  | TString String
  | TDef
  | TIn
  | TEnd
  | TAssign
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TLParen
  | TRParen
  | TSemicolon
  | TComma
  | TRead
  | TPrint
  | TEOF
  deriving (Eq, Ord, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

posnLine :: AlexPosn -> Int
posnLine (AlexPn _ line _) = line

posnCol :: AlexPosn -> Int
posnCol (AlexPn _ _ col) = col
}
