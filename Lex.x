{
module Lex (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

$white  = [\x20\x09\x0A\x0D\x0C\x0B]
$digit  = 0-9
$letter = [A-Za-z_]
$idrest = [A-Za-z0-9_]

tokens :-

  -- comentarios estilo Lisp
  ";" [^\n]*                 ;

  -- espacios
  $white+                    ;

  -- paréntesis / corchetes / coma
  \(                         { \_ -> TokenPA }
  \)                         { \_ -> TokenPC }
  \[                         { \_ -> TokenBRA }
  \]                         { \_ -> TokenKET }
  \,                         { \_ -> TokenComma }

  -- operadores aritméticos
  \+                         { \_ -> TokenPlus }
  \-                         { \_ -> TokenMinus }
  \*                         { \_ -> TokenTimes }
  \/                         { \_ -> TokenDiv }

  -- comparadores
  ">="                       { \_ -> TokenGeq }
  "<="                       { \_ -> TokenLeq }
  "!="                       { \_ -> TokenNeq }
  "="                        { \_ -> TokenEq }
  "<"                        { \_ -> TokenLt }
  ">"                        { \_ -> TokenGt }

  -- palabras clave
  not                        { \_ -> TokenNot }
  fst                        { \_ -> TokenFst }
  snd                        { \_ -> TokenSnd }
  head                       { \_ -> TokenHead }
  tail                       { \_ -> TokenTail }
  nil                        { \_ -> TokenNil }

  letrec                     { \_ -> TokenLetRec }
  let\*                      { \_ -> TokenLetStar }
  let                        { \_ -> TokenLet }
  lambda                     { \_ -> TokenLambda }
  if0                        { \_ -> TokenIf0 }
  if                         { \_ -> TokenIf }
  cond                       { \_ -> TokenCond }
  else                       { \_ -> TokenElse }

  add1                       { \_ -> TokenAdd1 }
  sub1                       { \_ -> TokenSub1 }
  sqrt                       { \_ -> TokenSqrt }
  expt                       { \_ -> TokenExpt }

  "#t"                       { \_ -> TokenBool True }
  "#f"                       { \_ -> TokenBool False }

  $digit+                    { \s -> TokenNum (read s) }
  $letter$idrest*            { \s -> TokenId s }

  .                          { \s -> error ("Lexical error: caracter no reconocido = "
                                         ++ show s
                                         ++ " | codepoints = "
                                         ++ show (map fromEnum s)) }

{
data Token
  = TokenId String
  | TokenNum Int
  | TokenBool Bool
  -- parens / brackets / comma
  | TokenPA | TokenPC | TokenBRA | TokenKET | TokenComma
  -- arithmetic
  | TokenPlus | TokenMinus | TokenTimes | TokenDiv
  -- comparators
  | TokenEq | TokenLt | TokenGt | TokenLeq | TokenGeq | TokenNeq
  -- keywords / builtins
  | TokenNot | TokenFst | TokenSnd | TokenHead | TokenTail | TokenNil
  | TokenLet | TokenLetStar | TokenLetRec
  | TokenIf0 | TokenIf | TokenLambda
  | TokenCond | TokenElse
  | TokenAdd1 | TokenSub1 | TokenSqrt | TokenExpt
  deriving (Show)

normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c && c /= '\n' then '\x20' else c)

lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
