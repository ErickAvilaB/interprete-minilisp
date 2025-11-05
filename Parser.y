{
module Parser where
import Lex (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  var       { TokenId $$ }
  int       { TokenNum $$ }
  bool      { TokenBool $$ }

  '('       { TokenPA }
  ')'       { TokenPC }
  '['       { TokenBRA }
  ']'       { TokenKET }
  ','       { TokenComma }

  '+'       { TokenPlus }
  '-'       { TokenMinus }
  '*'       { TokenTimes }
  '/'       { TokenDiv }

  '='       { TokenEq }
  '<'       { TokenLt }
  '>'       { TokenGt }
  "<="      { TokenLeq }
  ">="      { TokenGeq }
  "!="      { TokenNeq }

  "not"     { TokenNot }
  fst       { TokenFst }
  snd       { TokenSnd }
  head      { TokenHead }
  tail      { TokenTail }
  nil       { TokenNil }

  let       { TokenLet }
  "let*"    { TokenLetStar }
  letrec    { TokenLetRec }

  if0       { TokenIf0 }
  if        { TokenIf }
  lambda    { TokenLambda }

  cond      { TokenCond }
  else      { TokenElse }

  add1      { TokenAdd1 }
  sub1      { TokenSub1 }
  sqrt      { TokenSqrt }
  expt      { TokenExpt }

%%

-- Elementos básicos
SASA
  : var                                   { IdS $1 }
  | int                                   { NumS $1 }
  | bool                                  { BooleanS $1 }
  | nil                                   { NilS }

  -- pares y proyecciones
  | '(' SASA ',' SASA ')'                 { PairS $2 $4 }
  | '(' fst SASA ')'                      { FstS $3 }
  | '(' snd SASA ')'                      { SndS $3 }

  -- head/tail como alias de fst/snd
  | '(' head SASA ')'                     { FstS $3 }
  | '(' tail SASA ')'                     { SndS $3 }

  -- listas por corchetes
  | '[' ']'                               { ListS [] }
  | '[' ListElems ']'                     { ListS $2 }

  -- if / if0
  | '(' if SASA SASA SASA ')'             { IfS  $3 $4 $5 }
  | '(' if0 SASA SASA SASA ')'            { If0S $3 $4 $5 }

  -- cond
  | '(' cond '[' SASA SASA ']' '[' else SASA ']' ')' { CondS $4 $5 $9}

  -- operadores variádicos aritméticos
  | '(' '+'  SASA SASA MoreS ')'          { PrimNS Plus   ($3:$4:$5) }
  | '(' '-'  SASA SASA MoreS ')'          { PrimNS Minus  ($3:$4:$5) }
  | '(' '*'  SASA SASA MoreS ')'          { PrimNS Times  ($3:$4:$5) }
  | '(' '/'  SASA SASA MoreS ')'          { PrimNS Div    ($3:$4:$5) }

  -- comparadores variádicos
  | '(' '='  SASA SASA MoreS ')'          { PrimNS Eql    ($3:$4:$5) }
  | '(' '<'  SASA SASA MoreS ')'          { PrimNS Lt     ($3:$4:$5) }
  | '(' '>'  SASA SASA MoreS ')'          { PrimNS Gt     ($3:$4:$5) }
  | '(' "<=" SASA SASA MoreS ')'          { PrimNS Leq    ($3:$4:$5) }
  | '(' ">=" SASA SASA MoreS ')'          { PrimNS Geq    ($3:$4:$5) }
  | '(' "!=" SASA SASA MoreS ')'          { PrimNS Neq    ($3:$4:$5) }

  -- unarias / binarias especializadas
  | '(' "not" SASA ')'                    { NotS $3 }
  | '(' add1 SASA ')'                     { Add1S $3 }
  | '(' sub1 SASA ')'                     { Sub1S $3 }
  | '(' sqrt SASA ')'                     { SqrtS $3 }
  | '(' expt SASA SASA ')'                { ExptS $3 $4 }

  -- let / let* / letrec
  | '(' let Bindings SASA ')'             { LetManyS $3 $4 }
  | '(' "let*" Bindings SASA ')'          { LetStarManyS $3 $4 }
  | '(' letrec '(' var SASA ')' SASA ')'  { LetRecS $4 $5 $7 }

  -- lambda variádica y aplicación múltiple
  | '(' lambda '(' Params ')' SASA ')'    { FunManyS $4 $6 }
  | '(' SASA Args ')'                     { AppManyS $2 $3 }

-- Lista de más argumentos (≥ 0), usada para variádicos con al menos 2
MoreS
  :                                      { [] }
  | SASA MoreS                           { $1 : $2 }

-- Lista de expresiones (≥ 1) para aplicación
Args
  : SASA                                 { [$1] }
  | SASA Args                            { $1 : $2 }

-- Parámetros (≥1)
Params
  : var                                  { [$1] }
  | var Params                           { $1 : $2 }

-- Bindings ((x e) (y e) ...)
Bindings
  : '(' ')'                              { [] }
  | '(' BindList ')'                     { $2 }

BindList
  : '(' var SASA ')'                     { [($2,$3)] }
  | '(' var SASA ')' BindList            { ($2,$3) : $5 }

-- Listas por corchetes [e1, e2, ...]
ListElems
  : SASA                                 { [$1] }
  | SASA ',' ListElems                   { $1 : $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Superficie (SASA)
data SASA
  = IdS String
  | NumS Int
  | BooleanS Bool
  | NilS
  | PairS SASA SASA
  | FstS SASA
  | SndS SASA
  | ListS [SASA]

  | NotS SASA
  | Add1S SASA | Sub1S SASA | SqrtS SASA | ExptS SASA SASA

  | PrimNS PrimTag [SASA]     -- variádicos: + - * / = < > <= >= !=

  | LetManyS [(String,SASA)] SASA
  | LetStarManyS [(String,SASA)] SASA
  | LetRecS String SASA SASA

  | If0S SASA SASA SASA
  | IfS  SASA SASA SASA

  | CondS SASA SASA SASA

  | FunManyS [String] SASA
  | AppManyS SASA [SASA]
  deriving (Show)

data PrimTag = Plus | Minus | Times | Div | Eql | Lt | Gt | Leq | Geq | Neq | Sqrt | Expt
  deriving (Show, Eq)
}
