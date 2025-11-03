{
module Grammars where

import Lex (Token(..),lexer)

}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenId $$ }
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMult }
      "<="            { TokenLeq }
      "not"           { TokenNot }
      '('             { TokenPA }
      ')'             { TokenPC }
      let             { TokenLet }
      letrec          { TokenLetRec }
      if0             { TokenIf0 }
      if              { TokenIf }
      lambda          { TokenLambda }

%%

SASA : var                                 { IdS $1 }
     | int                                 { NumS $1 }
     | bool                                { BooleanS $1 }

     | '(' '+'   SASA SASA ')'             { AddS $3 $4 }
     | '(' '-'   SASA SASA ')'             { SubS $3 $4 }
     | '(' '*'   SASA SASA ')'             { MulS $3 $4 }
     | '(' "<="  SASA SASA ')'             { LeqS $3 $4 }

     | '(' "not" SASA ')'                  { NotS $3 }

     | '(' let    '(' var SASA ')' SASA ')'    { LetS    $4 $5 $7 }
     | '(' letrec '(' var SASA ')' SASA ')'    { LetRecS $4 $5 $7 }

     | '(' if0 SASA SASA SASA ')'          { If0S $3 $4 $5 }
     | '(' if  SASA SASA SASA ')'          { IfS  $3 $4 $5 }

     | '(' lambda '(' var ')' SASA ')'     { FunS $4 $6 }

     | '(' SASA SASA ')'                   { AppS $2 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data SASA = IdS String
          | NumS Int
          | BooleanS Bool
          | AddS SASA SASA
          | SubS SASA SASA
          | MulS SASA SASA
          | LeqS SASA SASA
          | NotS SASA
          | LetS String SASA SASA
          | LetRecS String SASA SASA
          | If0S SASA SASA SASA
          | IfS  SASA SASA SASA
          | FunS String SASA
          | AppS SASA SASA 
          deriving(Show)
}
