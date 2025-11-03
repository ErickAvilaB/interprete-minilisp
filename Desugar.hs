module Desugar where

import Parser

-- AST "desugared"
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA        
  | Leq ASA ASA        
  | Not ASA
  | If0 ASA ASA ASA    
  | If  ASA ASA ASA    
  | Fun String ASA
  | App ASA ASA
  deriving (Show)

-- AST de valores (lo que tu evaluador por pasos usa)
data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | MulV ASAValues ASAValues
  | LeqV ASAValues ASAValues
  | NotV ASAValues
  | If0V ASAValues ASAValues ASAValues
  | IfV  ASAValues ASAValues ASAValues
  | FunV String ASAValues
  | ExprV ASAValues [(String, ASAValues)]
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
  deriving(Show)

-- Traducción de la sintaxis superficial (SASA del parser) al ASA
desugar :: SASA -> ASA
desugar (IdS i)              = Id i
desugar (NumS n)             = Num n
desugar (BooleanS b)         = Boolean b
desugar (AddS i d)           = Add (desugar i) (desugar d)
desugar (SubS i d)           = Sub (desugar i) (desugar d)
desugar (MulS i d)           = Mul (desugar i) (desugar d)
desugar (LeqS i d)           = Leq (desugar i) (desugar d)
desugar (NotS e)             = Not (desugar e)
desugar (LetS p v c)         = App (Fun p (desugar c)) (desugar v)
desugar (LetRecS p v c)      = desugar (LetS p (AppS (IdS "Z") (FunS p v)) c)
desugar (If0S c t e)         = If0 (desugar c) (desugar t) (desugar e)
desugar (IfS  c t e)         = If  (desugar c) (desugar t) (desugar e)
desugar (FunS p c)           = Fun p (desugar c)
desugar (AppS f a)           = App (desugar f) (desugar a)

-- Traducción de ASA (ya desugared) a ASAValues (forma lista para evaluar)
desugarV :: ASA -> ASAValues
desugarV (Id i)              = IdV i
desugarV (Num n)             = NumV n
desugarV (Boolean b)         = BooleanV b
desugarV (Add i d)           = AddV (desugarV i) (desugarV d)
desugarV (Sub i d)           = SubV (desugarV i) (desugarV d)
desugarV (Mul i d)           = MulV (desugarV i) (desugarV d)
desugarV (Leq i d)           = LeqV (desugarV i) (desugarV d)
desugarV (Not e)             = NotV (desugarV e)
desugarV (If0 c t e)         = If0V (desugarV c) (desugarV t) (desugarV e)
desugarV (If  c t e)         = IfV  (desugarV c) (desugarV t) (desugarV e)
desugarV (Fun p c)           = FunV p (desugarV c)
desugarV (App f a)           = AppV (desugarV f) (desugarV a)
