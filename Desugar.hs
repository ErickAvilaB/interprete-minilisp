module Desugar where

-- Importamos Parser cualificado para usar su PrimTag y constructores (Plus, Minus, ...),
-- y al mismo tiempo traer los constructores de SASA sin colisiones.
import Parser hiding (PrimTag(..))
import qualified Parser as P

--------------------------------------------------------------------------------
-- Núcleo (ASA)
--------------------------------------------------------------------------------

-- ASA: core del lenguaje tras desazucarado
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Nil
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA

  -- Primitivas variádicas (se mantienen como lista en el núcleo)
  | PrimN P.PrimTag [ASA]

  | Not ASA
  | If0 ASA ASA ASA
  | If  ASA ASA ASA

  -- El núcleo es de lambdas unarias; lambdas variádicas se currifican
  | Fun String ASA
  | App ASA ASA
  deriving (Show)

--------------------------------------------------------------------------------
-- Forma para evaluar (ASAValues)
--------------------------------------------------------------------------------

data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | NilV
  | PairV ASAValues ASAValues
  | FstV ASAValues
  | SndV ASAValues

  | PrimNV P.PrimTag [ASAValues]  -- variádicos listados

  | NotV ASAValues
  | If0V ASAValues ASAValues ASAValues
  | IfV  ASAValues ASAValues ASAValues

  | FunV String ASAValues
  | ExprV ASAValues [(String, ASAValues)]
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
  deriving (Show)

--------------------------------------------------------------------------------
-- Azúcar (SASA) → Núcleo (ASA)
--------------------------------------------------------------------------------

desugar :: SASA -> ASA
-- básicos
desugar (IdS i)                 = Id i
desugar (NumS n)                = Num n
desugar (BooleanS b)            = Boolean b

-- nil y pares / listas
desugar NilS                    = Nil
desugar (PairS a b)             = Pair (desugar a) (desugar b)
desugar (FstS e)                = Fst (desugar e)
desugar (SndS e)                = Snd (desugar e)
desugar (ListS xs)              = foldr (\e acc -> Pair (desugar e) acc) Nil xs

-- unarias especiales y not
desugar (NotS e)                = Not (desugar e)
desugar (Add1S e)               = PrimN P.Plus  [desugar e, Num 1]
desugar (Sub1S e)               = PrimN P.Minus [desugar e, Num 1]
desugar (SqrtS e)               = PrimN P.Sqrt  [desugar e]
desugar (ExptS a b)             = PrimN P.Expt  [desugar a, desugar b]

-- variádicos (+ - * / = < > <= >= !=)
desugar (PrimNS t xs)           = PrimN t (map desugar xs)

-- if / if0
desugar (IfS  c t e)            = If  (desugar c) (desugar t) (desugar e)
desugar (If0S c t e)            = If0 (desugar c) (desugar t) (desugar e)

desugar (CondS c t e) = If  (desugar c) (desugar t) (desugar e)

-- let paralelo: ((lambda (x y ...) body) e1 e2 ...)
desugar (LetManyS binds body)   =
  let (ps, es) = unzip binds
   in appMany (funMany ps (desugar body)) (map desugar es)

-- let* secuencial: anidar lets de a uno
desugar (LetStarManyS [] body)  = desugar body
desugar (LetStarManyS ((x,e):xs) body) =
  App (Fun x (desugar (LetStarManyS xs body))) (desugar e)

-- letrec como en tu base, vía Z y lambda variádica singleton
desugar (LetRecS p v c)         =
  desugar (LetManyS [(p, AppManyS (IdS "Z") [FunManyS [p] v])] c)

-- lambdas variádicas → currificadas
desugar (FunManyS ps body)      = funMany ps (desugar body)

-- aplicación múltiple → aplicaciones anidadas
desugar (AppManyS f args)       = appMany (desugar f) (map desugar args)

--------------------------------------------------------------------------------
-- Helpers (azúcar del núcleo)
--------------------------------------------------------------------------------

-- f a1 a2 ... an  ==  (...((f a1) a2)...) an
appMany :: ASA -> [ASA] -> ASA
appMany = foldl App

-- (lambda (x1 x2 ... xn) body)  ==  (lambda x1 (lambda x2 ... (lambda xn body)...))
funMany :: [String] -> ASA -> ASA
funMany ps body = foldr (\p acc -> Fun p acc) body ps

--------------------------------------------------------------------------------
-- Núcleo (ASA) → Forma evaluable (ASAValues)
--------------------------------------------------------------------------------

desugarV :: ASA -> ASAValues
desugarV (Id i)          = IdV i
desugarV (Num n)         = NumV n
desugarV (Boolean b)     = BooleanV b
desugarV Nil             = NilV
desugarV (Pair a b)      = PairV (desugarV a) (desugarV b)
desugarV (Fst e)         = FstV (desugarV e)
desugarV (Snd e)         = SndV (desugarV e)

desugarV (PrimN t xs)    = PrimNV t (map desugarV xs)

desugarV (Not e)         = NotV (desugarV e)
desugarV (If0 c t e)     = If0V (desugarV c) (desugarV t) (desugarV e)
desugarV (If  c t e)     = IfV  (desugarV c) (desugarV t) (desugarV e)
desugarV (Fun p c)       = FunV p (desugarV c)
desugarV (App f a)       = AppV (desugarV f) (desugarV a)
