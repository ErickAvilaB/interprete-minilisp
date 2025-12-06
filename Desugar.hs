module Desugar where

-- Corrección 7 en el documento:
--  * Alineamos este módulo con el núcleo teórico del lenguaje.
--  * El núcleo YA NO tiene operadores variádicos ni if0 como primitivas.
--  * Los variádicos de la superficie (+ - * / = < > <= >= !=) se desazucaran
--    aquí a combinaciones BINARIAS, y if0 se define en términos de if y = 0.

-- Importamos solo el ASA de superficie (SASA) sin los constructores de PrimTag,
-- y usamos Parser cualificado (P.) para los tags de primitivas.
import Parser (SASA (..))
import Parser qualified as P

--------------------------------------------------------------------------------
-- Núcleo (ASA)
--------------------------------------------------------------------------------

-- ASA: core del lenguaje tras desazucarado.
-- En el núcleo solo hay:
--   * operaciones aritméticas y de comparación BINARIAS,
--   * un único condicional If,
--   * funciones unarias y aplicación binaria,
--   * pares y la constante Nil.
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Nil
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA
  | -- Aritmética binaria
    Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | Sqrt ASA
  | Expt ASA ASA
  | -- Comparaciones binarias
    Eql ASA ASA
  | Lt ASA ASA
  | Gt ASA ASA
  | Leq ASA ASA
  | Geq ASA ASA
  | Neq ASA ASA
  | -- Booleanas y control
    Not ASA
  | If ASA ASA ASA
  | -- Funciones y aplicación
    Fun String ASA
  | App ASA ASA
  deriving (Show)

--------------------------------------------------------------------------------
-- ASA evaluable (con entorno) – lo que consume el intérprete
--------------------------------------------------------------------------------

data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | NilV
  | PairV ASAValues ASAValues
  | FstV ASAValues
  | SndV ASAValues
  | -- Aritmética binaria
    AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | MulV ASAValues ASAValues
  | DivV ASAValues ASAValues
  | SqrtV ASAValues
  | ExptV ASAValues ASAValues
  | -- Comparaciones binarias
    EqlV ASAValues ASAValues
  | LtV ASAValues ASAValues
  | GtV ASAValues ASAValues
  | LeqV ASAValues ASAValues
  | GeqV ASAValues ASAValues
  | NeqV ASAValues ASAValues
  | -- Booleanas y control
    NotV ASAValues
  | IfV ASAValues ASAValues ASAValues
  | -- Funciones y aplicación
    FunV String ASAValues
  | ExprV ASAValues [(String, ASAValues)]
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
  deriving (Show)

--------------------------------------------------------------------------------
-- Desazúcar: SASA (superficie) → ASA (núcleo)
--------------------------------------------------------------------------------

desugar :: SASA -> ASA
-- Literales y variables
desugar (IdS i) = Id i
desugar (NumS n) = Num n
desugar (BooleanS b) = Boolean b
desugar NilS = Nil
-- Pares y listas
desugar (PairS a b) = Pair (desugar a) (desugar b)
desugar (FstS e) = Fst (desugar e)
desugar (SndS e) = Snd (desugar e)
-- Listas por corchetes: [e1, ..., en] ⇒ (e1, ...(en, nil)...)
desugar (ListS xs) = foldr (\e acc -> Pair (desugar e) acc) Nil xs
-- unarias especiales y not
desugar (NotS e) = Not (desugar e)
-- add1/sub1 como azúcar de + / -
desugar (Add1S e) = Add (desugar e) (Num 1)
desugar (Sub1S e) = Sub (desugar e) (Num 1)
desugar (SqrtS e) = Sqrt (desugar e)
desugar (ExptS a b) = Expt (desugar a) (desugar b)
-- Corrección 7 en el documento:
--   Los operadores variádicos de superficie se traducen aquí a combinaciones
--   binarias del núcleo; el intérprete SOLO ve primitivas binarias.
desugar (PrimNS tag xs) = desugarPrimNS tag (map desugar xs)
-- if / if0
desugar (IfS c t e) = If (desugar c) (desugar t) (desugar e)
-- Corrección 7 en el documento:
--   if0 es azúcar: (if0 e t f)  ==>  (if (= e 0) t f)
desugar (If0S c t e) =
  let c' = desugar c
   in If (Eql c' (Num 0)) (desugar t) (desugar e)
-- cond: lista de cláusulas + else
--   (cond [(c1 t1)] ... [(cn tn)] [else e0])
--   ==> if c1 t1 (if c2 t2 (... (if cn tn e0)...))
desugar (CondS clauses elseBody) =
  foldr
    (\(c, t) acc -> If (desugar c) (desugar t) acc)
    (desugar elseBody)
    clauses
-- let paralelo: ((lambda (x y ...) body) e1 e2 ...)
desugar (LetManyS binds body) =
  let (ps, es) = unzip binds
   in appMany (funMany ps (desugar body)) (map desugar es)
-- let* secuencial: anidar lets de a uno
desugar (LetStarManyS [] body) = desugar body
desugar (LetStarManyS ((x, e) : xs) body) =
  App (Fun x (desugar (LetStarManyS xs body))) (desugar e)
-- letrec vía combinador Z (variable libre "Z" provista en el prelude)
desugar (LetRecS p v c) =
  desugar (LetManyS [(p, AppManyS (IdS "Z") [FunManyS [p] v])] c)
-- lambdas variádicas → currificadas
desugar (FunManyS ps body) = funMany ps (desugar body)
-- aplicación múltiple → aplicaciones anidadas
desugar (AppManyS f args) = appMany (desugar f) (map desugar args)

--------------------------------------------------------------------------------
-- Helpers (azúcar del núcleo)
--------------------------------------------------------------------------------

-- f a1 a2 ... an  ==  (...((f a1) a2)...) an
appMany :: ASA -> [ASA] -> ASA
appMany = foldl App

-- (lambda (x1 x2 ... xn) body)
--   == (lambda x1 (lambda x2 ... (lambda xn body)...))
funMany :: [String] -> ASA -> ASA
funMany ps body = foldr Fun body ps

--------------------------------------------------------------------------------
-- Helpers para operadores variádicos de superficie
--------------------------------------------------------------------------------

-- Plegado izquierdo aritmético:
--   (+ a b c)  ==>  Add (Add a b) c
foldArith :: (ASA -> ASA -> ASA) -> [ASA] -> ASA
foldArith _ [] = error "operador aritmético variádico sin argumentos"
foldArith _ [_] = error "operador aritmético variádico requiere ≥ 2 argumentos"
foldArith op (e1 : e2 : es) = foldl op (op e1 e2) es

-- Comparadores en cadena con cortocircuito:
--   (< a b c)  ==>  if (a<b) (if (b<c) #t #f) #f
cmpChain :: (ASA -> ASA -> ASA) -> [ASA] -> ASA
cmpChain _ [] = error "comparador variádico sin argumentos"
cmpChain _ [_] = error "comparador variádico requiere ≥ 2 argumentos"
cmpChain op (e1 : e2 : es) = go e1 e2 es
  where
    go a b [] = op a b
    go a b (c : cs) = If (op a b) (go b c cs) (Boolean False)

-- Traducción de PrimNS de superficie al núcleo binario
desugarPrimNS :: P.PrimTag -> [ASA] -> ASA
desugarPrimNS tag es =
  case tag of
    -- Aritmética variádica (plegado izquierdo)
    P.Plus -> foldArith Add es
    P.Minus -> foldArith Sub es
    P.Times -> foldArith Mul es
    P.Div -> foldArith Div es
    -- Comparaciones variádicas encadenadas
    P.Eql -> cmpChain Eql es
    P.Neq -> cmpChain Neq es
    P.Lt -> cmpChain Lt es
    P.Gt -> cmpChain Gt es
    P.Leq -> cmpChain Leq es
    P.Geq -> cmpChain Geq es
    -- sqrt y expt:
    P.Sqrt ->
      case es of
        [e] -> Sqrt e
        _ -> error "sqrt espera 1 argumento (núcleo)"
    P.Expt ->
      case es of
        [b, e] -> Expt b e
        _ -> error "expt espera 2 argumentos (núcleo)"

--------------------------------------------------------------------------------
-- Núcleo (ASA) → Forma evaluable (ASAValues)
--------------------------------------------------------------------------------

desugarV :: ASA -> ASAValues
desugarV (Id i) = IdV i
desugarV (Num n) = NumV n
desugarV (Boolean b) = BooleanV b
desugarV Nil = NilV
desugarV (Pair a b) = PairV (desugarV a) (desugarV b)
desugarV (Fst e) = FstV (desugarV e)
desugarV (Snd e) = SndV (desugarV e)
-- Aritmética / comparaciones / unarias
desugarV (Add a b) = AddV (desugarV a) (desugarV b)
desugarV (Sub a b) = SubV (desugarV a) (desugarV b)
desugarV (Mul a b) = MulV (desugarV a) (desugarV b)
desugarV (Div a b) = DivV (desugarV a) (desugarV b)
desugarV (Sqrt e) = SqrtV (desugarV e)
desugarV (Expt b e) = ExptV (desugarV b) (desugarV e)
desugarV (Eql a b) = EqlV (desugarV a) (desugarV b)
desugarV (Lt a b) = LtV (desugarV a) (desugarV b)
desugarV (Gt a b) = GtV (desugarV a) (desugarV b)
desugarV (Leq a b) = LeqV (desugarV a) (desugarV b)
desugarV (Geq a b) = GeqV (desugarV a) (desugarV b)
desugarV (Neq a b) = NeqV (desugarV a) (desugarV b)
desugarV (Not e) = NotV (desugarV e)
-- Control y funciones
desugarV (If c t e) = IfV (desugarV c) (desugarV t) (desugarV e)
desugarV (Fun p c) = FunV p (desugarV c)
desugarV (App f a) = AppV (desugarV f) (desugarV a)
