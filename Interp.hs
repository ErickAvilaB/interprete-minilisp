module Interp where

-- Corrección 6 en el documento:
--  * El intérprete ahora solo maneja el núcleo BINARIO (sin variádicos ni if0).
--  * La evaluación sigue la SOS de paso-pequeño, call-by-value, con evaluación
--    estricta de izquierda a derecha, como en el documento teórico.

import Desugar

-- Entorno léxico
type Env = [(String, ASAValues)]

--------------------------------------------------------------------------------
-- Paso-pequeño (call-by-value / ansioso)
--------------------------------------------------------------------------------

smallStep :: ASAValues -> Env -> (ASAValues, Env)
-- Identificadores y literales
smallStep (IdV i) env = (lookupEnv i env, env)
smallStep (NumV n) env = (NumV n, env)
smallStep (BooleanV b) env = (BooleanV b, env)
smallStep NilV env = (NilV, env)
-- Pares
smallStep (PairV v w) env
  | isValueV v && isValueV w = (PairV v w, env)
smallStep (PairV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (PairV v w', env)
smallStep (PairV v w) env =
  let (v', _) = smallStep v env
   in (PairV v' w, env)
-- Proyecciones
smallStep (FstV p) env
  | isValueV p =
      case p of
        PairV v _ -> (v, env)
        _ -> runtimeError "fst espera un par"
smallStep (FstV p) env =
  let (p', _) = smallStep p env
   in (FstV p', env)
smallStep (SndV p) env
  | isValueV p =
      case p of
        PairV _ w -> (w, env)
        _ -> runtimeError "snd espera un par"
smallStep (SndV p) env =
  let (p', _) = smallStep p env
   in (SndV p', env)
-- Booleanas
smallStep (NotV v) env
  | isValueV v =
      case v of
        BooleanV b -> (BooleanV (not b), env)
        _ -> runtimeError "not espera booleano"
smallStep (NotV v) env =
  let (v', _) = smallStep v env
   in (NotV v', env)
--------------------------------------------------------------------------------
-- Primitivas aritméticas y de comparación binarias
--------------------------------------------------------------------------------

-- (+ v w)
smallStep (AddV v w) env
  | isValueV v && isValueV w = (applyIntBin (+) v w, env)
smallStep (AddV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (AddV v w', env)
smallStep (AddV v w) env =
  let (v', _) = smallStep v env
   in (AddV v' w, env)
-- (- v w)
smallStep (SubV v w) env
  | isValueV v && isValueV w = (applyIntBin (-) v w, env)
smallStep (SubV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (SubV v w', env)
smallStep (SubV v w) env =
  let (v', _) = smallStep v env
   in (SubV v' w, env)
-- (* v w)
smallStep (MulV v w) env
  | isValueV v && isValueV w = (applyIntBin (*) v w, env)
smallStep (MulV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (MulV v w', env)
smallStep (MulV v w) env =
  let (v', _) = smallStep v env
   in (MulV v' w, env)
-- (/ v w)
smallStep (DivV v w) env
  | isValueV v && isValueV w = (applyIntBin safeDiv v w, env)
smallStep (DivV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (DivV v w', env)
smallStep (DivV v w) env =
  let (v', _) = smallStep v env
   in (DivV v' w, env)
-- sqrt v
smallStep (SqrtV e) env
  | isValueV e =
      (applyUnaryInt (\n -> NumV (floor (sqrt (fromIntegral n :: Double)))) e, env)
smallStep (SqrtV e) env =
  let (e', _) = smallStep e env
   in (SqrtV e', env)
-- expt b e
smallStep (ExptV b e) env
  | isValueV b && isValueV e = (applyIntBin (^) b e, env)
smallStep (ExptV b e) env
  | isValueV b =
      let (e', _) = smallStep e env
       in (ExptV b e', env)
smallStep (ExptV b e) env =
  let (b', _) = smallStep b env
   in (ExptV b' e, env)
-- Comparadores binarios (sobre enteros)
smallStep (EqlV v w) env
  | isValueV v && isValueV w = (applyCmpBin (==) v w, env)
smallStep (EqlV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (EqlV v w', env)
smallStep (EqlV v w) env =
  let (v', _) = smallStep v env
   in (EqlV v' w, env)
smallStep (NeqV v w) env
  | isValueV v && isValueV w = (applyCmpBin (/=) v w, env)
smallStep (NeqV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (NeqV v w', env)
smallStep (NeqV v w) env =
  let (v', _) = smallStep v env
   in (NeqV v' w, env)
smallStep (LtV v w) env
  | isValueV v && isValueV w = (applyCmpBin (<) v w, env)
smallStep (LtV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (LtV v w', env)
smallStep (LtV v w) env =
  let (v', _) = smallStep v env
   in (LtV v' w, env)
smallStep (GtV v w) env
  | isValueV v && isValueV w = (applyCmpBin (>) v w, env)
smallStep (GtV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (GtV v w', env)
smallStep (GtV v w) env =
  let (v', _) = smallStep v env
   in (GtV v' w, env)
smallStep (LeqV v w) env
  | isValueV v && isValueV w = (applyCmpBin (<=) v w, env)
smallStep (LeqV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (LeqV v w', env)
smallStep (LeqV v w) env =
  let (v', _) = smallStep v env
   in (LeqV v' w, env)
smallStep (GeqV v w) env
  | isValueV v && isValueV w = (applyCmpBin (>=) v w, env)
smallStep (GeqV v w) env
  | isValueV v =
      let (w', _) = smallStep w env
       in (GeqV v w', env)
smallStep (GeqV v w) env =
  let (v', _) = smallStep v env
   in (GeqV v' w, env)
--------------------------------------------------------------------------------
-- Condicional
--------------------------------------------------------------------------------

smallStep (IfV c t e) env
  | isValueV c =
      case c of
        BooleanV True -> (t, env)
        BooleanV False -> (e, env)
        _ -> runtimeError "if espera condición booleana"
smallStep (IfV c t e) env =
  let (c', _) = smallStep c env
   in (IfV c' t e, env)
--------------------------------------------------------------------------------
-- Lambdas / clausuras / aplicación
--------------------------------------------------------------------------------

smallStep (FunV p c) env = (ClosureV p c env, env)
smallStep (AppV cv@(ClosureV p b env') a) env
  | isValueV a = (ExprV b ((p, a) : env'), env)
smallStep (AppV cv@(ClosureV _ _ _) a) env =
  let (a', _) = smallStep a env
   in (AppV cv a', env)
smallStep (AppV f a) env =
  let (f', _) = smallStep f env
   in (AppV f' a, env)
-- ExprV: avanza dentro del ambiente interno de la clausura
smallStep (ExprV v env') env
  | isValueV v = (v, env)
smallStep (ExprV v env') env =
  let (e1, _) = smallStep v env'
   in (ExprV e1 env', env)

--------------------------------------------------------------------------------
-- Interpretación total
--------------------------------------------------------------------------------

interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise =
      let (e', env') = smallStep e env
       in interp e' env'

--------------------------------------------------------------------------------
-- Predicados de valor y auxiliares
--------------------------------------------------------------------------------

isValueV :: ASAValues -> Bool
isValueV (NumV _) = True
isValueV (BooleanV _) = True
isValueV NilV = True
isValueV (PairV v w) = isValueV v && isValueV w
isValueV (ClosureV _ _ _) = True
isValueV _ = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = runtimeError ("Variable " ++ i ++ " no encontrada")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

runtimeError :: String -> a
runtimeError msg = error ("Runtime error: " ++ msg)

--------------------------------------------------------------------------------
-- Helpers numéricos para el núcleo binario
--------------------------------------------------------------------------------

-- Aritmética binaria sobre enteros
applyIntBin :: (Int -> Int -> Int) -> ASAValues -> ASAValues -> ASAValues
applyIntBin op (NumV n1) (NumV n2) = NumV (op n1 n2)
applyIntBin _ _ _ = runtimeError "se esperaban enteros"

-- Comparadores binarios sobre enteros
applyCmpBin :: (Int -> Int -> Bool) -> ASAValues -> ASAValues -> ASAValues
applyCmpBin op (NumV n1) (NumV n2) = BooleanV (op n1 n2)
applyCmpBin _ _ _ = runtimeError "comparador espera enteros"

-- Unarios sobre enteros
applyUnaryInt :: (Int -> ASAValues) -> ASAValues -> ASAValues
applyUnaryInt f (NumV n) = f n
applyUnaryInt _ _ = runtimeError "se esperaba entero"

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = runtimeError "división por cero"
safeDiv a b = a `div` b
