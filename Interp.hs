module Interp where

import qualified Parser as P
import Desugar

-- Entorno léxico
type Env = [(String, ASAValues)]

--------------------------------------------------------------------------------
-- Paso-pequeño (call-by-value / ansioso)
--------------------------------------------------------------------------------

smallStep :: ASAValues -> Env -> (ASAValues, Env)

-- Identificadores y literales
smallStep (IdV i) env                   = (lookupEnv i env, env)
smallStep (NumV n) env                  = (NumV n, env)
smallStep (BooleanV b) env              = (BooleanV b, env)
smallStep NilV env                      = (NilV, env)

-- Pares
smallStep (PairV v w) env
  | isValueV v && isValueV w            = (PairV v w, env)
smallStep (PairV v w) env
  | isValueV v                          = let (w',_) = smallStep w env in (PairV v w', env)
  | otherwise                           = let (v',_) = smallStep v env in (PairV v' w, env)

-- Proyecciones
smallStep (FstV p) env
  | isValueV p =
      case p of
        PairV v _ -> (v, env)
        _         -> runtimeError "fst espera un par"
  | otherwise   = let (p',_) = smallStep p env in (FstV p', env)

smallStep (SndV p) env
  | isValueV p =
      case p of
        PairV _ w -> (w, env)
        _         -> runtimeError "snd espera un par"
  | otherwise   = let (p',_) = smallStep p env in (SndV p', env)

-- Booleanas
smallStep (NotV v) env
  | isValueV v =
      case v of
        BooleanV b -> (BooleanV (not b), env)
        _          -> runtimeError "not espera booleano"
  | otherwise   = let (v',_) = smallStep v env in (NotV v', env)

-- Condicionales
smallStep (IfV c t e) env
  | isValueV c =
      case c of
        BooleanV True  -> (t, env)
        BooleanV False -> (e, env)
        _              -> runtimeError "if espera condición booleana"
  | otherwise   = let (c',_) = smallStep c env in (IfV c' t e, env)

smallStep (If0V c t e) env
  | isValueV c =
      case c of
        NumV 0 -> (t, env)
        NumV _ -> (e, env)
        _      -> runtimeError "if0 espera entero"
  | otherwise   = let (c',_) = smallStep c env in (If0V c' t e, env)

-- Primitivas variádicas
smallStep (PrimNV tag vs) env
  | all isValueV vs                     = (evalPrimN tag vs, env)
  | otherwise                           = let (vs',_) = stepFirst vs env in (PrimNV tag vs', env)

-- Lambdas / clausuras / aplicación (como en tu base)
smallStep (FunV p c) env                = (ClosureV p c env, env)

smallStep (AppV cv@(ClosureV p b env') a) env
  | isValueV a                          = (ExprV b ((p, a) : env'), env)
  | otherwise                           = let (a',_) = smallStep a env in (AppV cv a', env)

smallStep (AppV f a) env                = let (f',_) = smallStep f env in (AppV f' a, env)

-- ExprV: avanza dentro del entorno interno
smallStep (ExprV v env') env
  | isValueV v                          = (v, env)
  | otherwise                           = let (e1, _) = smallStep v env' in (ExprV e1 env', env)

--------------------------------------------------------------------------------
-- Interpretación total
--------------------------------------------------------------------------------

interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise  =
      let (e', env') = smallStep e env
       in interp e' env'

--------------------------------------------------------------------------------
-- Predicados de valor y auxiliares
--------------------------------------------------------------------------------

isValueV :: ASAValues -> Bool
isValueV (NumV _)           = True
isValueV (BooleanV _)       = True
isValueV NilV               = True
isValueV (PairV v w)        = isValueV v && isValueV w
isValueV (ClosureV _ _ _)   = True
isValueV _                  = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = runtimeError ("Variable " ++ i ++ " no encontrada")
lookupEnv i ((j, v) : env)
  | i == j    = v
  | otherwise = lookupEnv i env

-- Avanza la primera subexpresión no valor de una lista de argumentos
stepFirst :: [ASAValues] -> Env -> ([ASAValues], Env)
stepFirst [] env = ([], env)
stepFirst (x:xs) env
  | isValueV x = let (xs', env') = stepFirst xs env in (x:xs', env')
  | otherwise  = let (x', env')  = smallStep x env in (x':xs, env')

runtimeError :: String -> a
runtimeError msg = error ("Runtime error: " ++ msg)

--------------------------------------------------------------------------------
-- Primitivas variádicas y helpers numéricos
--------------------------------------------------------------------------------

-- Convierten y validan enteros
nums :: [ASAValues] -> [Int]
nums = map num
  where
    num (NumV n) = n
    num _        = runtimeError "se esperaba entero"

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = runtimeError "división por cero"
safeDiv a b = a `div` b

-- Cadena de comparaciones sobre enteros
boolChain :: (Int -> Int -> Bool) -> [ASAValues] -> ASAValues
boolChain _ []       = BooleanV True
boolChain _ [_]      = BooleanV True
boolChain op vs      =
  let ns = nums vs
  in BooleanV (and (zipWith op ns (tail ns)))

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

allDistinct :: Ord a => [a] -> Bool
allDistinct []     = True
allDistinct (x:xs) = notElem x xs && allDistinct xs

-- Evaluación de primitivas
evalPrimN :: P.PrimTag -> [ASAValues] -> ASAValues

-- Aritmética (+ - * /) sobre enteros (≥ 2 args)
evalPrimN P.Plus  vs = NumV (foldl1 (+) (nums vs))
evalPrimN P.Minus vs = NumV (foldl1 (-) (nums vs))
evalPrimN P.Times vs = NumV (foldl1 (*) (nums vs))
evalPrimN P.Div   vs = NumV (foldl1 safeDiv (nums vs))

-- sqrt (exactamente 1 arg)
evalPrimN P.Sqrt [v]   = NumV (floor (sqrt (fromIntegral (head (nums [v])) :: Double)))
evalPrimN P.Sqrt _     = runtimeError "sqrt espera 1 argumento"

-- expt (exactamente 2 args)
evalPrimN P.Expt [a,b] =
  case nums [a,b] of
    [x,y] -> NumV (x ^ y)
    _     -> runtimeError "expt espera 2 argumentos"
evalPrimN P.Expt _     = runtimeError "expt espera 2 argumentos"

-- Comparaciones variádicas (≥ 2), cadena
evalPrimN P.Lt  vs = boolChain (<)  vs
evalPrimN P.Gt  vs = boolChain (>)  vs
evalPrimN P.Leq vs = boolChain (<=) vs
evalPrimN P.Geq vs = boolChain (>=) vs

-- Igualdad / desigualdad variádicas (≥ 2), enteros
evalPrimN P.Eql vs = BooleanV (allEqual (nums vs))
evalPrimN P.Neq vs = BooleanV (allDistinct (nums vs))
