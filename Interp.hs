module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Importante: En esta versión del intérprete usamos evaluación ansiosa junto con el 
-- combinador Z y la semántica de paso pequeño. En este contexto, durante la evaluación 
-- de una función recursiva, el cuerpo puede contener variables (como ‘f’ en Z) que 
-- deben mantenerse accesibles dentro de su propio entorno, sin propagarse al contexto 
-- externo. 
--
-- Para preservar correctamente esas ligaduras, realizamos dos ajustes fundamentales:
--
-- • Al aplicar una cerradura, el cuerpo se devuelve envuelto en una expresión con 
--   entorno: ExprV b env''. De esta forma, la evaluación del cuerpo ocurre dentro 
--   del ambiente extendido que contiene la variable recursiva, evitando la pérdida 
--   de ligaduras y asegurando que las referencias a ‘f’ o ‘r’ sean válidas.
--
-- • Dado que ahora el cuerpo se representa como una ExprV, el sistema debe saber 
--   cómo avanzar dentro de ella. Para ello se añade una regla que permite reducir 
--   un paso en el entorno interno de la expresión, manteniendo intacto el ambiente 
--   del contexto. Esto garantiza que la evaluación siga siendo ansiosa, pero que 
--   cada función recursiva conserve su propio entorno léxico independiente.


smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env                                   = (lookupEnv i env, env)
smallStep (NumV n) env                                  = (NumV n, env)
smallStep (BooleanV b) env                              = (BooleanV b, env)

-- Suma
smallStep (AddV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (AddV (NumV n) d) env                         = (AddV (NumV n) (fst (smallStep d env)), env)
smallStep (AddV i d) env                                = (AddV (fst (smallStep i env)) d, env)

-- Resta
smallStep (SubV (NumV n) (NumV m)) env                  = (NumV (n - m), env)
smallStep (SubV (NumV n) d) env                         = (SubV (NumV n) (fst (smallStep d env)), env)
smallStep (SubV i d) env                                = (SubV (fst (smallStep i env)) d, env)

-- Multiplicación
smallStep (MulV (NumV n) (NumV m)) env                  = (NumV (n * m), env)
smallStep (MulV (NumV n) d) env                         = (MulV (NumV n) (fst (smallStep d env)), env)
smallStep (MulV i d) env                                = (MulV (fst (smallStep i env)) d, env)

-- Comparación menor o igual
smallStep (LeqV (NumV n) (NumV m)) env                  = (BooleanV (n <= m), env)
smallStep (LeqV (NumV n) d) env                         = (LeqV (NumV n) (fst (smallStep d env)), env)
smallStep (LeqV i d) env                                = (LeqV (fst (smallStep i env)) d, env)

-- Not
smallStep (NotV (BooleanV b))   env                     = (BooleanV (not b), env)
smallStep (NotV b) env                                  = (NotV (fst (smallStep b env)), env)

-- If
-- If (booleana)
smallStep (IfV (BooleanV True)  t e) env = (t, env)
smallStep (IfV (BooleanV False) t e) env = (e, env)
smallStep (IfV c t e) env                = (IfV (fst (smallStep c env)) t e, env)

-- If0
smallStep (If0V (NumV 0) t e) env                       = (t,env)
smallStep (If0V (NumV n) t e) env                       = (e,env)
smallStep (If0V c t e) env                              = (If0V (fst (smallStep c env)) t e, env)

-- Funciones y aplicación
smallStep (FunV p c) env                                = (ClosureV p c env, env)
smallStep (AppV cv@(ClosureV p b env') a) env
  | isValueV a = (ExprV b ((p, a) : env'), env)
  | otherwise = (AppV cv (fst (smallStep a env)), env)
smallStep (AppV f a) env                                = (AppV (fst (smallStep f env)) a, env)

smallStep (ExprV v env') env
  | isValueV v = (v, env)
  | otherwise = (ExprV e1 env', env)
  where (e1, _) = smallStep v env'


-- Interpretación total
interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise  =
      let (e', env') = smallStep e env
       in interp e' env'

-- Predicados y auxiliares
isValueV :: ASAValues -> Bool
isValueV (NumV _)           = True
isValueV (BooleanV _)       = True
isValueV (ClosureV _ _ _)   = True
isValueV _                  = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j    = v
  | otherwise = lookupEnv i env

numN :: ASAValues -> Int
numN (NumV n) = n

boolN :: ASAValues -> Bool
boolN (BooleanV b) = b
boolN _ = False

closureP :: ASAValues -> String
closureP (ClosureV p _ _) = p

closureC :: ASAValues -> ASAValues
closureC (ClosureV _ c _) = c

closureE :: ASAValues -> Env
closureE (ClosureV _ _ e) = e
