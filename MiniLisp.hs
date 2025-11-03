module REPL where

import Lex
import Parser
import Desugar
import Interp

-- Combinador Z (orden aplicativa / ansiosa)
combinadorZ :: String
combinadorZ =
  "(lambda (f)                                   \
  \  ((lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))             \
  \   (lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))))"

-- Valor de Z ya evaluado en entorno vacío
z :: ASAValues
z =
  let sasa = parse (lexer combinadorZ)
      asa  = desugar sasa
  in interp (desugarV asa) []

--------------------------------------------------------------------------------
-- Pretty-printer (muestra listas como [a, b, c] y pares como (a . d))
--------------------------------------------------------------------------------

saca :: ASAValues -> String
saca (NumV n)           = show n
saca (BooleanV True)    = "#t"
saca (BooleanV False)   = "#f"
saca NilV               = "[]"
saca (PairV a d)        = case toList (PairV a d) of
                            Just xs -> "[" ++ joinWith ", " (map saca xs) ++ "]"
                            Nothing -> "(" ++ saca a ++ " . " ++ saca d ++ ")"
saca (ClosureV _ _ _)   = "#<procedure>"
-- ExprV, FunV, PrimNV, etc. no deberían imprimirse como resultados finales;
-- si aparecen, mostramos un marcador genérico.
saca _                  = "#<valor>"

-- Detecta si un valor es una lista encadenada de PairV ... NilV y la convierte
toList :: ASAValues -> Maybe [ASAValues]
toList NilV                 = Just []
toList (PairV x xs)         = do
  rest <- toList xs
  pure (x : rest)
toList _                    = Nothing

joinWith :: String -> [String] -> String
joinWith _   []     = ""
joinWith _   [x]    = x
joinWith sep (x:xs) = x ++ sep ++ " " ++ joinWith sep xs

--------------------------------------------------------------------------------
-- REPL y helpers
--------------------------------------------------------------------------------

-- Entorno inicial: ligamos Z; alias Y -> Z opcional
prelude :: Env
prelude = [("Z", z), ("Y", z)]

-- Evalúa una cadena fuente en el prelude y devuelve el valor
evalString :: String -> ASAValues
evalString src =
  let toks = lexer src
      sasa = parse toks
      asa  = desugar sasa
  in  interp (desugarV asa) prelude

-- REPL simple (una forma por línea)
repl :: IO ()
repl = do
  putStr "Mini-Lisp> "
  line <- getLine
  if line == "(exit)" || line == ":q"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (evalString line)
      repl

-- Ejecuta REPL
run :: IO ()
run = do
  putStrLn "Mini-Lisp (ansioso) — extendido. Escribe (exit) para salir."
  repl

-- Helper para probar rápidamente expresiones
test :: String -> IO ()
test x = putStrLn $ saca (evalString x)

--------------------------------------------------------------------------------
-- Pruebas sugeridas (puedes invocarlas en GHCi)
--------------------------------------------------------------------------------

-- Aritmética variádica
testPlus    = test "(+ 1 2 3 4)"           -- 10
testMinus   = test "(- 10 3 2)"            -- 5
testTimes   = test "(* 3 4 5)"             -- 60
testDiv     = test "(/ 40 2 4)"            -- 5

-- Unarias
testAdd1    = test "(add1 5)"              -- 6
testSub1    = test "(sub1 0)"              -- -1
testSqrt    = test "(sqrt 10)"             -- 3 (floor)
testExpt    = test "(expt 2 10)"           -- 1024

-- Comparaciones variádicas (sobre enteros)
testLt      = test "(< 2 5 9)"             -- #t
testGeq     = test "(>= 9 9 1)"            -- #t
testEq      = test "(= 4 4 4)"             -- #t
testNeq     = test "(!= 1 2 3)"            -- #t

-- Pares y listas
testPair    = test "(fst (1, #t))"         -- 1
testList    = test "[1, 2, 3]"             -- [1, 2, 3]
testHead    = test "(fst [1, 2, 3])"       -- 1
testTail    = test "(snd [1, 2, 3])"       -- [2, 3] (impreso como lista)

-- let / let* variádicos
-- En let (paralelo), y no puede depender del nuevo x ⇒ debería fallar si lo usa
testLetPar  = test "(let ((x 3) (y (+ x 1))) (+ x y))"
-- En let* (secuencial), sí puede depender
testLetSeq  = test "(let* ((x 3) (y (+ x 1))) (+ x y))" -- 7

-- Lambda variádica y aplicación múltiple
testLambda  = test "((lambda (x y z) (+ x y z)) 1 2 3)" -- 6

-- if e if0
testIf      = test "(if #t 1 2)"           -- 1
testIf0     = test "(if0 0 1 2)"           -- 1

-- letrec vía Z
testFact    = test "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))" -- 120
