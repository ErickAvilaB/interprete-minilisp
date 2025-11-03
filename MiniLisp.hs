module REPL where

import Lex
import Desugar
import Parser
import Interp

-- Combinador Z (orden aplicativa / ansiosa)
combinadorZ :: String
combinadorZ =
  "(lambda (f)                                   \
  \  ((lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))             \
  \   (lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))))"

-- Valor de Z ya desazucarado y evaluado en []
z :: ASAValues
z =
  let sasa = parse (lexer combinadorZ)  -- SASA
      asa  = desugar sasa               -- ASA
  in interp (desugarV asa) []           -- ASAValues evaluado en []

-- Pretty-printer sencillo (sin ExprV en la versión ansiosa)
saca :: ASAValues -> String
saca (NumV n)           = show n
saca (BooleanV True)    = "#t"
saca (BooleanV False)   = "#f"
saca (ClosureV _ _ _)   = "#<procedure>"
saca _                  = "#<valor-desconocido>"

-- Ambiente inicial: ligamos Z; opcionalmente también Y → Z para compatibilidad
prelude :: Env
prelude = [("Z", z)]

repl :: IO ()
repl = do
  putStr "> "
  str <- getLine
  if str == "(exit)"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (interp (desugarV (desugar (parse (lexer str)))) prelude)
      repl

run :: IO ()
run = do
  putStrLn "Mini-Lisp (versión ansiosa). Bienvenidx."
  repl

test :: String -> IO ()
test x = putStrLn $ saca (interp (desugarV (desugar (parse (lexer x)))) prelude)

-- Pruebas (asumiendo que letrec se desazucara vía Z)
testSuma       = test "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))"      -- 6
testFactorial  = test "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))" -- 120
testFibo       = test "(letrec (fib (lambda (n) (if (<= n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 5))" -- 5
