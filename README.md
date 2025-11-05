# Mini-Lisp

Autores

- Avila Barba Erick Yahir
- Galvan Cordoba Uriel Federico
- Hernandez Coronel Angel Carlos

## Instalar dependencias (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install -y ghc alex happy
```

## Generar lexer y parser

Desde la raíz del proyecto:

```bash
alex Lex.x        # -> genera Lex.hs
happy Parser.y    # -> genera Parser.hs
```

## Compilar

```bash
ghc -O2 MiniLisp.hs Interp.hs Desugar.hs Parser.hs Lex.hs \
  -main-is REPL.run -o mini-lisp
```

## Ejecutar

```bash
./mini-lisp
```

## Casos de Prueba

A

1. Suma de los primeros n números naturales.

`sum1_n(12) = 78`

```bash
((letrec (sum1_n (lambda (n) (if0 n 0 (+ n (sum1_n (sub1 n)))))) sum1_n) 12)
```

2. Factorial

`fact(6) = 720`

```bash
((letrec (fact (lambda (n) (if0 n 1 (* n (fact (sub1 n)))))) fact) 6)
```

3. Fibonacci

`fib(10) = 55`

```bash
((letrec (fib (lambda (n) (if (<= n 1) n (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))) fib) 10)
```

4. Función map para listas.

En este caso calculamos los cuadrados para una lista de numeros. `lambda (x) (* x x)` aplicada a la lista `[1, 2, 3, 4]`, obtenemos como resultado `[1, 4, 9, 16]`.

```bash
((letrec (map_n (lambda (f xs n) (if0 n [] ((f (head xs)), (map_n f (tail xs) (sub1 n)))))) map_n) (lambda (x) (* x x)) [1, 2, 3, 4] 4)
```

5. Función filter para listas.

Filtramos los numero mayores a 3 con `lambda (x) (> x 3)` con la lista `[1, 2, 3, 4, 5, 6]` tenemos `[4, 5, 6]`

```bash
((letrec (filter_n (lambda (p xs n) (if0 n [] (if (p (head xs)) ((head xs), (filter_n p (tail xs) (sub1 n))) (filter_n p (tail xs) (sub1 n)))))) filter_n) (lambda (x) (> x 3)) [1, 2, 3, 4, 5, 6] 6)
```

## (Opcional) Limpiar artefactos

```bash
git clean -fx
```
