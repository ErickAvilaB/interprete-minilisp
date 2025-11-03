# Mini-Lisp

Autores

- Avila Barba Erick Yahir
- Galvan Cordoba Uriel Federico
- Hernandez Coronel Angel Carlos

## 1) Instalar dependencias (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install -y ghc alex happy
```

## 2) Generar lexer y parser

Desde la raíz del proyecto:

```bash
alex Lex.x        # -> genera Lex.hs
happy Parser.y    # -> genera Parser.hs
```

## 3) Compilar

```bash
ghc -O2 Main.hs MiniLisp.hs Interp.hs Desugar.hs Parser.hs Lex.hs -o mini-lisp
```

## 4) Ejecutar

```bash
./mini-lisp
```

## 5) (Opcional) Limpiar artefactos

```bash
git clean -fx
```
