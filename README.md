
# Proyecto de Compiladores

## Resumen
Este proyecto compila y ejecuta un analizador lexico sencillo para IMP. El main está en `proyecto/app/Main.hs`. El programa:
- Lee `./specs/IMP.md`.
- Elimina comentarios del tipo `*- -*` y `** **`.
- Construye tokens según los nombres presentes en la especificación.
- Muestra un menú para procesar ejemplos `./samples/*.md`.
- Realiza un lexer y muestra tokens (con y sin espacios).

## Reporte.pdf

El reporte con la documentación del proceso para construir nuestro lexer se encuentra en la raíz de este proyecto -> Reporte.pdf

Sin embargo, el latex está en reporte/latex

## Requisitos
- Linux.
- GHC (Glasgow Haskell Compiler).
- Cabal (>= 3.6 importante).

## Compilar

- `cabal build`

## Ejecutar

- Directamente con Cabal:
  - `cabal run`

Al ejecutar, y para visualizar mejor los resultados, verás el menú interactivo:
- Selecciona un ejemplo por número.
- Presiona ENTER para volver al menú.
- `0` para salir.

## Autores
- Maryam Michelle Del Monte Ortega 320083527
- Sahara Mariel Monroy Romero 320206391