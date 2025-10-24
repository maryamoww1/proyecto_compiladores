{-# LANGUAGE NamedFieldPuns #-}
-- Módulo del lexer: arma un analizador léxico a partir de una MDD o desde AFDs
module Lexer
  ( 
    lexerM
  -- lexer principal que camina la MDD y devuelve (token, lexema)

  , buildLexerFromAFDs     -- construye el lexer desde AFDs ya dados
  , buildLexerFromRegex    -- construye el lexer desde expresiones regulares
  ) where

import qualified Data.Map.Strict as M
import           Data.Char (isSpace)

import           RegularExpression (Expresion)
import           RegexToAFNe       (regexToAfne)
import           AFNeToAFN         (afnEpToAfn)
import           AFNToAFD          (afnAafd)
import           AFDMin            (minimizar)
import qualified AFD               as D
import           MDD               (MDD(..), buildMDD)

-- La función se encarga de recorrer la MDD carácter por carácter y sacar tokens 
-- Ignora espacios, y ante un símbolo inválido truena con un error léxico.
lexerM :: MDD -> String -> [(String, String)]
lexerM mdd = go
  where
    -- tabla de transiciones (estado, símbolo) -> estado
    tabla      = M.fromList [ ((p, c), q) | (p, c, q) <- transicionesM mdd ]
    -- mapa de estados finales -> (token, prioridad) para decidir etiqueta
    finalesMap = M.fromList [ (s, (t, pr)) | (s, t, pr) <- finalesM mdd ]

    -- Recorre toda la entrada y acumula pares (token, lexema)
    go [] = []
    go s@(c:cs)
      | isSpace c = go cs                   -- saltamos espacios
      | otherwise =
          case reco (estadoInicialM mdd) "" s Nothing of
            Nothing ->
              error $ "Error léxico: símbolo inesperado '" ++ [c] ++ "'"
            Just (tok, lexema, resto) ->
              (tok, lexema) : go resto

    -- La función se encarga de hacer el maximal munch, avanza lo más posible
    -- guardando el último final visto para cortar ahí si ya no hay transición.
    reco :: Int -> String -> String -> Maybe (String, String, String)
         -> Maybe (String, String, String)
    -- Si se acabó la entrada, devolvemos lo último final válido (si hubo)
    reco _ _ [] lastFin =
      case lastFin of
        Just (t, lex, rest) -> Just (t, lex, rest)
        Nothing             -> Nothing
    -- Miramos transición; si no hay, cortamos en el último final guardado
    reco st acc (x:xs) lastFin =
      case M.lookup (st, x) tabla of
        Nothing ->
          case lastFin of
            Just (t, lex, rest) -> Just (t, lex, rest)   -- regresamos al último aceptado
            Nothing             -> Nothing               -- no hay token válido
        Just st' ->
          let acc' = acc ++ [x]                          -- extendemos el lexema
              lastFin' =
                case M.lookup st' finalesMap of
                  Just (t, _pr) -> Just (t, acc', xs)    
                  Nothing       -> lastFin
          in reco st' acc' xs lastFin'

-- La función se encarga de armar un lexer a partir de AFDs:
-- primero minimiza cada AFD, construye la MDD y devuelve la función lexer.
buildLexerFromAFDs :: [(String, D.AFD)] -> (String -> [(String,String)])
buildLexerFromAFDs xs =
  let afdsMin = [ (name, minimizar afd) | (name, afd) <- xs ]
      mdd     = buildMDD afdsMin
  in lexerM mdd

-- La función se encarga de armar un lexer desde regex:
-- regex -> AFNε -> AFN -> AFD -> AFD mínimo; luego MDD y listo.
buildLexerFromRegex :: [(String, Expresion)] -> (String -> [(String, String)])
buildLexerFromRegex toks =
  let afds = [ (name, minimizar (afnAafd (afnEpToAfn (regexToAfne expr))))
             | (name, expr) <- toks
             ]
      mdd  = buildMDD afds
  in lexerM mdd
