module AFNeToAFN
-- este modulo convierte un afn-e a un afn equivalente sin epsilons

(
  afnEpToAfn
) where


import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M

import qualified AFNe as E
import qualified AFN  as N

--esta funcion construye un mapa que asocia cada estado p con su cerradura epsilon (todos los estados alcanzables desde p usando solo epsilons)
tablaCerradura :: E.AFNe -> M.Map E.Estado (S.Set E.Estado)
tablaCerradura a =
  let es = E.estados a
  in M.fromList [ (p, E.cerraduraEpsilon a (S.singleton p)) | p <- es ]-- S.singleton crea un conjunto que contiene exactamente un elemento

-- devuelve los destinos alcanzables por transiciones etiquetadas con c (sin usar epsilons aqui)
moverEp :: E.AFNe -> S.Set E.Estado -> E.Simbolo -> S.Set E.Estado
moverEp a qs c =
  let tabla = M.fromListWith (++)
        [ ((p,c'), [q]) | (p, Just c', q) <- E.transiciones a ]
      destinos = [ q | p <- S.toList qs
                     , q <- M.findWithDefault [] (p,c) tabla ]
  in S.fromList destinos

-- esta funcion dice si un conjunto de estados (casi siempre una cerradura epsilon) contiene algun estado final del automata; si lo contiene, ese conjunto representa aceptación

esFinalDerivado :: E.AFNe -> S.Set E.Estado -> Bool
esFinalDerivado a cerradura =
  not . S.null $ S.intersection cerradura (S.fromList (E.estadosFinales a))


-- afnEpToAfn así:
-- 1. p.c. estado p, calculamos su cerradura epsilon cE(p)
-- 2. p.c. par (p, c), desde cE(p) avanzamos con el simbolo c y luego aplicamos cerradura epsilon otra vez para capturar los destinos indirectos
-- 3. las transiciones resultantes quedan solo con simbolos (sin epsilons), y un estado p se considera final si cE(p) contiene algun final del afne
-- 4. construimos el afn y lo normalizamos para tener una numeracion clara y ordenada
afnEpToAfn :: E.AFNe -> N.AFN
afnEpToAfn a =
  let todos = E.estados a
      alf   = E.alfabeto a

      cerraduras = tablaCerradura a-- cerraduras epsilon por estado
      cE p = M.findWithDefault (S.singleton p) p cerraduras

      ini = E.estadoInicial a
      -- para cada estado p y simbolo c, destinos alcanzables: primero cerradura en p, luego movimiento por c, y por ultimo cerradura
      tablaPC :: M.Map (E.Estado, E.Simbolo) (S.Set E.Estado)
      tablaPC =
        M.fromList
          [ ((p,c), E.cerraduraEpsilon a (moverEp a (cE p) c))
          | p <- todos
          , c <- alf
          ]

      transSinE =
        [ (p, c, q)
        | ((p,c), qs) <- M.toList tablaPC
        , q <- S.toList qs
        ]
      -- estados p que deben ser finales en el afn
      -- porque su cerradura epsilon incluye un final del afne
      finalesDerivados =
        [ p | p <- todos
            , esFinalDerivado a (cE p)
        ]
      -- orden y deduplicacion para estabilidad
      transUnicas = L.nub (L.sort transSinE)
      -- construccion del afn antes de normalizar
      afnCrudo = N.AFN
        { N.estados        = L.nub (todos ++ [ ini ])
        , N.alfabeto       = L.nub alf
        , N.transiciones   = transUnicas
        , N.estadoInicial  = ini
        , N.estadosFinales = L.nub finalesDerivados
        }
  in N.normalizarEstados afnCrudo
