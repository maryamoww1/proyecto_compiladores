module AFNe
-- este modulo define un automata finito no determinista con epsilon transiciones
(
  Estado,
  Simbolo,
  Transicion,
  AFNe(..),
  afneUnidad,
  cerraduraEpsilon,
  mover,
  normalizarEstados
) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M

type Estado = Int
type Simbolo = Char
type Transicion = (Estado, Maybe Simbolo, Estado)


-- estructura del afne:
-- - estados: lista de estados presentes
-- - alfabeto: lista de simbolos observables
-- - transiciones: (p, maybe c, q), con c ausente para epsilon
data AFNe = AFNe
  { estados        :: [Estado]
  , alfabeto       :: [Simbolo]
  , transiciones   :: [Transicion]
  , estadoInicial  :: Estado
  , estadosFinales :: [Estado]
  } deriving (Eq, Show)

-- afneUnidad construye un automata elemental que reconoce exactamente el simbolo c
-- crea dos estados consecutivos s y s+1 y una transicion etiquetada con c
afneUnidad :: Estado -> Simbolo -> AFNe
afneUnidad s c =
  let f = s + 1
  in AFNe { estados        = [s, f]
          , alfabeto       = [c]
          , transiciones   = [(s, Just c, f)]
          , estadoInicial  = s
          , estadosFinales = [f]
          }

-- cerraduraEpsilon calcula todos los estados alcanzables desde qs usando solo transiciones epsilon
-- recorre en lo ancho agregando destinos por epsilon hasta que no haya nuevos estados
-- devuelve el conjunto visitado que incluye qs y sus alcances por epsilon
cerraduraEpsilon :: AFNe -> S.Set Estado -> S.Set Estado
cerraduraEpsilon a qs =
  let epsSalidas = M.fromListWith (++)
        [ (p, [q]) | (p, Nothing, q) <- transiciones a ]
      paso visitados frontera =
        case S.minView frontera of
          Nothing -> visitados
          Just (x, resto) ->
            let sigs   = S.fromList (M.findWithDefault [] x epsSalidas)
                nuevos = S.difference sigs visitados
            in paso (S.union visitados nuevos) (S.union resto nuevos)
  in paso qs qs


-- mover calcula destinos por un simbolo c desde un conjunto de estados qs
-- solo usa transiciones con etiqueta just c, ignora epsilons
mover :: AFNe -> S.Set Estado -> Simbolo -> S.Set Estado
mover a qs c =
  let tabla = M.fromListWith (++)
        [ ((p,c'), [q]) | (p, Just c', q) <- transiciones a ]
      destinos = [ q | p <- S.toList qs
                     , q <- M.findWithDefault [] (p,c) tabla ]
  in S.fromList destinos

-- normalizarEstados re-enumera estados desde 0 en orden creciente y aplica la renumeracion a transiciones, inicial y finales
normalizarEstados :: AFNe -> AFNe
normalizarEstados a =
  let vs     = L.nub (estados a)
      orden  = L.sort vs
      asign  = M.fromList (zip orden [0..])
      ren s  = M.findWithDefault s s asign
      renT (p, m, q) = (ren p, m, ren q)
  in AFNe
     { estados        = map ren orden
     , alfabeto       = L.nub (alfabeto a)
     , transiciones   = map renT (transiciones a)
     , estadoInicial  = ren (estadoInicial a)
     , estadosFinales = map ren (estadosFinales a)
     }
