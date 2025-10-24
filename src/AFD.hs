module AFD

-- este modulo define un automata finito determinista
--para cada par (estado, simbolo) hay a lo sumo un destino
(
  Estado,
  Simbolo,
  Transicion,
  AFD(..),
  mover,
  normalizarEstados
) where


import qualified Data.List as L
import qualified Data.Map.Strict as M

type Estado = Int
type Simbolo = Char

-- en un afd no existen transiciones con maybe, siempre hay un simbolo explicito
type Transicion = (Estado, Simbolo, Estado)
--definicion del afd
data AFD = AFD
  { estados        :: [Estado]
  , alfabeto       :: [Simbolo]
  , transiciones   :: [Transicion]
  , estadoInicial  :: Estado
  , estadosFinales :: [Estado]
  } deriving (Eq, Show)

-- dado un afd, un estado p y un simbolo c devuelve el estado destino si existe la transicion (p, c) -> q, si no existe, regresa nothing

mover :: AFD -> Estado -> Simbolo -> Maybe Estado
mover a p c =
  let tabla = M.fromList [ ((x,s), y) | (x,s,y) <- transiciones a ]
  in M.lookup (p,c) tabla

-- normalizarEstados: renumera los estados de forma estable.
normalizarEstados :: AFD -> AFD
normalizarEstados a =
  let vs = L.nub (estados a)
      orden = L.sort vs
      asign = M.fromList (zip orden [0..])
      ren s = M.findWithDefault s s asign
      renT (p, c, q) = (ren p, c, ren q)
  in AFD
     { estados        = map ren orden
     , alfabeto       = L.nub (alfabeto a)
     , transiciones   = map renT (transiciones a)
     , estadoInicial  = ren (estadoInicial a)
     , estadosFinales = map ren (estadosFinales a)
     }
