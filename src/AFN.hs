module AFN
-- Automata finito no determinista y funciones basicas

(
  Estado,
  Simbolo,
  Transicion,
  AFN(..),
  mover,
  normalizarEstados
) where


import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- estado es un entero que identifica una bolita del automata
type Estado = Int

-- simbolo es un caracter del alfabeto del automata
type Simbolo = Char

-- una transicion es de la forma (origen, simbolo, destino)
type Transicion = (Estado, Simbolo, Estado)


-- estructura del afn
data AFN = AFN
  { estados        :: [Estado]
  , alfabeto       :: [Simbolo]       
  , transiciones   :: [Transicion] --lista de transiciones (p, c, q)    
  , estadoInicial  :: Estado
  , estadosFinales :: [Estado]
  } deriving (Eq, Show)

-- mover calcula el conjunto de destinos desde un conjunto de estados qs
-- leyendo el simbolo c segun las transiciones del afn
-- si no hay transiciones por c desde algun estado, ese estado no aporta destinos
mover :: AFN -> S.Set Estado -> Simbolo -> S.Set Estado
mover a qs c =
  let tabla = M.fromListWith (++)
        [ ((p,c'), [q]) | (p, c', q) <- transiciones a ]
      destinos = [ q | p <- S.toList qs
                     , q <- M.findWithDefault [] (p,c) tabla ]
  in S.fromList destinos

-- normalizarEstados renumera los estados en orden creciente comenzando en 0
-- y aplica esa renumeracion a transiciones, inicial y finales
normalizarEstados :: AFN -> AFN
normalizarEstados a =
  let vs = L.nub (estados a)
      orden = L.sort vs
      asign = M.fromList (zip orden [0..])
      ren s = M.findWithDefault s s asign
      renT (p, c, q) = (ren p, c, ren q)
  in AFN
     { estados        = map ren orden
     , alfabeto       = L.nub (alfabeto a)
     , transiciones   = map renT (transiciones a)
     , estadoInicial  = ren (estadoInicial a)
     , estadosFinales = map ren (estadosFinales a)
     }
