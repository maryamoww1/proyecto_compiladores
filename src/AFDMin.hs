{-# LANGUAGE NamedFieldPuns #-}
-- Módulo para minimizar AFDs: reduce el autómata quitando estados innecesarios.
module AFDMin
  ( minimizar
  , afdToAFMin
  ) where

import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S

import AFD
  ( AFD(..)
  , Estado
  , Simbolo
  , Transicion
  , mover
  )

-- La función se encarga de hacer todo el proceso de minimización del AFD.
-- Quita estados inalcanzables, agrega el nodo de muerte si falta,
-- separa los estados en grupos equivalentes y reconstruye el AFD mínimo.
minimizar :: AFD -> AFD
minimizar afd0 =
  let afd1 = eliminarInalcanzables afd0
      afdC = completarConnodoMuerte afd1
      p0   = particionInicial afdC
      pF   = puntoFijo (refinar afdC) p0
  in reconstruir afdC pF

-- Minimizamos
afdToAFMin :: AFD -> AFD
afdToAFMin = minimizar

-- Quita los estados que no se pueden alcanzar desde el estado inicial.
eliminarInalcanzables :: AFD -> AFD
eliminarInalcanzables AFD{estados, alfabeto, transiciones, estadoInicial, estadosFinales} =
  let alfabeto' = L.nub alfabeto
      tabla     = aMapa transiciones
      reach     = alcanzables alfabeto' tabla estadoInicial
      est'      = L.sort (S.toList reach)
      trans'    = [ (p,c,q)
                  | (p,c,q) <- transiciones
                  , S.member p reach
                  , S.member q reach
                  ]
      fin'      = L.sort [ q | q <- estadosFinales, S.member q reach ]
  in AFD { estados = est'
         , alfabeto = alfabeto'
         , transiciones = trans'
         , estadoInicial = estadoInicial
         , estadosFinales = fin'
         }

-- Busca todos los estados alcanzables desde el inicial recorriendo las transiciones.
alcanzables :: [Simbolo] -> M.Map (Estado,Simbolo) Estado -> Estado -> S.Set Estado
alcanzables sigma delta q0 = go S.empty (S.singleton q0)
  where
    go vis pend
      | S.null pend = vis
      | otherwise   =
          let (q,rest) = S.deleteFindMin pend
              vis'     = S.insert q vis
              succs    = S.fromList
                        [ q' | a <- sigma, Just q' <- [M.lookup (q,a) delta] ]
              new      = S.difference succs vis'
          in go vis' (S.union rest new)

-- Agrega un nodo de muerte si faltan transiciones para completar el AFD.
completarConnodoMuerte :: AFD -> AFD
completarConnodoMuerte AFD{estados, alfabeto, transiciones, estadoInicial, estadosFinales} =
  let sigma      = L.nub alfabeto
      tabla      = aMapa transiciones
      falta (q,a) = M.notMember (q,a) tabla
      pares       = [ (q,a) | q <- estados, a <- sigma ]
      hayHuecos   = any falta pares
      nodoMuerte  = if null estados then 0 else (maximum estados + 1)
      transComp   = [ (q,a, maybe nodoMuerte id (M.lookup (q,a) tabla)) | (q,a) <- pares ]
      transSink   = [ (nodoMuerte,a,nodoMuerte) | a <- sigma ]
  in if not hayHuecos
       then AFD { estados = estados
                , alfabeto = sigma
                , transiciones = transiciones
                , estadoInicial = estadoInicial
                , estadosFinales = estadosFinales
                }
       else AFD { estados = L.sort (nodoMuerte : estados)
                , alfabeto = sigma
                , transiciones = L.nub (transComp ++ transSink)
                , estadoInicial = estadoInicial
                , estadosFinales = estadosFinales
                }

-- Representa una partición de estados, o sea los grupos en los que se separan.
type Particion = [S.Set Estado]

-- Crea la partición inicial separando estados finales y no finales.
particionInicial :: AFD -> Particion
particionInicial AFD{estados, estadosFinales} =
  let f = S.fromList estadosFinales
      n = S.difference (S.fromList estados) f
      bloques = filter (not . S.null) [f,n]
  in ordenarBloques bloques

-- Refina la partición actual separando los estados según sus transiciones.
refinar :: AFD -> Particion -> Particion
refinar afd p = ordenarBloques (concatMap (partirBloque afd p) p)

-- Divide un bloque en subgrupos según su comportamiento con respecto a la partición.
partirBloque :: AFD -> Particion -> S.Set Estado -> [S.Set Estado]
partirBloque afd p b =
  let clases = M.elems (agruparPorFirma afd p b)
  in if length clases <= 1 then [b] else clases

-- Agrupa los estados del bloque según su "firma", o sea las clases a donde van por cada símbolo.
agruparPorFirma
  :: AFD -> Particion -> S.Set Estado -> M.Map [Int] (S.Set Estado)
agruparPorFirma AFD{alfabeto, transiciones} p b =
  let sigma     = L.nub alfabeto
      delta     = aMapa transiciones
      classOf q = indiceBloque p q
      step q a  = M.lookup (q,a) delta
      firma q   = [ maybe (-1) classOf (step q a) | a <- sigma ]
      ins q m   = M.insertWith S.union (firma q) (S.singleton q) m
  in S.foldr ins M.empty b

-- Devuelve el índice del bloque al que pertenece un estado.
indiceBloque :: Particion -> Estado -> Int
indiceBloque p q =
  case [ i | (i,blk) <- zip [0..] p, S.member q blk ] of
    (i:_) -> i
    []    -> error "indiceBloque: estado fuera de la partición"

-- Ordena los bloques para mantener un orden estable en la reconstrucción.
ordenarBloques :: [S.Set Estado] -> [S.Set Estado]
ordenarBloques =
  L.sortOn (\s -> if S.null s then (-1) else S.findMin s)

-- Aplica una función hasta que el resultado no cambie (punto fijo).
puntoFijo :: Eq a => (a -> a) -> a -> a
puntoFijo f x = let x' = f x in if x' == x then x else puntoFijo f x'

-- Reconstruye el nuevo AFD a partir de las clases de estados equivalentes.
reconstruir :: AFD -> Particion -> AFD
reconstruir AFD{alfabeto, transiciones, estadoInicial, estadosFinales} clases =
  let sigma     = L.nub alfabeto
      clasesOrd = ordenarBloques clases               
      idxDe q   = case [ i | (i,blk) <- zip [0..] clasesOrd, S.member q blk ] of
                    (i:_) -> i
                    []    -> error "reconstruir: estado sin clase"
      estados'  = [0 .. length clasesOrd - 1]
      finals'   = L.sort . L.nub $
                  [ idxDe q | q <- estadosFinales, any (S.member q) clasesOrd ]
      start'    = idxDe estadoInicial
      delta     = aMapa transiciones
      rep c     = S.findMin c
      paso c a  = case M.lookup (rep c, a) delta of
                    Just q' -> idxDe q'
                    Nothing -> idxDe (rep c) 
      trans'    = [ (i, a, paso blk a)
                  | (i, blk) <- zip [0..] clasesOrd
                  , a <- sigma
                  ]
  in AFD { estados        = estados'
         , alfabeto       = sigma
         , transiciones   = L.nub trans'
         , estadoInicial  = start'
         , estadosFinales = finals'
         }

-- Convierte una lista de transiciones a un mapa para buscar más fácil.
aMapa :: [Transicion] -> M.Map (Estado,Simbolo) Estado
aMapa ts = M.fromList [ ((p,a),q) | (p,a,q) <- ts ]
