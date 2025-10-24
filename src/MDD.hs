{-# LANGUAGE NamedFieldPuns #-}
-- Módulo para construir una MDD a partir de varios AFDs
-- De deben tiquetar estados que aceptan
-- y priorizar por orden dado.
module MDD
  ( 
    MDD(..)
  , EstadoM
  , Etiqueta
  , buildMDD
  , buildMDDFromAFDs
  , buildMDDFromMinAFDs
  ) where

import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import           Data.Maybe       (fromMaybe)
import qualified AFD              as D
import           AFDMin           (minimizar)

-- Estado entero de la MDD.
type EstadoM = Int
-- Etiqueta con (token, prioridad) para estados finales de la MDD.
type Etiqueta = (String, Int)

-- Estructura de la MDD: estados, alfabeto compartido, transiciones, inicio y finales etiquetados.
data MDD = MDD
  { estadosM        :: [EstadoM]
  , alfabetoM       :: [D.Simbolo]
  , transicionesM   :: [(EstadoM, D.Simbolo, EstadoM)]
  , estadoInicialM  :: EstadoM
  , finalesM        :: [(EstadoM, String, Int)]
  } deriving (Eq, Show)

-- Construye la MDD a partir de una lista de (nombre, AFD).
-- Totaliza cada AFD, sincroniza el alfabeto, hace un BFS sobre loe estados,
-- y etiqueta finales con el primer match según prioridad.
buildMDD :: [(String, D.AFD)] -> MDD
buildMDD namedAFDs =
  let afds        = map snd namedAFDs
      nombres     = map fst namedAFDs
      prioridades = [0..length afds - 1]

      -- Unimos alfabetos de todos los AFDs.
      sigma = L.nub . L.sort $ concatMap D.alfabeto afds

      -- Aseguramos que cada AFD esté totalizado sobre sigma.
      afdsTot = map (totalizar sigma) afds

      -- Vector de estados iniciales (uno por AFD).
      q0Vec = map D.estadoInicial afdsTot

      -- Exploramos y asignamos ids, juntamos transiciones y finales.
      (stateIdMap, trans, finals) = explora sigma afdsTot nombres prioridades q0Vec
      nStates = M.size stateIdMap
      estados = [0 .. nStates - 1]
      finalesEtiquetados =
        [ (sid, tok, pr)
        | (sid, (tok, pr)) <- M.toList finals
        ]
  in MDD { estadosM = estados
         , alfabetoM = sigma
         , transicionesM = trans
         , estadoInicialM = 0
         , finalesM = finalesEtiquetados
         }

-- Recorre el producto de AFDs (por símbolo) asignando ids a cada vector de estados.
-- También determina si un estado del producto es final y con qué etiqueta
explora
  :: [D.Simbolo]
  -> [D.AFD]
  -> [String]
  -> [Int]
  -> [D.Estado]
  -> (M.Map [D.Estado] EstadoM, [(EstadoM, D.Simbolo, EstadoM)], M.Map EstadoM Etiqueta)
explora sigma afds nombres pris q0 =
  go (M.singleton q0 0) [(q0, 0)] [] M.empty
  where
    -- Caso base: ya no hay pendientes: devolvemos todo lo acumulado.
    go m [] accT accF = (m, L.nub accT, accF)
    -- Tomamos un vector de estados con su id, generamos transiciones y etiquetamos si aplica.
    go m ((vec, sid):pend) accT accF =
      let etq = etiquetaEstado afds nombres pris vec
          pasos = [ (a, nextVec vec a) | a <- sigma ]
          (m', nuevos, transNuevas) = insertaEstados sid m pasos
          accF' = case etq of
                    Nothing     -> accF
                    Just (t, p) -> M.insert sid (t, p) accF
      in go m' (pend ++ nuevos) (accT ++ transNuevas) accF'

    -- Aplica un símbolo al vector: avanza cada AFD con movr
    nextVec vec a = [ step i q a | (i,q) <- zip [0..] vec ]

    -- Un paso en el i-ésimo AFD; asumimos totalizados
    step i q a =
      case D.mover (afds !! i) q a of
        Just q' -> q'
        Nothing -> error "AFD no totalizado: mover devolvió Nothing"

    -- Inserta estados nuevos en el mapa y crea transiciones desde 'sid'.
    insertaEstados
      :: EstadoM
      -> M.Map [D.Estado] EstadoM
      -> [(D.Simbolo, [D.Estado])]
      -> ( M.Map [D.Estado] EstadoM
         , [([D.Estado], EstadoM)]
         , [(EstadoM, D.Simbolo, EstadoM)]
         )
    insertaEstados sid m pasos =
      let foldF (macc, news, transs) (a, v) =
            case M.lookup v macc of
              -- si existe agregamos la transición.
              Just sid' -> (macc, news, (sid, a, sid') : transs)
              -- si no existe: le damos un id nuevo y lo pponemos en la cola
              Nothing   ->
                let sid'  = M.size macc
                    macc' = M.insert v sid' macc
                in (macc', news ++ [(v, sid')], (sid, a, sid') : transs)
      in L.foldl' foldF (m, [], []) pasos

-- Decide si un estado del producto es final y qué etiqueta se queda.
-- Si varios AFDs aceptan, gana el de menor prioridad.
etiquetaEstado
  :: [D.AFD] -> [String] -> [Int] -> [D.Estado] -> Maybe Etiqueta
etiquetaEstado afds nombres pris vec =
  let candidatos =
        [ (nombres !! i, pris !! i)
        | (i, (afd, q)) <- zip [0..] (zip afds vec)
        , q `elem` D.estadosFinales afd
        ]
  in case L.sortOn snd candidatos of
       []      -> Nothing
       (x : _) -> Just x

-- Asegura que un AFD tenga todas las transiciones definidas para todo sigma.
-- Si falta alguna, agrega un estado y completa.
totalizar :: [D.Simbolo] -> D.AFD -> D.AFD
totalizar sigma afd =
  let es   = D.estados afd
      alf  = L.nub (D.alfabeto afd ++ sigma)
      ts   = D.transiciones afd
      tab  = M.fromList [ ((p,c), q) | (p,c,q) <- ts ]
      sink = if null es then 0 else maximum es + 1
      faltan = [ (p,c) | p <- es, c <- alf, M.notMember (p,c) tab ]
      tsFalt = [ (p,c,sink) | (p,c) <- faltan ]
      loops  = [ (sink,c,sink) | c <- alf ]
      es' = L.sort (if null faltan then es else sink : es)
      ts' = if null faltan then ts else L.nub (ts ++ tsFalt ++ loops)
  in afd { D.estados = es', D.alfabeto = alf, D.transiciones = ts' }

-- Se construye una MDD directamente de AFDs tal cual.
buildMDDFromAFDs :: [(String, D.AFD)] -> MDD
buildMDDFromAFDs = buildMDD

-- Igual que arriba, pero primero minimiza cada AFD antes de construir la MDD.
buildMDDFromMinAFDs :: [(String, D.AFD)] -> MDD
buildMDDFromMinAFDs xs = buildMDD [ (name, minimizar afd) | (name, afd) <- xs ]
