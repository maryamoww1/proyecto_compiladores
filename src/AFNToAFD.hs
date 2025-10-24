module AFNToAFD
-- convierte un afn en un afd 
-- usamos construccion de subconjuntos
(
  afnAafd
) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M

import qualified AFN as N
import qualified AFD as D

-- entrada: un afn
-- salida: un afd equivalente que reconoce el mismo lenguaje
afnAafd :: N.AFN -> D.AFD
afnAafd a =
  let alf = L.nub (N.alfabeto a) -- alfabeto sin repetidos
      --conjunto de estados finales del afn
      finalesAFN :: S.Set N.Estado
      finalesAFN = S.fromList (N.estadosFinales a)
      -- conjunto inicial para el afd: solo el estado inicial del afn
      inicialSet :: S.Set N.Estado
      inicialSet = S.fromList [N.estadoInicial a]


      --realiza la expansion sobre la cola de conjuntos pendientes
      paso :: [S.Set N.Estado]
           -> M.Map (S.Set N.Estado) D.Estado
           -> M.Map (D.Estado, D.Simbolo) D.Estado
           -> D.Estado
           -> ( M.Map (S.Set N.Estado) D.Estado
              , M.Map (D.Estado, D.Simbolo) D.Estado )
      paso [] asign trans _ = (asign, trans)
      paso (q:cola) asign trans nextId =  -- nextId: proximo id libre para nuevos conjuntos
        let idQ = asign M.! q
            (asign', cola', trans', nextId') =
              L.foldl'
                (\(ma, co, mt, nid) c ->
                   let destinoSet = N.mover a q c
                   in if S.null destinoSet
                      then (ma, co, mt, nid)  
                      else case M.lookup destinoSet ma of
                             Just idD ->
                               (ma, co, M.insert (idQ, c) idD mt, nid)
                             Nothing  ->
                               let idD = nid
                                   ma' = M.insert destinoSet idD ma
                               in (ma', co ++ [destinoSet], M.insert (idQ, c) idD mt, nid + 1)
                )
                (asign, cola, trans, nextId) alf
        in paso cola' asign' trans' nextId'
      -- asignacion inicial: el conjunto {inicial} recibe id 0
      asign0 = M.fromList [(inicialSet, 0)]
      -- expandimos desde el conjunto inicial
      (asignFinal, transMap) = paso [inicialSet] asign0 M.empty 1
      -- estados del afd: ids de todos los conjuntos descubiertos
      estadosAFD = L.sort (M.elems asignFinal)
      -- estados finales del afd: aquellos cuyo conjunto toca finales del afn
      finalesAFD =
        [ idQ
        | (qSet, idQ) <- M.toList asignFinal
        , not (S.null (S.intersection qSet finalesAFN))
        ]

      transAFD =
        [ (p, c, q)
        | ((p,c), q) <- M.toList transMap
        ]
      -- afd crudo es el resultado directo de la construcción, es aún "sin pulir" 
      afdCrudo = D.AFD
        { D.estados        = estadosAFD
        , D.alfabeto       = alf
        , D.transiciones   = L.sort transAFD
        , D.estadoInicial  = 0
        , D.estadosFinales = L.nub finalesAFD
        }

      afdNorm = D.normalizarEstados afdCrudo

      afdFinal = afdNorm

  in afdFinal

