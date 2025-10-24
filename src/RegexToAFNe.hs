module RegexToAFNe
(
  regexToAfne
) where


import qualified Data.Set as S
import qualified Data.List as L
import AFNe
import RegularExpression


unionAlfabeto :: [Simbolo] -> [Simbolo] -> [Simbolo]
unionAlfabeto a b = L.nub (a ++ b)

construirTermino :: Int -> Char -> (AFNe, Int)
construirTermino sig c =
  let a = afneUnidad sig c
  in (a, sig + 2)

construirOr :: Int -> AFNe -> AFNe -> (AFNe, Int)
construirOr sig a b =
  let s = sig
      f = sig + 1
      sig' = sig + 2
      eps x y = (x, Nothing, y)
      trans = transiciones a
           ++ transiciones b
           ++ [ eps s (estadoInicial a)
              , eps s (estadoInicial b)
              ]
           ++ [ eps x f | x <- estadosFinales a ]
           ++ [ eps y f | y <- estadosFinales b ]
      ests = s : f : (estados a ++ estados b)
      alf  = unionAlfabeto (alfabeto a) (alfabeto b)
      r = AFNe ests alf trans s [f]
  in (r, sig')

construirConcat :: AFNe -> AFNe -> AFNe
construirConcat a b =
  let eps x y = (x, Nothing, y)
      trans = transiciones a
           ++ transiciones b
           ++ [ eps x (estadoInicial b) | x <- estadosFinales a ]
      ests = estados a ++ estados b
      alf  = unionAlfabeto (alfabeto a) (alfabeto b)
  in AFNe ests alf trans (estadoInicial a) (estadosFinales b)

construirEstrella :: Int -> AFNe -> (AFNe, Int)
construirEstrella sig a =
  let s = sig
      f = sig + 1
      sig' = sig + 2
      eps x y = (x, Nothing, y)
      trans = transiciones a
           ++ [ eps s (estadoInicial a)
              , eps s f
              ]
           ++ [ eps x (estadoInicial a) | x <- estadosFinales a ]
           ++ [ eps x f | x <- estadosFinales a ]
      ests = s : f : estados a
      r = AFNe ests (alfabeto a) trans s [f]
  in (r, sig')

regexToAfne :: Expresion -> AFNe
regexToAfne e =
  let (a, _) = go 0 e
  in normalizarEstados a
  where
    go :: Int -> Expresion -> (AFNe, Int)
    go sig (Term c)       = construirTermino sig c
    go sig (Or e1 e2)     =
      let (a1, s1) = go sig e1
          (a2, s2) = go s1  e2
          (r, s3)  = construirOr s2 a1 a2
      in (r, s3)
    go sig (Concat e1 e2) =
      let (a1, s1) = go sig e1
          (a2, s2) = go s1  e2
          r        = construirConcat a1 a2
      in (r, s2)
    go sig (Star e1)      =
      let (a1, s1) = go sig e1
          (r, s2)  = construirEstrella s1 a1
      in (r, s2)
