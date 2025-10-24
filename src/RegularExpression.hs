module RegularExpression
-- este modulo define una representacion simple de expresiones regulares


(
  Expresion(..)
) where

--La expresion puede ser:
--Term: un simbolo individual del alfabeto
--Concat e1 e2: concatenacion de dos expresiones
--Or e1 e2: sirve para expresar alternativas entre dos expresiones
--Star e: estrella de Kleene para repitir e cero o mas veces
data Expresion =
            Term Char         
          | Concat Expresion Expresion   
          | Or Expresion Expresion       
          | Star Expresion        

-- la instancia show imprime la expresion en una forma legible
instance Show Expresion where
  show (Term a)     = a : []
  show (Concat a b) = "(" ++ show a ++ show b ++ ")"
  show (Or a b)     = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Star a)   = show a ++ "*"
