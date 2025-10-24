# Tokens del lenguaje IMP
-- L = letra (mayúscula o minúscula)
-- D = dígito del 1 al 9
-- Z = dígito del 0 al 9

num = Or [Term 0] [Or [Concat [Term D] [Star [Or [Term 0] [Term D]]]] [Concat [Term -] [Concat [Term D] [Star [Or [Term 0] [Term D]]]]]]
# Regex: 0|[1-9][0-9]|-[1-9][0-9]

ident = Concat [Term L] [Concat [Star [Term L]] [Star [Term Z]]]
# Regex: [a-zA-Z][a-zA-Z][0-9]

asign = Concat [Term :] [Term =]
# Regex: :=

opArit = Or [Term +] [Or [Term -] [Or [Term *] [Term /]]]
# Regex: \+|-|\*|/

opRel = Or [Term <] [Or [Term >] [Term =]]
# Regex: <|>|=

opBool = Or [Concat [Term n] [Concat [Term o] [Term t]]] [Or [Concat [Term a] [Concat [Term n] [Term d]]] [Concat [Term o] [Term r]]]
# Regex: not|and|or

bool = Or [Concat [Term t] [Concat [Term r] [Concat [Term u] [Term e]]]] [Concat [Term f] [Concat [Term a] [Concat [Term l] [Concat [Term s] [Term e]]]]]
# Regex: true|false

reservCond = Or [Concat [Term i] [Term f]] [Or [Concat [Term t] [Concat [Term h] [Concat [Term e] [Term n]]]] [Concat [Term e] [Concat [Term l] [Concat [Term s] [Concat [Term e]]]]]]
# Regex: if|then|else

reservCiclo = Or [Concat [Term w] [Concat [Term h] [Concat [Term i] [Concat [Term l] [Term e]]]]] [Or [Concat [Term d] [Term o]] [Concat [Term f] [Concat [Term o] [Term r]]]]
# Regex: while|do|for

reservSkip = Concat [Term s] [Concat [Term k] [Concat [Term i] [Term p]]]
# Regex: skip

delim = Or [Term {] [Or [Term }] [Or [Term (] [Term )]]]
# Regex: \{|\}|\(|\)

puntuacion = Term ;
# Regex: ;