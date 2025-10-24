module Main where

import Data.Char (isSpace, isAlphaNum)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import RegularExpression (Expresion(..))  

main :: IO ()
main = do

  putStrLn "\nLeyendo especificación del lenguaje IMP..."
  specs <- readFile "./specs/IMP.md"

  let limpioSpecs = quitarComentarios specs

  let impTokens = buildTokensFromSpecs limpioSpecs

  let archivos =
        [ ("ejemplo1.md", "./samples/ejemplo1.md")
        , ("ejemplo2.md", "./samples/ejemplo2.md")
        , ("ejemplo3.md", "./samples/ejemplo3.md")
        , ("ejemplo4.md", "./samples/ejemplo4.md")
        , ("ejemplo5.md", "./samples/ejemplo5.md")
        ]

  menu archivos impTokens

-- Menú, lista archivos de ejemplo, pide opción, valida y procesa.
menu :: [(String, FilePath)] -> [(String, Expresion)] -> IO ()
menu archivos impTokens = do
  putStrLn ""
  putStrLn "Proyecto de Compiladores IMP: \n"
  forM_ (zip [1..] archivos) $ \(i, (nombre, _)) ->
    putStrLn $ "║ " ++ show i ++ ". " ++ padRight 28 nombre ++ "║"
  putStrLn "║ 0. Salir:\n"
  putStr   "Elige una opción: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "0" -> putStrLn "Saliendo..."
    n | n `elem` map show [1..length archivos] -> do
          let idx = read n - 1
          let (nombre, ruta) = archivos !! idx
          existe <- doesFileExist ruta
          if not existe
            then putStrLn $ "Aviso: no existe el archivo: " ++ ruta
            else do
              putStrLn $ "\nProcesando " ++ nombre ++ " ..."
              procesarEjemplo impTokens ruta
          putStrLn "\nPresiona ENTER para volver al menú..."
          _ <- getLine
          menu archivos impTokens
      | otherwise -> do
          putStrLn "Opción no válida."
          menu archivos impTokens

-- rellena con espacios a la derecha para alinear el menú.
padRight :: Int -> String -> String
padRight n s =
  let len = length s
  in if len >= n then take n s else s ++ replicate (n - len) ' '

-- imprime archivo original, sin comentarios,
-- marca qué líneas tenían comentarios y tokeniza.
procesarEjemplo :: [(String, Expresion)] -> FilePath -> IO ()
procesarEjemplo impTokens ruta = do
  putStrLn "--------------------------------------------"
  putStrLn $ "Archivo: " ++ ruta
  putStrLn "--------------------------------------------"

  progOriginal <- readFile ruta
  putStrLn "\n=== Programa ORIGINAL ==="
  putStrLn progOriginal

  let progSinComentarios = quitarComentarios progOriginal

  putStrLn "\n=== Programa SIN comentarios ==="
  putStrLn progSinComentarios

  putStrLn "=== Resumen por línea ==="
  let origLs  = lines progOriginal
      cleanLs = lines progSinComentarios
      maxLen  = max (length origLs) (length cleanLs)
      pad xs  = xs ++ replicate (maxLen - length xs) ""
      pairs   = zip (pad origLs) (pad cleanLs)
  mapM_ putStrLn (zipWith marcarComentarios [1..] pairs)

  putStrLn "\n=== Tokens generados ==="
  let todos = lexWithRE impTokens progSinComentarios
  mapM_ print todos

  putStrLn "\n=== Tokens (sin blancos) ==="
  let sinEspacios = filter (\(tag, _) -> tag /= "WS") todos
  mapM_ print sinEspacios

  putStrLn "\n=== Fin del análisis del archivo ==="

-- Etiqueta la línea según si tenía comentario total, parcial o ninguno.
marcarComentarios :: Int -> (String, String) -> String
marcarComentarios n (o, c) =
  let oTrim = trim o
      cTrim = trim c
      etiqueta
        | not (null oTrim) && null cTrim     = "Solo comentario"
        | oTrim /= cTrim && not (null cTrim) = "Comentario parcial (Existen cadenas fuera del comentario)"
        | otherwise                          = "Sin comentario"
  in "Línea " ++ show n ++ ": " ++ etiqueta

-- Quita espacios al inicio y al final de una línea.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- Quita por la derecha mientras se cumpla el predicado.
dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

-- Elimina bloques de comentarios:
--   *- ... -*   y   ** ... **,
quitarComentarios :: String -> String
quitarComentarios = goNormal
  where
    goNormal [] = []
    goNormal (a:b:xs)
      | a == '*' && b == '-' = goBlockDash xs         
      | a == '*' && b == '*' = goBlockStar xs         
      | otherwise            = a : goNormal (b:xs)
    goNormal [x] = [x]

    goBlockDash [] = []  
    goBlockDash (x:xs) =
      case xs of
        ('*':rest) | x == '-' -> goNormal rest        
        _ -> (if x=='\n' then '\n' else '\0') `cons` goBlockDash xs

    goBlockStar [] = []
    goBlockStar (x:xs) =
      case xs of
        ('*':rest) | x == '*' -> goNormal rest       
        _ -> (if x=='\n' then '\n' else '\0') `cons` goBlockStar xs

    cons '\0' acc = acc
    cons c    acc = c:acc

-- Azúcar para construir expresiones regulares básicas del lenguaje c:
char :: Char -> Expresion
char = Term

oneOf :: [Char] -> Expresion
oneOf []     = error "oneOf: vacío"
oneOf [c]    = char c
oneOf (c:cs) = Or (char c) (oneOf cs)

str :: String -> Expresion
str []     = error "str: vacío"
str [c]    = char c
str (c:cs) = Concat (char c) (str cs)

range :: Char -> Char -> Expresion
range a b = oneOf [a..b]

-- Definiciones  letras, dígitos y variantes útiles.
letter, digit, nonzero :: Expresion
letter  = Or (range 'a' 'z') (range 'A' 'Z')
digit   = range '0' '9'
nonzero = range '1' '9'

-- Espacios en blanco: un char y su clausura para WS.
wsChar :: Expresion
wsChar = oneOf " \t\r\n"

ws :: Expresion
ws = Concat wsChar (Star wsChar)

-- Palabras reservadas 
res_if, res_then, res_else, res_while, res_do, res_for, res_skip :: Expresion
res_if     = str "if"
res_then   = str "then"
res_else   = str "else"
res_while  = str "while"
res_do     = str "do"
res_for    = str "for"
res_skip   = str "skip"

-- Booleanos y operadores booleanos.
boolExpr, opBoolExpr :: Expresion
boolExpr   = Or (str "true") (str "false")
opBoolExpr = Or (str "not") (Or (str "and") (str "or"))

-- Operadores aritméticos y relacionales, y asignación.
asignExpr, opRelExpr, opAritExpr :: Expresion
asignExpr  = Concat (char ':') (char '=')
opRelExpr  = Or (char '<') (Or (char '>') (char '='))
opAritExpr = Or (char '+') (Or (char '-') (Or (char '*') (char '/')))

-- Delimitadores y puntuación.
puntuacionExpr, delimExpr :: Expresion
puntuacionExpr = char ';'
delimExpr      = Or (char '{') (Or (char '}') (Or (char '(') (char ')')))

-- Tokens: identificadores y números
identExpr, numExpr :: Expresion
identExpr = Concat letter (Concat (Star letter) (Star digit))
-- num: 0 | [1-9][0-9]* | -[1-9][0-9]*
numExpr   = Or (char '0')
           (Or (Concat nonzero (Star digit))
               (Concat (char '-') (Concat nonzero (Star digit))))

-- SE busca la Expresión por nombre para coincidir con lo definido en IMP.md.
lookupExprByName :: String -> Expresion
lookupExprByName name =
  case name of
    -- Reservadas / booleanos
    "reservCond"  -> Or res_if (Or res_then res_else)
    "reservCiclo" -> Or res_while (Or res_do res_for)
    "reservSkip"  -> res_skip
    "bool"        -> boolExpr
    "opBool"      -> opBoolExpr
    -- Operadores / delims
    "asign"       -> asignExpr
    "opRel"       -> opRelExpr
    "opArit"      -> opAritExpr
    "puntuacion"  -> puntuacionExpr
    "delim"       -> delimExpr
    "num"         -> numExpr
    "ident"       -> identExpr
    other         -> error $ "Token desconocido en lookup: " ++ show other

-- Valida si un char puede ser parte de un nombre de token.
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- Quita espacios por la izquierda.
trimL :: String -> String
trimL [] = []
trimL (c:cs) | isSpace c = trimL cs
             | otherwise = c:cs

-- Extrae nombres de tokens desde el texto de specs (ignorando comentarios y líneas raras).
extractTokenNames :: String -> [String]
extractTokenNames txt = uniq (go (lines txt))
  where
    go [] = []
    go (l:ls) =
      case parseName (trimL l) of
        Nothing   -> go ls
        Just name -> name : go ls

    parseName s
      | null s               = Nothing
      | "#"  `isPrefixOf` s  = Nothing
      | "--" `isPrefixOf` s  = Nothing
      | otherwise =
          case break (=='=') s of
            (lhs, _:' ':_) -> valid lhs
            (lhs, '=':_)   -> valid lhs
            _              -> Nothing
      where
        valid lhs =
          let nm  = takeWhile isNameChar lhs
              rest= drop (length nm) lhs
          in if nm /= "" && all isSpace rest then Just nm else Nothing

    isPrefixOf pfx s = pfx == take (length pfx) s

    uniq []     = []
    uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Construcción de la lista de tokens.
reservedOrder  :: [String]
reservedOrder  = ["reservCond","reservCiclo","reservSkip","bool","opBool"]

operatorsOrder :: [String]
operatorsOrder = ["asign","opRel","opArit"]

delimsOrder    :: [String]
delimsOrder    = ["puntuacion","delim"]

genericOrder   :: [String]
genericOrder   = ["num","ident"]

present :: [String] -> [String] -> [String]
present want have = [ n | n <- want, n `elem` have ]

-- Construye la lista (nombre, Expresión) desde el texto de specs en el orden deseado.
-- Se agrega "WS" primero para que el lexer lo considere.
buildTokensFromSpecs :: String -> [(String, Expresion)]
buildTokensFromSpecs specsText =
  ("WS", ws) :
  [ (name, lookupExprByName name) | name <- ordered ]
  where
    names   = extractTokenNames specsText
    ordered = concat
      [ present reservedOrder  names
      , present operatorsOrder names
      , present delimsOrder    names
      , present genericOrder   names
      , [ n | n <- names
            , n `notElem` (reservedOrder ++ operatorsOrder ++ delimsOrder ++ genericOrder) ]
      ]

--Devuelve longitudes que hacen match desde el inicio.
matches :: Expresion -> String -> [Int]
matches (Term c) (x:_) | c == x = [1]
matches (Term _) _               = []
matches (Concat a b) s           =
  [ la + lb | la <- matches a s, let rest = drop la s, lb <- matches b rest ]
matches (Or a b) s               = dedup (matches a s ++ matches b s)
matches (Star a) s               = 0 : more a s
  where
    more _ ""  = []
    more e str =
      let ps = filter (>0) (matches e str)
      in if null ps then [] else dedup (concat [ map (n+) (matches (Star e) (drop n str)) | n <- ps ])

-- Quita duplicados de una lista (manteniendo el primero que aparece).
dedup :: [Int] -> [Int]
dedup []     = []
dedup (x:xs) = x : dedup (filter (/= x) xs)

-- Elige la mejor longitud de match para un token dado.
bestLen :: (String, Expresion) -> String -> Int
bestLen (_, re) s = case matches re s of [] -> 0; xs -> maxList xs
  where maxList (y:ys) = foldl (\m v -> if v>m then v else m) y ys

-- Lexer basado solo en expresiones cn unmáximo avance, si nada coincide emite UNKNOWN y avanza 1.
lexWithRE :: [(String, Expresion)] -> String -> [(String,String)]
lexWithRE _    ""     = []
lexWithRE toks s@(c:cs) =
  let (bestName,bestL) = choose toks ("",0)
  in if bestL > 0
        then (bestName, take bestL s) : lexWithRE toks (drop bestL s)
        else ("UNKNOWN",[c])          : lexWithRE toks cs
  where
    choose [] acc = acc
    choose ((nm,re):ts) (bn,bl) =
      let l = bestLen (nm,re) s
      in if l > bl then choose ts (nm,l) else choose ts (bn,bl)