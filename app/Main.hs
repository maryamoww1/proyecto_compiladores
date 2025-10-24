module Main where

import Data.Char (isSpace, isAlphaNum)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import RegularExpression (Expresion(..)) 

-- punto de entrada principal
main :: IO ()
main = do
  -- imprime cabecera simple del proyecto
  putStrLn "proyecto de compiladores imp"
  -- paso 1: leer el archivo de especificación imp.md
  putStrLn "leyendo especificacion del lenguaje imp..."
  specs <- readFile "./specs/IMP.md"
  -- paso 2: eliminar comentarios del texto de especificaciones
  let limpioSpecs = stripComments specs
  -- paso 3: construir la lista de tokens a partir de los nombres del lhs
  let impTokens = buildTokensFromSpecs limpioSpecs
  -- lista de ejemplos disponibles con nombre y ruta
  let archivos =
        [ ("ejemplo1.imp", "./samples/ejemplo1.md")
        , ("ejemplo2.imp", "./samples/ejemplo2.md")
        , ("ejemplo3.imp", "./samples/ejemplo3.md")
        , ("ejemplo4.imp", "./samples/ejemplo4.md")
        , ("ejemplo5.imp", "./samples/ejemplo5.md")
        ]
  -- muestra menú principal e inicia interacción
  menu archivos impTokens

-- menú interactivo simple sin adornos
menu :: [(String, FilePath)] -> [(String, Expresion)] -> IO ()
menu archivos impTokens = do
  -- imprime opciones del menú con numeración
  putStrLn ""
  putStrLn "proyecto de compiladores imp:"
  forM_ (zip [1..] archivos) $ \(i, (nombre, _)) ->
    putStrLn $ "║ " ++ show i ++ ". " ++ padRight 28 nombre ++ "║"
  putStrLn "║ 0. salir:"
  putStr   "elige una opcion: "
  hFlush stdout
  opcion <- getLine
  -- evalúa la opción elegida
  case opcion of
    "0" -> putStrLn "saliendo..."
    n | n `elem` map show [1..length archivos] -> do
          -- obtiene índice y archivo elegido
          let idx = read n - 1
          let (nombre, ruta) = archivos !! idx
          existe <- doesFileExist ruta
          if not existe
            then putStrLn $ "aviso: no existe el archivo: " ++ ruta
            else do
              -- procesa el ejemplo seleccionado
              putStrLn $ "procesando " ++ nombre ++ " ..."
              procesarEjemplo impTokens ruta
          -- espera enter y regresa al menú
          putStrLn "presiona enter para volver al menu..."
          _ <- getLine
          menu archivos impTokens
      | otherwise -> do
          -- maneja opción inválida
          putStrLn "opcion no valida."
          menu archivos impTokens

-- rellena a la derecha para alinear nombres en el menú
padRight :: Int -> String -> String
padRight n s =
  let len = length s
  in if len >= n then take n s else s ++ replicate (n - len) ' '

-- procesa un archivo imp usando los tokens construidos
procesarEjemplo :: [(String, Expresion)] -> FilePath -> IO ()
procesarEjemplo impTokens ruta = do
  -- encabezado del análisis
  putStrLn $ "archivo: " ++ ruta
  -- lee el programa original desde el archivo
  progOriginal <- readFile ruta
  putStrLn "=== programa original ==="
  putStrLn progOriginal
  -- elimina comentarios del programa
  let progSinComentarios = stripComments progOriginal
  putStrLn "=== programa sin comentarios ==="
  putStrLn progSinComentarios
  -- muestra resumen por línea comparando original y limpio
  putStrLn "=== resumen por linea ==="
  let origLs  = lines progOriginal
      cleanLs = lines progSinComentarios
      maxLen  = max (length origLs) (length cleanLs)
      pad xs  = xs ++ replicate (maxLen - length xs) ""
      pairs   = zip (pad origLs) (pad cleanLs)
  mapM_ putStrLn (zipWith marcarComentarios [1..] pairs)
  -- lexea el código sin comentarios con expresiones regulares
  putStrLn "=== tokens generados ==="
  let todos = lexWithRE impTokens progSinComentarios
  mapM_ print todos
  -- filtra y muestra tokens excluyendo espacios
  putStrLn "=== tokens (sin blancos) ==="
  let sinEspacios = filter (\(tag, _) -> tag /= "WS") todos
  mapM_ print sinEspacios
  putStrLn "fin del analisis del archivo"

-- ayuda visual para marcar estado de cada línea respecto a comentarios
marcarComentarios :: Int -> (String, String) -> String
marcarComentarios n (o, c) =
  let oTrim = trim o
      cTrim = trim c
      etiqueta
        -- solo comentario si original tiene contenido y limpio queda vacío
        | not (null oTrim) && null cTrim     = "solo comentario"
        -- comentario parcial si cambia pero queda algo fuera del comentario
        | oTrim /= cTrim && not (null cTrim) = "comentario parcial (existen cadenas fuera del comentario)"
        -- sin comentario en otro caso
        | otherwise                          = "sin comentario"
  in "linea " ++ show n ++ ": " ++ etiqueta

-- recorta espacios al inicio y final
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- recorta desde el final mientras se cumpla el predicado
dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

-- eliminación de comentarios tipo *- -* y ** **, no anidados, preservando \n
stripComments :: String -> String
stripComments = goNormal
  where
    -- estado normal, copia carácter a carácter salvo inicio de bloque
    goNormal [] = []
    goNormal (a:b:xs)
      | a == '*' && b == '-' = goBlockDash xs         -- inicio bloque *-
      | a == '*' && b == '*' = goBlockStar xs         -- inicio bloque **
      | otherwise            = a : goNormal (b:xs)
    goNormal [x] = [x]
    -- bloque *- ... -*, termina con la secuencia -*
    goBlockDash [] = []
    goBlockDash (x:xs) =
      case xs of
        ('*':rest) | x == '-' -> goNormal rest
        -- dentro del bloque se preservan saltos de línea y se descartan otros caracteres
        _ -> (if x=='\n' then '\n' else '\0') `cons` goBlockDash xs
    -- bloque ** ... **, termina con la secuencia **
    goBlockStar [] = []
    goBlockStar (x:xs) =
      case xs of
        ('*':rest) | x == '*' -> goNormal rest
        -- dentro del bloque se preservan saltos de línea y se descartan otros caracteres
        _ -> (if x=='\n' then '\n' else '\0') `cons` goBlockStar xs
    -- agrega carácter si no es nulo, usado para descartar contenido del bloque
    cons '\0' acc = acc
    cons c    acc = c:acc

-- constructores y atajos para expresiones regulares básicas
-- construye término para un carácter
char :: Char -> Expresion
char = Term

-- alternativa entre una lista de caracteres, error si lista vacía
oneOf :: [Char] -> Expresion
oneOf []     = error "oneOf: vacío"
oneOf [c]    = char c
oneOf (c:cs) = Or (char c) (oneOf cs)

-- concatena una cadena como expresión regular
str :: String -> Expresion
str []     = error "str: vacío"
str [c]    = char c
str (c:cs) = Concat (char c) (str cs)

-- rango de caracteres inclusivo
range :: Char -> Char -> Expresion
range a b = oneOf [a..b]

-- definiciones de clases de caracteres
letter, digit, nonzero :: Expresion
letter  = Or (range 'a' 'z') (range 'A' 'Z')
digit   = range '0' '9'
nonzero = range '1' '9'

-- espacios en blanco y repetición
wsChar :: Expresion
wsChar = oneOf " \t\r\n"

ws :: Expresion
ws = Concat wsChar (Star wsChar)

-- expresiones por nombre según imp.md
-- palabras reservadas
res_if, res_then, res_else, res_while, res_do, res_for, res_skip :: Expresion
res_if     = str "if"
res_then   = str "then"
res_else   = str "else"
res_while  = str "while"
res_do     = str "do"
res_for    = str "for"
res_skip   = str "skip"

-- booleanos y operadores booleanos
boolExpr, opBoolExpr :: Expresion
boolExpr   = Or (str "true") (str "false")
opBoolExpr = Or (str "not") (Or (str "and") (str "or"))

-- operadores de asignación, relacionales y aritméticos
asignExpr, opRelExpr, opAritExpr :: Expresion
asignExpr  = Concat (char ':') (char '=')
opRelExpr  = Or (char '<') (Or (char '>') (char '='))
opAritExpr = Or (char '+') (Or (char '-') (Or (char '*') (char '/')))

-- delimitadores y puntuación
puntuacionExpr, delimExpr :: Expresion
puntuacionExpr = char ';'
delimExpr      = Or (char '{') (Or (char '}') (Or (char '(') (char ')')))

-- identificadores y números
identExpr, numExpr :: Expresion
-- identificador: letra + letras* + dígitos*
identExpr = Concat letter (Concat (Star letter) (Star digit))
-- número: 0 | [1-9][0-9]* | -[1-9][0-9]*
numExpr   = Or (char '0')
           (Or (Concat nonzero (Star digit))
               (Concat (char '-') (Concat nonzero (Star digit))))

-- mapeo de nombres de tokens a expresiones
lookupExprByName :: String -> Expresion
lookupExprByName name =
  case name of
    "reservCond"  -> Or res_if (Or res_then res_else)
    "reservCiclo" -> Or res_while (Or res_do res_for)
    "reservSkip"  -> res_skip
    "bool"        -> boolExpr
    "opBool"      -> opBoolExpr
    "asign"       -> asignExpr
    "opRel"       -> opRelExpr
    "opArit"      -> opAritExpr
    "puntuacion"  -> puntuacionExpr
    "delim"       -> delimExpr
    "num"         -> numExpr
    "ident"       -> identExpr
    other         -> error $ "Token desconocido en lookup: " ++ show other

-- extracción de nombres del lado izquierdo del '=' en imp.md
-- válido para letras, dígitos y guión bajo
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- trim a la izquierda
trimL :: String -> String
trimL [] = []
trimL (c:cs) | isSpace c = trimL cs
             | otherwise = c:cs

-- extrae nombres de tokens únicos desde el texto
extractTokenNames :: String -> [String]
extractTokenNames txt = uniq (go (lines txt))
  where
    -- recorre línea a línea
    go [] = []
    go (l:ls) =
      case parseName (trimL l) of
        Nothing   -> go ls
        Just name -> name : go ls
    -- intenta parsear nombre antes de '=' ignorando comentarios # y --
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
        -- valida que el lhs sea solo nombre y espacios
        valid lhs =
          let nm  = takeWhile isNameChar lhs
              rest= drop (length nm) lhs
          in if nm /= "" && all isSpace rest then Just nm else Nothing
    -- prefijo simple sin librería extra
    isPrefixOf pfx s = pfx == take (length pfx) s
    -- elimina duplicados preservando orden
    uniq []     = []
    uniq (x:xs) = x : uniq (filter (/= x) xs)

-- orden de prioridad por clases y construcción de la lista de tokens
-- orden para reservadas y booleanos
reservedOrder  :: [String]
reservedOrder  = ["reservCond","reservCiclo","reservSkip","bool","opBool"]

-- orden para operadores
operatorsOrder :: [String]
operatorsOrder = ["asign","opRel","opArit"]

-- orden para delimitadores
delimsOrder    :: [String]
delimsOrder    = ["puntuacion","delim"]

-- orden para genéricos
genericOrder   :: [String]
genericOrder   = ["num","ident"]

-- filtra nombres presentes en orden deseado
present :: [String] -> [String] -> [String]
present want have = [ n | n <- want, n `elem` have ]

-- construye la lista final de (nombre, expresión) con ws al inicio
buildTokensFromSpecs :: String -> [(String, Expresion)]
buildTokensFromSpecs specsText =
  ("WS", ws) :
  [ (name, lookupExprByName name) | name <- ordered ]
  where
    names   = extractTokenNames specsText
    -- concatenación de órdenes y luego cualquier nombre no clasificado
    ordered = concat
      [ present reservedOrder  names
      , present operatorsOrder names
      , present delimsOrder    names
      , present genericOrder   names
      , [ n | n <- names
            , n `notElem` (reservedOrder ++ operatorsOrder ++ delimsOrder ++ genericOrder) ]
      ]

-- lexer basado en expresiones regulares con avance máximo
-- devuelve longitudes de matches para una expresión sobre el inicio de s
matches :: Expresion -> String -> [Int]
matches (Term c) (x:_) | c == x = [1]
matches (Term _) _               = []
matches (Concat a b) s           =
  [ la + lb | la <- matches a s, let rest = drop la s, lb <- matches b rest ]
matches (Or a b) s               = dedup (matches a s ++ matches b s)
matches (Star a) s               = 0 : more a s
  where
    -- intenta repetir la subexpresión a mientras haga progreso
    more _ ""  = []
    more e str =
      let ps = filter (>0) (matches e str)
      in if null ps then [] else dedup (concat [ map (n+) (matches (Star e) (drop n str)) | n <- ps ])

-- elimina duplicados en una lista de enteros
dedup :: [Int] -> [Int]
dedup []     = []
dedup (x:xs) = x : dedup (filter (/= x) xs)

-- elige la mejor longitud de match para una pareja (nombre, expresión)
bestLen :: (String, Expresion) -> String -> Int
bestLen (_, re) s = case matches re s of [] -> 0; xs -> maxList xs
  where maxList (y:ys) = foldl (\m v -> if v>m then v else m) y ys

-- lexer que recorre la cadena y produce lista de (nombre, lexema)
lexWithRE :: [(String, Expresion)] -> String -> [(String,String)]
lexWithRE _    ""     = []
lexWithRE toks s@(c:cs) =
  let (bestName,bestL) = choose toks ("",0)
  in if bestL > 0
        then (bestName, take bestL s) : lexWithRE toks (drop bestL s)
        else ("UNKNOWN",[c])          : lexWithRE toks cs
  where
    -- elige el token con mayor avance en la posición actual
    choose [] acc = acc
    choose ((nm,re):ts) (bn,bl) =
      let l = bestLen (nm,re) s
      in if l > bl then choose ts (nm,l) else choose ts (bn,bl)

-- programa de ejemplo simple para pruebas
ejemploIMP :: String
ejemploIMP = unlines
  [ "if x > 0 then"
  , "  y := y - 1;"
  , "else"
  , "  skip"
  ]
