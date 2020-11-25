-- diferença entre duas strings
-- exmplod e entrada: php python
-- exemplo de saida: php
--(independe da posição)
comp2 :: String -> String -> String
comp2 s1 s2 = [if elem x s2 then x else '_' | x <- s1]

comp3 :: String -> String -> String
comp3 s1 s2
    | let i = 0 in if i < length s1 && elem s1 !! i s2 then let s2 = remove (s1 !! i) s2 in else let i = i+1

remove :: Char -> String -> String
remove c s
    | not(elem c s) = s
    | head s == c = '_': (tail s)
    | otherwise = head s :(remove c (tail s))

ccheck :: String -> String
ccheck (x:s)
    | length s == 0 = if x == '_' then "nao e anagrama" else "e anagrama"
    | x == '_' = "nao e anagrama"
    | otherwise = ccheck s

comp :: String -> String -> String
comp s1 s2 = "destes caracteres: " ++ s1 ++ ", estes: " ++ comp2 s1 s2 ++ " estao presentes em: " ++ s2 ++ ", portanto " ++ ccheck (comp2 s1 s2)

ncomp2 :: String -> String -> String
ncomp2 s1 s2 = [if elem x s2 then '_' else x | x <- s1]

nccheck :: String -> String
nccheck (x:s)
    | length s == 0 = if x == '_' then "e anagrama" else "nao e anagrama"
    | x /= '_' = "e anagrama"
    | otherwise = nccheck s

ncomp :: String -> String -> String
ncomp s1 s2 = "destes caracteres: " ++ s1 ++ ", estes: " ++ ncomp2 s1 s2 ++ " nao estao presentes em: " ++ s2 ++ ", portanto " ++ nccheck (ncomp2 s1 s2)

-- diferença entre duas strings
-- exmplod e entrada: php python
-- exemplo de saida: p__
dif2 :: String -> String -> String
dif2 (x:xs) (y:ys)
    | length xs < 1 = ""
    | length ys < 1 = (if x == y then x else '_') : ['_' | a <- xs ]
    | (x == y) = x:(dif2 xs ys)
    | otherwise = '_':(dif2 xs ys)

dcheck :: String -> String -> String
dcheck s1 s2 = if s1 == s2 then "iguais" else "diferentes"

dif :: String -> String -> String
dif    s1 s2 = if length s1 > length s2
         then "as duas strings tem estes caracteres em comum: " ++ dif2 s1 s2 ++ ", portanto as strings sao: " ++ dcheck s1 (dif2 s1 s2)
         else "as duas strings tem estes caracteres em comum: " ++ dif2 s2 s1 ++ ", portanto as strings sao: " ++ dcheck s2 (dif2 s2 s1)

ndif2 :: String -> String -> String
ndif2 (x:xs) (y:ys)
    | length xs < 1 = ""
    | length ys < 1 = (if x == y then '_' else x): xs
    | (x == y) = '_':(ndif2 xs ys)
    | otherwise = x:(ndif2 xs ys)

ndcheck :: String -> String
ndcheck (x:s)
    | length s == 0 = if x == '_' then "iguais" else "diferentes"
    | x /= '_' = "diferentes"
    | otherwise = ndcheck s

ndif :: String -> String -> String
ndif    s1 s2 = if length s1 > length s2
         then "as duas strings tem estes caracteres diferentes: " ++ ndif2 s1 s2 ++ ", portanto as strings sao: " ++ ndcheck (ndif2 s1 s2)
         else "as duas strings tem estes caracteres diferentes: " ++ ndif2 s2 s1 ++ ", portanto as strings sao: " ++ ndcheck (ndif2 s2 s1)

main = do
    putStrLn "comp: recebe duas strings e verifica quais caracteres da primeira string estao presentes na segunda string"
    putStrLn "ncomp: recebe duas strings e verifica quais caracteres da primeira string nao estao presentes na segunda string"
    putStrLn "dif: recebe duas strings e as compara diretamente  elemento a elemento diz quais caracteres sao iguais ou nao"
    putStrLn "ndif: recebe duas strings e as compara diretamente  elemento a elemento diz quais caracteres sao iguais ou nao"