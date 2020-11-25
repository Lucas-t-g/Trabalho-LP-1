dif2 :: String -> String -> String
dif2 (x:xs) (y:ys)
    | length xs < 1 = if x == y then [x] else "_"
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
    | length xs < 1 = if x == y then "_" else [x]
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
    putStrLn "dif: recebe duas strings e as compara diretamente  elemento a elemento diz quais caracteres sao iguais ou nao"
    putStrLn "ndif: recebe duas strings e as compara diretamente  elemento a elemento diz quais caracteres sao iguais ou nao"
    putStrLn "Como chamar uma funcao: funcao <string1> <string2>\ncolocando ambas as strings entre aspas "