some n = [1..n]

double n = n * 2

squared n = n * n

doubleList list = 
    if length list == 0 then Nothing
    else Just (map double list)

squareList list =
    if length list == 0 then Nothing
    else Just (map squared list)

isRoot :: Int -> Int -> Bool
isRoot value target = squared value == target

find :: (Int -> Bool) -> [Int] -> Maybe Int
find _ [] = Nothing         -- se a lista for vazia, retorna nada
find f (x:xs)               -- sintaxe especial: (x:xs) é uma lista, x é a head e xs é o resto da lista
    | f x = Just x          -- se f x der True, retorna Just x
    | otherwise = find f xs -- senão, continua procurando no resto (xs) da lista

root :: Int -> Maybe Int
root n
    | n <= 1 = Nothing
    | otherwise = find (\x -> isRoot x n) [2..n `div` 2]