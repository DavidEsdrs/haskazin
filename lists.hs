double :: Int -> Int
double n = n * 2

squared :: Int -> Int
squared n = n * n

doubleList :: [Int] -> Maybe [Int]
doubleList [] = Nothing
doubleList list = Just (map double list)

squareList :: [Int] -> Maybe [Int]
squareList [] = Nothing
squareList list = Just (map squared list)

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

isDivisor :: Int -> Int -> Bool
isDivisor x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = filter (isDivisor x) [1..x `div` 2]

isPerfect :: Int -> Bool
isPerfect x
    | sum (divisors x) == x = True
    | otherwise = False

perfectNumbers n = filter isPerfect [1..n]
