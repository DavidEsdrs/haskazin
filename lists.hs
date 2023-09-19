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
isPerfect x = sum (divisors x) == x

perfectNumbers n = filter isPerfect [1..n]

-- soma itens equivalentes em duas listas
sum' :: [Int] -> [Int] -> [Int]
sum' [] _ = []
sum' _ [] = []
sum' (h1:t1) (h2:t2) = h1 + h2 : sum' t1 t2

prod :: [Int] -> Int
prod l = foldl (\acc x -> acc * x) 1 l

-- fatorial
fac :: Int -> Int
fac n = prod [2..n]

-- os primeiros fatoriais - uso: take 10 ffac <--- retorna o fatorial dos 10 primeiros números
ffac :: [Int]
ffac = map fac [1..]

equals :: Char -> Char -> Bool
equals a b = a == b

center :: String -> String
center str
    | length str > 2 = init (tail str)
    | otherwise = ""

evaluate :: String -> [Bool]
evaluate "" = []
evaluate str = equals h l : evaluate c
    where
        h = head str
        l = last str
        c = center str

isPalindrome :: String -> Bool
isPalindrome str = all (==True) (evaluate str)