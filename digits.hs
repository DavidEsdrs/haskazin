import Data.Char (digitToInt)

stringToNumberList :: String -> [Int]
stringToNumberList str = map digitToInt str

sumList :: [Int] -> Int
sumList l = foldl (\acc x -> acc + x) 0 l

digitsSum :: Int -> Int
digitsSum n = sumList nums 
    where nums = stringToNumberList (show n)