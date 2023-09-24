splitInHalf l = 
    splitAt h l 
    where 
        h = length l `div` 2


merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge l [] = l
merge [] l = l
merge l1 l2
    | h1 < h2 = h1:merge t1 l2
    | h1 > h2 = h2:merge l1 t2
    | otherwise = h1:h2:merge t1 t2
    where
        h1 = head l1
        t1 = tail l1
        h2 = head l2
        t2 = tail l2


mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort l = merge (mergeSort h1) (mergeSort h2) 
    where 
        (h1, h2) = splitInHalf l