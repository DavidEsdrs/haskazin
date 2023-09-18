doublen x = x * 2

doubleus x y = doublen x + doublen y

doublebign x = if x > 100 then x else doublen x

squared n = n * n

circleArea r = pi * squared r

squareArea s = squared s

isPrime n | n <= 1    = False
          | otherwise = all (\x -> n `mod` x /= 0) [2..sqrt_n]
          where sqrt_n = floor (sqrt (fromIntegral n))

primes = filter isPrime [2..]