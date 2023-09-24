
isEven n = n `mod` 2 == 0

evenn l = sum (filter isEven l)