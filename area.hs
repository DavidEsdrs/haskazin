squared :: Float -> Float
squared n = n * n

squareArea :: Float -> Float
squareArea f = squared f

circleArea :: Float -> Float
circleArea r = pi * squared r

triangleArea :: Float -> Float -> Float
triangleArea b h = b * h / 2