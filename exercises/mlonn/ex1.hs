
pounds :: Double -> Double
pounds kr = kr/12.7775

fToC :: Double -> Double
fToC f = (f - 32)/1.8

price :: Double -> Double
price v | v <= 10   = 3.5*v
        | v <= 20   = 5+3*v
        | otherwise = 20+2.5*v

average :: Double -> Double -> Double
average x y = (x+y)/2

next :: Integer -> Integer
next n | even n    = div n 2
       | otherwise = 3*n+1

steps :: Integer -> Integer
steps n
    | n == 1    = 1
    | otherwise = steps(next n)+1

numbers :: Integer -> [Integer]
numbers n
        | n==1      = [1]
        | otherwise = n : numbers(next n)