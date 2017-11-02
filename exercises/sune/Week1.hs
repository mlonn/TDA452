import Test.QuickCheck

next :: Int -> [Int]
next 1 = [1]
next n | even n = n : next (div n 2)
       | odd n  = n : next (n * 3 + 1)

steps n = length $ next n
