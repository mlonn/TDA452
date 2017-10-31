{-
Part 1
It uses k+1 steps
-}
import           Test.QuickCheck
-- power n^k
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product $ replicate (fromInteger k) n


-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
power2 n k | odd k = n * power2 n (k-1)

{-
Part 4
A.
Test Cases:
| n | 0 | 0 | 1 | 2 | 3 | 6 | 8 | 5  | 17 |
| k | 0 | 1 | 0 | 3 | 2 | 8 | 6 | 17 | 5  |
-}
-- B.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k' == power1 n k') == (power n k' == power2 n k')
  where
    k' = abs k

{-- C
nk is a list of values for n and k
-}
nk = [0, 1, 2, 100, 99, 1024]
test = [prop_powers n k | n <- nk, k <- nk]
-- D
-- prop_powers worked the first time.
