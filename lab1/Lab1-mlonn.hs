{-
Part 1
It uses k+1 steps
-}
import Test.QuickCheck
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


--Part 4
--A.
k = [0, 2, 5, 5789, 8962] --test 0, odd, even, large odd, large even
n = k ++ [-3, -7] -- add negative for n
-- B.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k' == power1 n k') == (power n k' == power2 n k')
  where
    k' = abs k

-- C
test = [prop_powers n k | n <- n, k <- k]
-- D
-- prop_powers worked the first time.