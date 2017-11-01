
import           Test.QuickCheck
----------------------------------------------------------------------------
    -- Code from assignment
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)
----------------------------------------------------------------------------

--Part 1
--  It uses k+1 steps

--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product $ replicate (fromInteger k) n

--Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
           | odd k = n * power2 n (k-1)

--Part 4

-- A.
-- for k: test 0, odd, even, large odd, large even
-- for n: same as k but with one odd negative and one even negative since k
--        does not support negative numbers
k = [0, 2, 5, 5789, 8962]
n = k ++ [-3, -8]

-- B.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (a == b) && (a == c)
  where a = power n k
        b = power1 n k
        c = power2 n k

-- C
runTest = printResult (length (filter (==True) result)) (length result)
    where result = [ prop_powers n k | n <- n, k <- k]

printResult ok all = putStrLn $ "Tests ok: " ++ show ok ++ "/" ++ show all

-- D.
prop_powers' n k = prop_powers n $ abs k
