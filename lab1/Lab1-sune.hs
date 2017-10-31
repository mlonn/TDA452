import Test.QuickCheck

--    Part 1
-- It should take k-1 steps for the algorithm.


--    Part 2
power1 :: Integer -> Integer -> Integer
power1 n k = product $ replicate (fromInteger k) n


--    Part 3
power2 :: Integer -> Integer -> Integer
power2  n 0 = 1
power2  n k | even k = power2 (n*n) (div k 2)
            | odd k = n * power2 n (k-1)

--    Part 4

--    A
--  n   k
--  20  0  test with k = 0
--  0   7  test with n = 0
--  5   8  test with both positive
--  -3  4  test with n negative
--  We skip negative k since we do not intend for that to work.

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


-- B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (a == b) && (a == c)
  where a = power n k
        b = power1 n k
        c = power2 n k

-- C
runTest = prop_powers 20 0 && prop_powers 5 8 && prop_powers (-3) 4 && prop_powers 0 7

-- D
prop_powers' n k = prop_powers n $ abs k
