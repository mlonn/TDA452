import Test.QuickCheck
-- | 1.(*) The Maximum Function
-- maxi x y returns the maximum of x and y
maxi :: Ord a => a -> a -> a
maxi x y | x >= y    = x
         | otherwise = y 
prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = maxi x y == y || maxi x y == x


-- | 2. Sum of squares.
-- sumsq n returns 1*1 + 2*2 + ... + n*n
sumsq :: Integral a => a -> a
sumsq n | n == 0 = 0
        | otherwise = n*n + sumsq (n-1)

sumsq' :: Integral a => a -> a
sumsq' n | n == 1 = 1
         | otherwise = div (n*(n + 1) * (2 * n + 1)) 6
                 
        
prop_sumsq :: Integral a => a -> Property
prop_sumsq n = n>=0 ==> sumsq n == sumsq' n
 
-- | 3. (*) The Towers of Hanoi
--
--hanoi n :
-- | 4. Fibonacci Numbers
fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)


-- | 5. Factors.
--smallestFactor :: Int -> Int


-- | 6. (*) Multiplying list elements
multiply :: Num a => [a] -> a
multiply []       =  1
multiply (x : xs) = x * multiply xs

prop_multiply :: Numm a => [a] -> Bool
prop_multiply xs = multiply xs == product xs
-- | 7. Avoiding Duplicates
-- duplicates :: Eq a => [a] -> Bool
-- removeDuplicates :: Eq a => [a] -> [a]
--prop_duplicatesRemoved :: [Integer] -> Bool
--prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

-- | 8. Testing

-- | 9. (*) Defining Types
-- daysInMonth :: Month -> Integer -> Integer
-- validDate :: Date -> Bool
-- tomorrow :: Date -> Date
