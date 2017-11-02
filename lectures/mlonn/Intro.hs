-- | Some small examples to introduce Haskell.
-- Functional Programming course 2017.
-- Thomas Hallgren

{-
This started as a skeleton, the function definitions were filled in
during the first lecture.
-}

import Test.QuickCheck

--------------------------------------------------------------------------------
-- * Currency conversion

exchangeRate = 9.7145  -- SEK / EUR

toEUR sek = sek / exchangeRate

toSEK eur = eur * exchangeRate



prop_exchange eur = toEUR (toSEK eur) ~== eur
    where
    x ~== y = abs(x-y) < 1e-10

-- automated random testing with QuickCheck



--------------------------------------------------------------------------------
-- * Definition by cases

--absolute x | x>=0 = x
--absolute x | x<0  = -x

absolute x | x>=0 = x
    | x<0  = -x


absolute2 x = if x>=0 then x else -x

-- local definitions

--------------------------------------------------------------------------------
-- * Definition by recursion
-- The power function n^k

--power n k | k==0 =  1
--          | k>0  = power n (k-1) * n

--power n k | k<0  = 1 / power n (-k)

power :: Integer -> Integer -> Integer
power n 0        =  1
power n k | k>=0 = power n (k-1) * n

prop_power n k = power n k' == n ^ k'
    where k' = abs k


-- intersecting lines

intersect 0         = 0
intersect n | n>0   = intersect (n-1) + n-1

--------------------------------------------------------------------------------
-- * Tuples

examplePair = (4,True)

exampleTriple = (4,False,"Hello")

exampleFunction (b,n,s) = if b then s else show n

--------------------------------------------------------------------------------
-- * List

snacks = "Spam"

dinner = [snacks, "Fish", "Chips", snacks, snacks, "Pudding"]

summary :: [String] -> String
summary []    = "Nothing"
summary [x]   = "Only "++x
summary [x,y] = x++" and "++y
summary (x:xs) = x++" followed by other things, finally "++last xs

-- | Computing the length of a list
--len

--last'

--------------------------------------------------------------------------------
-- * List comprehensions

ex1 = [ x*x | x <- [1..10] ]

doubles xs = [ 2*x | x<-xs]

ex2 = [[x,y] | x <- "ABC", y<-"1234"]

--ex3


pythag= [ (a,b,c) | a<-[1..100], b<-[a..100], c<-[b..100], a^2+b^2==c^2]