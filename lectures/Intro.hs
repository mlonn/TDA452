-- | Some small examples to introduce Haskell.
-- Functional Programming course 2017.
-- Thomas Hallgren

{-
This is just a skeleton, the function definitions will be filled in
during the first lecture.
-}

       
---------------------------------------------------------------------------------- * Currency conversion
import Test.QuickCheck

exchangeRate = 9.7145  -- SEK / EUR

toEUR sek = sek / exchangeRate

toSEK eur = eur * exchangeRate

prop_exchange s = toSEK (toEUR s) ~== s
  where
    x ~== y = abs(x-y) < 1e-10

-- automated random testing with QuickCheck



--------------------------------------------------------------------------------
-- * Definition by cases

absolute x | x>=0 = x
absolute x | x<0 = -x

absolute2 x | x>=0 = x
            | x<0 = -x

absolute3 x = if x>=0 then x else -x
-- local definitions

--------------------------------------------------------------------------------
-- * Definition by recursion
-- The power function n^k

power n k | k==0 = 1
          | k>0 = power n (k-1) * n

prop_power n k = power n k' == n^k'
    where k' = abs k

-- intersecting lines

--intersect

--------------------------------------------------------------------------------
-- * Tuples

--examplePair = 

--exampleTriple = 

--exampleFunction (b,n,s) = 

--------------------------------------------------------------------------------
-- * List

snacks = "Spam"

dinner = [snacks, "Fish", "Chips", snacks, snacks, "Pudding"]

--summary :: [String] -> String
--summary

-- | Computing the length of a list
--len

--last'

--------------------------------------------------------------------------------
 -- * List comprehensions

--ex1

--doubles xs

--ex2

--ex3


--pythag n =
