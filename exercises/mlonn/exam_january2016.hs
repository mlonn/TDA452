import  Test.QuickCheck
import Data.Char
import Data.List
-- 1
-- a
xmas :: Int -> IO()
xmas n = putStr $ stars n n

stars :: Int -> Int -> String
stars n max | n == 0 = str max " *" ++ "\n"
            | otherwise = str n " " ++ str (max-n) " *" ++ "\n" ++ stars (n-1) max
        where str n = concat . replicate n 

-- b

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = [[]]
splitWhen f xs = start : case end of 
    [] -> []
    c -> splitWhen f (drop 1 end)
    where (start, end) = span (not . f) xs

prop_splitWhen0 =
    splitWhen (== ';') "A;BB;;DDDD;" == ["A","BB","","DDDD",""]
    && splitWhen (>1) [3,0,1,2,0,0] == [[],[0,1],[0,0]]
    && splitWhen (>1) [] == [[]]

prop_splitWhen :: (a -> Bool) -> [a] -> Bool
prop_splitWhen p xs = length (splitWhen p xs) ==  length (filter p xs) + 1

-- 2
--a 
data Sudoku = Sudoku [[Int]]

ex = Sudoku [[3,6,0,0,7,1,2,0,0],[0,5,0,0,0,0,1,8,0],[0,0,9,2,0,4,7,0,0],[0,0,0,0,1,3,0,2,8],[4,0,0,5,0,2,0,0,9],[2,7,0,4,6,0,0,0,0],[0,0,5,3,0,8,9,0,0],[0,8,3,0,0,0,0,6,0],[0,0,7,6,9,0,0,4,3]]

--showSudoku :: Sudoku -> String
showSudoku (Sudoku s) = rows
    where rows = map makeRows s
          makeRows r = map (intToDigit) r