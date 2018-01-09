import Test.QuickCheck
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq,Show)
t = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 10)) -- an example tree

-- 1
sumTree :: Num a => Tree a -> a -- Example: sumTree t == 16
sumTree = foldTree (+)

minTree :: Ord a => Tree a -> a -- Example: minTree t == 1
minTree = foldTree min

foldTree :: (a->a->a) -> Tree a -> a
foldTree f (Leaf a) = a
foldTree f (Branch l r) = f (foldTree f l) (foldTree f r)

-- 2

l = [1,2,3,4,5,6,7,8,9]
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n l = splitHelper n l 1 []

splitHelper :: Int -> [a] -> Int -> [a] -> ([a],[a])
splitHelper n (x:xs) i l  | n == i = (l++[x], xs)
                          | otherwise = splitHelper n xs (i+1) (l++[x])

splitAt'' :: Int -> [a] -> ([a],[a])
splitAt'' n xs | n<=0 = ([],xs)
splitAt'' _ [] = ([],[])
splitAt'' n (x:xs) = (x:ys,zs) where (ys,zs) = splitAt'' (n-1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf i [] = []
chunksOf i l = take i l : chunksOf i (drop i l)

prop_chunk :: Int -> [a] -> Bool
prop_chunk _ [] = True
prop_chunk i xs = all (\x -> length x == i') chunks
    where 
        i' = abs i
        c = chunksOf i' xs
        chunks = take (length c - 1) c

fa :: a -> [a]        
fa x = [x,x]

fb :: Num a => Bool -> a -> a
fb x y = if x then y else -y

fc :: (a -> b , c -> d) -> (a , c) -> (b , d)
fc (f,g) (x,y) = (f x,g y)

printFramed :: String -> IO()
printFramed s = putStr (framed s)

framed :: String -> String
framed s = replicate (length s + 2) '*' ++ "\n*" ++ s  ++ "*\n" ++ replicate (length s + 2) '*' ++ "\n"

type TagName = String -- A few lowercase letters ’a’..’z’
data XML = Text String -- Arbitrary text
            | Elem TagName [XML] -- A tagged element <t>...</t>
deriving (Eq,Show)

elem = [["A","B"],["C","D"]]
tableToXML :: [[String]] -> XML








        