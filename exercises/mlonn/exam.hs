
a :: a -> (a,a)
a x = (x,x)

b :: (Ord a, Num a) => a -> a -> Bool
b x y = x < y+1

c :: (y -> z -> x) -> (y,z) -> x
c x (y,z) = x y z


fa :: Eq a => [a] -> a -> [b] -> Maybe b  
fa l m = m `lookup` zip l

fb :: [(a -> a)] -> a -> a
fb [] a = a
fb (b:c) a = fb c (b a)

fc :: Eq a => [a] -> [[a]] -> Bool
fc (a:b) (c:d) = b /= c
fc _ e         = null e

-- (a −> b) −> [a] −> [b]
--fb' :: [a->b] -> [[a] -> [b]]
fb' [] = []
fb' (b:c) = (map b) : fb' c

fc' :: Eq a => [a] -> [a] -> Bool
fc' (a:b) (c:d) = a /= c && fc' b d
fc' _ _ = True