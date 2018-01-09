-- 1
-- a
--xmas :: Int -> IO()


stars :: Int -> Int -> String
stars n max | n == 0 = concat (replicate max "*") ++ "\n"
            | otherwise = concat (replicate (n `div` 2) " ") ++ concat (replicate (max-n) "*") ++ "\n" ++ stars (n-1) max