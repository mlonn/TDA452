
-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing), 
                   (1, elements [Just n | n <- [1..9]]) 
                 ]

-- * C

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)


-- * C3

prop_sudoku :: Sudoku -> Bool
prop_sudoku = isSudoku

-----------------------------------------------------------------------------

type Block = [Maybe Int]

testBlockT = [Just 1 :: Maybe Int, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
testBlockF = [Just 1 :: Maybe Int, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]

-- * D1
-- | Checks that there are no duplicates in a block.
-- checks each element once
-- if its nothing we don't care, we simply check the rest
-- if not we check if it occures twice.
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) 
  | isNothing x = isOkayBlock xs
  | otherwise   = notElem x xs && isOkayBlock xs

-- * D2
-- | Makes a list with every block that should contain 1-9.
blocks :: Sudoku -> [Block]
blocks sudoku 
  | isSudoku sudoku = concat [rows', transpose rows', getBlock rows']
  | otherwise       = error "blocks: input not a Sudoku"
    where rows' = rows sudoku

-- | Split in 3 and traspose until we have all 9x9 in 9 3x3 instead.
-- When it is split in to a 3x3 concatinate all cells.
getBlock :: [Block] -> [Block]
getBlock [] = []
getBlock matrix 
  | is3x3 matrix = [concat matrix]
  | otherwise    = getBlock (transpose first3) ++ getBlock remaining
    where (first3, remaining) = splitAt 3 matrix

-- | Checks if its a 3x3 matrix by checking its length before and after
-- transponing it.
is3x3 :: [[a]] -> Bool
is3x3 matrix = all (==3) (map length [matrix, transpose matrix])

-- | check that there are a correct amount of blocks 
-- and that the blocks are the right size
prop_blocks :: Sudoku -> Bool
prop_blocks sudoku = length blocks' == 27 && 
                     all (\x -> 9 == length x) blocks'   
  where blocks' = blocks sudoku

-- * D3
-- | Checks that a Sudoku does not violate any rules.
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock $ blocks sudoku

-----------------------------------------------------------------------------

type Pos = (Int, Int)


-- * E1

{- blanks :: Sudoku -> [Pos]
blanks sudoku = concatMap getNothingCells rows'
  where rows' = zip [0..8] $ map (zip [0..8]) (rows sudoku)

getNothingCells :: (Int, [(Int, Maybe Int)]) -> [(Int, Int)]
getNothingCells (rowNr, row) = [(rowNr, x) | x <-  map fst $ filter (isNothing . snd) row]
 -}

blanks :: Sudoku -> [Pos]
blanks = getPosOfAll isNothing 

filled :: Sudoku -> [Pos]
filled = getPosOfAll isJust 

getPosOfAll :: (Maybe Int -> Bool) -> Sudoku -> [Pos]
getPosOfAll comp sudoku = concatMap parseIndexes indexed
    where indexed = zip [0..8] $ map (indexOf isNothing) (rows sudoku)

parseIndexes :: (Int, [Int]) -> [Pos]
parseIndexes (row, cols) = [(row, col) | col <- cols]

indexOf :: (Maybe Int -> Bool) -> [Maybe Int] -> [Int]
indexOf comp cols = map fst $ filter (comp . snd) cols'
  where cols' = zip [0..8] cols

-- * E2

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newEle) = concat [before, [newEle], drop 1 after]  
    where (before, after) = splitAt index list


prop_putAt :: Eq a => [a] -> (Int, a) -> Bool
prop_putAt [] _ = True
prop_putAt list (i, new) = new == (list !!= (i', new)) !! i'
  where i' = mod i $ length list

-- * E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (row, col) newEle = Sudoku $ newSudoku $ newRow newEle
    where rows' = rows sudoku
          newSudoku line = rows' !!= (row, line)
          newRow ele = (rows' !! row) !!= (col, ele)

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sudoku (row, col) new = new == updated !! row' !! col'
    where row'    = mod (abs row) 8
          col'    = mod (abs col) 8
          updated = rows $ update sudoku (row', col') new


-- * E4

candidates :: Sudoku -> Pos -> [Int]
candidates sudoku pos = map fst $ filter (isOkay . snd) sudokuList
  where sudokuList = [(int, update sudoku pos (Just int)) | int <- [1..9]]

-----------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve sudoku 
  | isFilled sudoku = Just sudoku
  | otherwise = case mapMaybe solve (filter isOkay sudokus') of
      [] -> Nothing
      (x:_) -> Just x
  where (blank:xs)  = blanks sudoku
        sudokus' = map (update sudoku blank . Just) (candidates sudoku blank)



-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve path = do 
    sudoku <- readSudoku path
    case solve sudoku of
      Nothing -> putStr "(No solution possible)" 
      (Just sudoku) -> printSudoku sudoku

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = isSolved sud1 && all (cellEq sud1 sud2) (filled sud1)

isSolved :: Sudoku -> Bool
isSolved sud = isOkay sud && isFilled sud

cellEq :: Sudoku -> Sudoku -> Pos -> Bool
cellEq sud1 sud2 pos = cell pos sud1' == cell pos sud2'
  where sud1' = rows sud1
        sud2' = rows sud2
        cell (x, y) a = a !! y !! x

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust solved ==> isSolutionOf (fromJust solved) sud
      where solved = solve sud

-----------------------------------------------------------------------------