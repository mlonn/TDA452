module Main where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List


main = quickCheck prop_SolveSound

-- | Representation of sudoku puzzlese (allows some junk)
newtype Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example1 :: Sudoku
example1 =
    Sudoku
      [ [j 3,j 6,j 5  ,j 5  ,j 7,j 1,j 2,j 5  ,j 5  ]
      , [j 5  ,j 5,j 5  ,j 5  ,j 5  ,j 5  ,j 1,j 8,j 5  ]
      , [j 5  ,j 5  ,j 9,j 2,j 5  ,j 4,j 7,j 5  ,j 5  ]
      , [j 5  ,j 5  ,j 5  ,j 5  ,j 1,j 3,j 5  ,j 2,j 8]
      , [j 4,j 5  ,j 5  ,j 5,j 5  ,j 2,j 5  ,j 5  ,j 9]
      , [j 2,j 7,j 5  ,j 4,j 6,j 5  ,j 5  ,j 5  ,j 5  ]
      , [j 5  ,j 5  ,j 5,j 3,j 5  ,j 8,j 9,j 5  ,j 5  ]
      , [j 5  ,j 8,j 3,j 5  ,j 5  ,j 5  ,j 5  ,j 6,j 5  ]
      , [j 5  ,j 5  ,j 7,j 6,j 9,j 5  ,j 5  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length (rows sudoku) == 9 && all checkRow (rows sudoku)

-- | Checkrow will check that the amount of rows are correct and then check 
-- that each row contains valid values and 9 of them
checkRow :: [Maybe Int] -> Bool
checkRow row = length row == 9 && all checkCell row

-- | Check cell will make sure that a cell contains valid data
checkCell :: Maybe Int -> Bool
checkCell (Just a) = a > 0 && a < 10
checkCell Nothing  = True

  -- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sudoku = all (all isJust) (rows sudoku)

-----------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen

printSudoku :: Sudoku -> IO ()
printSudoku sudoku =  putStr $ concRows $ map formatRow $ rows sudoku

-- | Will convert a sudoku row to a string in a readable formation  
formatRow :: [Maybe Int] -> String
formatRow [] = "|\n"
formatRow row = concat ["| ", concatMap toStr first3, formatRow rest]
  where (first3, rest) = splitAt 3 row

-- | Will parse a value in a sudoku to a string 
toStr :: Maybe Int -> String
toStr Nothing = ". "
toStr (Just num) = show num ++ " "

-- | will return a line with some modifications for a sudoku
hLine :: String
hLine = concat $ replicate 3 ("+" ++ replicate 7 '-') ++ ["+\n"]

-- | Will merge all rows in to a readable formatted string
concRows :: [String] -> String
concRows [] = hLine
concRows rows' = concat [ hLine, concat first3, concRows rest ]
  where (first3, rest) = splitAt 3 rows'

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
  content <- readFile path
  checkFormat $ Sudoku $ map (map parseChar) $ lines content
  
-- | Check that the sudoko is valid in a sense of values and size.
checkFormat :: Sudoku -> IO Sudoku
checkFormat sudoku 
  | isSudoku sudoku = return sudoku
  | otherwise       = error "Isn't a Sudoku"

-- | Will take string and turn in into a useable value in a Sudoku.
parseChar :: Char -> Maybe Int
parseChar '.' = Nothing
parseChar char 
    | isDigit char = Just (digitToInt char)
    | otherwise = error "parseChar: non vaild char in sudoku"

-----------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [ 
  (9, return Nothing), 
  (1, elements [Just n | n <- [1..9]])
                 ]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-----------------------------------------------------------------------------

-- * D1

type Block = [Maybe Int]

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

-- | Creates blocks for all rows columns and 3x3 grids
blocks :: Sudoku -> [Block]
blocks (Sudoku rows') = concat [rows' , transpose rows', makeBlocks rows' ]

-- | Will turn each 3x3 part of the sudoku to a block 
makeBlocks :: [Block] -> [Block]
makeBlocks sud 
    | any null sud = []
    | otherwise = merge (map (take 3) sud) ++ makeBlocks (map (drop 3) sud)

-- | Merges 3 blocks in to one recursivly.
merge :: [Block] -> [Block]
merge blocks 
    | not (null blocks) =  concat (take 3 blocks): merge (drop 3 blocks)
    | otherwise = []

---------------------------------------------------------------------------

-- | checks that a sudoku has a sudoku has 3*9 blocks (9 rows 9 coulumns 
--    and 3 3x3 blocks) and that each blocks is 9 long.
prop_Blocks :: Sudoku -> Bool
prop_Blocks sudoku = length (blocks sudoku) == 3*9 &&
                     all (\x -> length x == 9) (blocks sudoku)

-- * D3

-- | Checks that a sudoku dosent have any cells that goes against the rules.
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock $ blocks sudoku

-----------------------------------------------------------------------------

-- * E1
type Pos = (Int,Int)

-- | returns all blank positions is a sudoku
blanks :: Sudoku -> [Pos]
blanks = values isNothing

-- | returns all filled positions is a sudoku
filled :: Sudoku -> [Pos]
filled = values isJust

-- | return all positions that matches the function f for a given position 
values ::  (Maybe Int -> Bool) -> Sudoku -> [Pos]
values f sudoku = filter (\x -> f (sudoku !!? x)) pos
  where pos = [(x,y) | x <- [0..8], y <- [0..8]]

-- | Will return a value at a given position in a sudoku
(!!?) :: Sudoku -> Pos -> Maybe Int
(!!?) sudoku (x, y) = rows sudoku !! x !! y

-- * E2 

-- | Sets a value at a specific position in a row.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) list (i, value) = concat[take i list, [value], drop (i + 1) list]


prop_change_elem :: Eq a => [a] -> (Int, a) -> Property
prop_change_elem list (i, value) =  i >= 0 && i < length list - 1 ==> 
                                ((list !!= (i, value)) !! i) == value

-- * E4

-- | Sets a new value at a given position in a sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (x,y) value = Sudoku (rows' !!= (x, newRow))
  where rows' = rows sudoku
        newRow = (rows' !! x) !!= (y, value)
        
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sudoku (x,y) value =  updated !!? (x', y') == value
  where updated = update sudoku (x', y') value
        x' = mod x 8
        y' = mod y 8

-- * E5

-- | Finds all possible values for a position in a sudoku. 
candidates :: Sudoku -> Pos -> [Int]
candidates sud pos = map fromJust $ filter (isOkay . update sud pos) numbers
  where numbers = [Just x | x <- [1..9]]

prop_candidates :: Sudoku -> Property
prop_candidates sud = isOkay sud && isSudoku sud ==> isOkay updated
  where pos = head (blanks sud)
        candidate = head (candidates sud pos)
        updated = update sud pos (Just candidate)

-----------------------------------------------------------------------------

-- * F1

-- | Solves a given sudoku if possible.
solve :: Sudoku -> Maybe Sudoku
solve sudoku
  | isFilled sudoku = Just sudoku
  | otherwise = case mapMaybe solve sudokus' of
      [] -> Nothing
      (x:_) -> Just x
  where blank = findBestBlank sudoku
        sudokus' = map (update sudoku blank . Just) (candidates sudoku blank)

-- | Reads a sudoku from file and solves it if possible.
readAndSolve :: FilePath -> IO()
readAndSolve filepath = do
            sudoku <- readSudoku filepath
            case solve sudoku of
              Nothing -> putStrLn "(no sulotion)"
              Just sudoku -> printSudoku sudoku

-- | determines if a given solution is a valid solution of a another sudoku.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol org = isSolved && all (compareCell org sol) (filled org)
  where isSolved = isFilled sol && isOkay sol

-- | checks if a cell has the same value in 2 sudokus.
compareCell :: Sudoku -> Sudoku -> Pos -> Bool
compareCell sol org pos = (sol !!? pos) == (org !!? pos)

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isSudoku sud && isOkay sud && isJust solved ==> 
                      fromJust solved `isSolutionOf` sud
                        where solved = solve sud

-----------------------------------------------------------------------------
-- * X

-- | Finds a blank posisiton with the least possible candidates. 
findBestBlank :: Sudoku -> Pos
findBestBlank sud = minimumBy (leastCandidates sud) (blanks sud)

-- | Determines which of two cells in a sudoku has the 
-- least possible candidates.
leastCandidates :: Sudoku -> Pos -> Pos -> Ordering
leastCandidates sud pos1 pos2 = compare (nbrCandid pos1) (nbrCandid pos2)
    where nbrCandid pos = length (candidates sud pos)

prop_fbb :: Sudoku -> Bool
prop_fbb sud =  all (best <=) $ map (length . candidates sud) (blanks sud) 
              where best = length (candidates sud (findBestBlank sud))
    