module Main where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

main = quickCheck prop_Blocks

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

checkRow :: [Maybe Int] -> Bool
checkRow row = length row == 9 && all checkCell row

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

formatRow :: [Maybe Int] -> String
formatRow [] = "|\n"
formatRow row = concat ["| ", concatMap toStr first3, formatRow rest]
  where (first3, rest) = splitAt 3 row

toStr :: Maybe Int -> String
toStr Nothing = ". "
toStr (Just num) = show num ++ " "

hLine :: String
hLine = concat $ replicate 3 ("+" ++ replicate 7 '-') ++ ["+\n"]

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
  
checkFormat :: Sudoku -> IO Sudoku
checkFormat sudoku 
  | isSudoku sudoku = return sudoku
  | otherwise       = error "Isn't a Sudoku"

parseChar :: Char -> Maybe Int
parseChar '.' = Nothing
parseChar char 
    | isDigit char = Just (digitToInt char)
    | otherwise = Just 0

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
        [(9, return Nothing),
        (1, do n <- choose(1,9)
               return (Just n))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
-------------------------------------------------------------------------

-- * D1

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block = isDuplicate block []

isDuplicate :: Block -> Block -> Bool
isDuplicate [] found = True
isDuplicate (x:xs) found | elem x found = False
                         | x == Nothing = isDuplicate xs found
                         | otherwise    = isDuplicate xs (x:found)

-- * D2

blocks :: Sudoku -> [Block]
blocks sudoku = concat [rows sudoku, 
                        transpose $ rows sudoku,
                        makeBlocks sudoku]

makeBlocks :: Sudoku -> [Block]
makeBlocks sudoku = concat[merge(map (take 3) (rows sudoku)),
                           merge(map (take 3) (map (drop 3) (rows sudoku))),
                           merge(map (drop 6) (rows sudoku))]

merge :: [Block] -> [Block]
merge blocks | length blocks > 0 =  concat (take 3 (blocks)):
                                          merge (drop 3 blocks)
merge blocks | otherwise = []
                 
prop_Blocks :: Sudoku -> Bool
prop_Blocks sudoku = length (blocks sudoku) == 3*9 &&
                      all (\x -> length x == 9) (blocks sudoku)

-- * D3
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock $ blocks sudoku

-- * E1
type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sudoku = filter (isBlank sudoku) pos
  where pos = [(x,y) | x <- [0..8], y <- [0..8]]

isBlank :: Sudoku -> Pos -> Bool
isBlank sudoku pos = (((rows sudoku) !! (fst pos)) !! (snd pos)) == Nothing

-- * E2 
-----------------------------------------------------------------------------
