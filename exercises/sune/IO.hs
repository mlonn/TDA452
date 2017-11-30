{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.List (insert)
import Test.QuickCheck hiding (listOf)
import Data.Maybe
import System.Directory

main :: IO ()
main = test

test :: IO ()
test = do
    putStrLn "Enter a positive int:"
    n :: Int <- readLn
    s <- getInt n
    putStr "= "
    print s

getInt :: Int -> IO Int
getInt 0 = return 0
getInt n = do
    putStr "+ "
    v :: Int <- readLn
    prev <- getInt (n-1)
    return $ v + prev


readUntill0 :: IO ()
readUntill0 = do
    v::Int <- readLn
    list <- readInt v
    print list

readInt :: Int -> IO [Int] 
readInt 0 = return [0]
readInt n = do
    v::Int <- readLn
    list <- readInt v
    return $ insert n list


repeat' :: IO Bool -> IO () -> IO ()
repeat' test op = do
    t <- test
    if t 
        then putStrLn "Bra jobbat"
        else repeat' test op

test' :: IO Bool
test' = do 
    ans <- getLine
    return $ ans == "y" 

op :: IO ()
op = putStrLn "Say y"

guessNr :: IO Bool
guessNr = do
    putStrLn "What number am i thinking about?"
    guess :: Int  <- readLn
    if guess == 7 then return True else return False


prop_LookNothing :: Eq a => a -> [(a, b)] -> Property
prop_LookNothing a l = notElem a (map fst l) ==> 
    isNothing $ lookup a l

prop_LookJust :: Eq a => a -> [(a, b)] -> Property
prop_LookJust a l = elem a (map fst l) ==> 
    isJust $ lookup a l

prop_Look :: Eq a => a -> [(a, b)] -> Property
prop_Look a l = if any (\x -> a == fst x) l
    then prop_LookJust a l
    else prop_LookNothing a l

-- Define a property prop_LookNothing that expresses that if the look function delivers Nothing, then the thing we were looking for was not in the table.
-- Also define a property prop_LookJust that expresses that if the look function delivers a result Just y, then the pair (x,y) should have been in the table.

-- Also write a property prop_Look that combines prop_LookNothing and prop_Just into one property.


sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (mx:mxs) = do
    x <- mx
    xs <- sequence' mxs
    return (x : xs)
    

mapM :: Monad m => (a->m b) -> [a] -> m [b]
mapM f l = sequence' (map f l)

onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf b m = if b then m else return ()

-- sequence takes a list of instructions resulting in a value of type a, and creates one big instruction that executes all of these, gathering all results into one result list. Example: the instructions
-- sequence [ readFile file | file <- files ]
-- mapM readFile files
-- both reads the contents of all files in the list files, and produces the contents of each of the files.
-- onlyIf takes a boolean and an instruction, and creates an instruction that only executes the argument instruction if the boolean was True. If the boolean was False, nothing happens. For example,

-- onlyIf failed tryAgain
-- executes the instructions tryAgain only if the boolean failed is True.

game :: IO()
game = do
    putStrLn "Think of a random nr 1 - 99"
    st <- askPlayer 50 2
    putStrLn st 
    
askPlayer :: Int -> Int -> IO String
askPlayer v 10 = return "You done fucked up"
askPlayer v g = do
    putStr $ concat ["Is it ", show v, "? - "]
    a <- getLine 
    case a of 
        "h" -> askPlayer (v + diff) (g+1)
        "l" -> askPlayer (v - diff) (g+1)
        "y" -> return "Yaaaaay"
    where diff = if 2^g < 100 then div 100 (2^g) else 1

untilAns :: (a -> Bool) -> IO a -> IO a
untilAns valid ask = do
    v <- ask
    if valid v then return v 
               else do
                putStrLn "Anwer properly"
                untilAns valid ask


{-
creates a new directory called "backup",
copies all the files in the current directory into the backup directory.

createDirectory :: FilePath -> IO ()
doesDirectoryExist :: FilePath -> IO Bool

-}

prompt :: String -> IO String
prompt a = do 
    putStrLn a
    getLine

mkBackup :: IO ()
mkBackup = do 
    dirN <- prompt "What folder?"
    exists <- doesDirectoryExist dirN
    if exists then backup dirN
              else mkBackup

backup :: FilePath -> IO()
backup p = do 
    contents <- getDirectoryContents p
    createDirectoryIfMissing False "backup"
    cpAll (drop 2 contents) p
    putStrLn "Done, copied stuff to backup"
    
cpAll :: [FilePath] -> FilePath -> IO ()
cpAll [] _ = return ()
cpAll (f:fs) p = do 
    cpAll fs p
    copyFile (concat [p,"/",f]) ("backup/"++f)



listOf :: Int -> Gen a -> Gen [a]
listOf 0 _ = return []
listOf n gv = do
    x <- gv
    xs <- listOf (n-1) gv
    return (x:xs)

--  B. Now use listOf to write a generator that generates pairs of lists of the same, random, length.

pairGen :: Gen a -> Gen ([a],[a])
pairGen a = do
    n' <- arbitrary
    let n = abs n'
    n1 <- listOf n a
    n2 <- listOf n a
    return (n1, n2)

ordered :: Ord a => [a] -> Bool
ordered (x:y:xs) = x <= y && ordered (y:xs)
ordered _ = True

orderedRand :: Gen [Integer]
orderedRand = do xs <- arbitrary
                 let ds = map abs xs
                 return (tail (scanl (+) 0 ds))

prop_orderedList = forAll orderedRand ordered