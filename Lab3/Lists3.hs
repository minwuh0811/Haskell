-- | Working with List 3
-- Examples to illustrate more recursion patterns, lists of lists, etc
-- Introduction Functional Programming 2019.

import Prelude hiding (zip,unzip,take,drop)
import Data.List(transpose)
import Test.QuickCheck


{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}

--------------------------------------------------------------------------------
-- * A variant of Quicksort (illustrating a different recursion pattern)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x1:xs) = quickSort smaller ++ [x1] ++ quickSort larger
  where
     smaller = [ x | x<-xs, x<x1 ]
     larger  = [ x | x<-xs, x>=x1 ]



{- Example
   quickSort [3,5,2,4]
-}

-- ** Testing quickSort
prop_quickSort :: [Int] -> Bool
prop_quickSort xs = iSort xs == quickSort xs

-- ** Insertion sort (from last week)

-- | Insert a value in a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x<y       = x : y : ys
                | otherwise = y : insert x ys

-- sorting a list (insertion sort)
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)


-- ** Random numbers for testing

randomNumbers n = generate (vectorOf n (choose (1,n)))

--------------------------------------------------------------------------------
-- * zip and unzip

-- | Combining two lists into a list of pairs:
-- Example: zip [1,2,3] [10,20] = [(1,10),(2,20)]
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _      _      = []

--zip []     []     = []
--zip (x:xs) []     = []
--zip []     (y:ys) = []


-- | Unzipping a list of pairs into a pair of lists
-- Example: unzip [(1,10),(2,20)] = ([1,2],[10,20])
unzip :: [(a,b)] -> ([a],[b])
unzip xys = ([x|(x,y)<-xys], [y|(x,y)<-xys])

prop_unzip_zip :: [Int] -> [Int] -> Bool
prop_unzip_zip xs ys = unzip (zip xs ys) == (take n xs,take n ys)
   where n = min (length xs) (length ys)

prop_zip_unzip :: [(Int,Int)] -> Bool
prop_zip_unzip xys = zip xs ys == xys
   where (xs,ys) = unzip xys

--------------------------------------------------------------------------------
-- * take and drop

prop_take_drop :: NonNegative Int -> [Int] -> Bool
prop_take_drop (NonNegative n) xs = take n xs ++ drop n xs == xs

-- | Take the first n elements from a list
-- Example: take 2 "Haskell" == "Ha"
take :: Int -> [a] -> [a]
--take 0 xs = []
--take n [] = []
take n (x:xs) | n>0 = x : take (n-1) xs
take _ _            = []

-- | Drop the first n elements from a list
-- Example: drop 2 "Java" == "va"
drop :: Int -> [a] -> [a]
drop 0 xs           = xs
drop n []           = []
drop n (x:xs) | n>0 = drop (n-1) xs

--------------------------------------------------------------------------------
-- * Lists of lists, custom show functions

-- Example: data types for modelling naughts-and-crosses or tic-tac-toe

data Grid = Grid [[Mark]]          --deriving Show
data Mark = Blank | Naught | Cross deriving Eq   --Show

-- custom show functions
instance Show Mark where
  -- show :: Mark -> String
  show Blank  = " "
  show Naught = "O"
  show Cross  = "X"

instance Show Grid where
   -- show :: Grid -> String
   show (Grid rows) = unlines [showRow row | row<-rows]
     where
       showRow row = unwords [ show m | m<-row]
       
exampleGrid :: Grid
exampleGrid =  Grid [[Naught,Blank,Naught],
                     [Blank,Naught,Cross],
                     [Cross,Cross,Naught]]



-- | Did someone win= (i.e., fill a row, column or diagonal)
win :: Grid -> Bool
win grid = winningRow grid || winningColumn grid || winningDiagonal grid
-- TODO: win allows Blank to win, we need to fix that!

winningRow :: Grid -> Bool
winningRow (Grid rows) = or [ allSame row | row<-rows]

winningColumn :: Grid -> Bool
winningColumn grid = winningRow (transposeGrid grid)

transposeGrid :: Grid -> Grid
transposeGrid (Grid grid) = Grid (transpose grid)

winningDiagonal :: Grid -> Bool
winningDiagonal (Grid rows) =
   allSame (diagonal1 rows) || allSame (diagonal2 rows)

diagonal1 :: [[a]] -> [a]
diagonal1 ((m:row):rows) = m : diagonal1 [tail row | row<-rows]
diagonal1 _              = []

diagonal2 rows = diagonal1 (reverse rows)


allSame :: Eq a => [a] -> Bool
allSame (x1:xs) = and [ x == x1 | x<-xs ]

--rows :: Grid -> [[Mark]]
