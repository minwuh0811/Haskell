-- | Working with List 2
-- Examples to illustrate list comprehensions and recursive list functions
-- Introduction Functional Programming 2019.

import Prelude hiding (head,tail,last,init,reverse)
import Data.Char
import Test.QuickCheck

{-
This is just a skeleton, the definitions will be filled in
during the lecture.
-}

--------------------------------------------------------------------------------
-- * List comprehensions for operating on *all* elemenents in a list

-- | sumSquares n gives the sum of the squares of the numbers from 1 to n
--sumSquares :: Int -> Int
--sumSquares n =

-- | Change each character to upper case
--allCAPS :: String -> String
--allCAPS s = 

-- | takeWords n s returns the first n words of s
--takeWords :: Int -> String -> String
--takeWords n s =

-- | capitalize s capitalizes all words in s
--capitalize :: String -> String
--capitalize s =
--------------------------------------------------------------------------------
-- * List comprehensions for operating on *some* elements in a list (filtering)

-- | keepPositives as delete all negative numbers from as
--keepPositives :: [Int] -> [Int]
--keepPositives as =

-- | factors n lists the (positive) factors of n
--factors :: Int -> [Int]
--factors n =

-- | isPrime n checks is n is a (positive) prime number
--isPrime :: Int -> Bool
--isPrime n = 


--------------------------------------------------------------------------------
-- * List comprehensions with multiple list generators

-- | A chess board has 8*8 squares, columns A to H, rows 1 to 8.
--chessSquares :: [String]
--chessSquares = 

-- | The squares ordered by row
--chessSquares' :: [String]
--chessSquares' = 

-- | dices n returns the ways that a pair of dices can result in the sum n
--dices :: Int -> [(Int,Int)]
--dices n =

-- | check_max n checks that the max function is correct for all arguments
-- from 0 to n
--check_max :: Int -> Bool
--check_max n = 

-- Property: max x y returns the larger of x and y
prop_max x y = if x>y then max x y == x else max x y == y

-- | closestPoints ps gives the distance between the closest (non-equal)
-- points in the list ps
--closestPoints :: [(Double,Double)] -> Double
--closestPoints ps =

distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

points :: [(Double,Double)]
points = [(0.0,0.0), (0,4.0), (3.0,4.0)]

-- | translate a sentense to the "pirate language" (rövarspråket)
-- Example: toPirateLang "bar" == "bobaror"
--toPirateLang :: String -> String
--toPirateLang s =

-- | Finding pythagorean triangles,
-- e.g. integers a, b, c such that a^2 + b^2 = c^2
--pythagorean :: Int -> [(Int,Int,Int)]
--pythagorean n =

--------------------------------------------------------------------------------
-- * More functions on lists (including some recursive functions)

-- ** Working near the front of a list is quick and easy
-- | head [1,2,3] = 1

-- | tail [1,2,3] = [2,3]

-- ** Working near the back of a list require more work
-- | last [1,2,3] = 3

-- | init [1,2,3] = [1,2]

-- | append xs ys = xs++ys, example: append [1,2] [3,4] = [1,2,3,4]
--append :: [a] -> [a] -> [a]

{- Example
append [1,2] [3,4]
append (1 : 2 : []) [3,4]
...
-}

-- | reverse (same as last time)
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

{- Example
reverse [1,2,3]
reverse (1 : 2 : 3 : [])
reverse (2 : 3 : []) ++ [1]
(reverse (3 : []) ++ [2]) ++ [1]
((reverse [] ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
-- 1 step
([3] ++ [2]) ++ [1]
-- 2 steps
[3,2] ++ [1]
-- 3 steps
[3,2,1]
-}

-- | The number of steps to reverse a list of length n
steps_reverse n = n + sum [1..n]

steps_reverse' n = n + (n^2 + n) `div` 2

prop_steps_reverse (NonNegative n) = steps_reverse n == steps_reverse' n

-- | Faster reverse
--rev :: [a] -> [a]
--rev xs =

-- | revOnto xs ys = reverse xs ++ ys
-- move elements from the front of one list to the front of another list
--revOnto :: [a] -> [a] -> [a]

{- Example: 
rev [6,7,8]
...
-}

--prop_rev :: [Int] -> Bool
--prop_rev xs =
