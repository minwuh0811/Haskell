-- | Working with List 1
-- Examples to illustrate pattern matching, recursion and testing for lists.
-- Introduction Functional Programming 2019.

import Prelude hiding (null,length,sum,reverse)
import Test.QuickCheck

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}

--------------------------------------------------------------------------------
-- * Introducing pattern matching on lists

sumOfThree [x,y,z] = x+y+z

sumOfAtMostThree [] = 0
sumOfAtMostThree [x] = x
sumOfAtMostThree [x,y] = x+y
sumOfAtMostThree [x,y,z] = x+y+z

null :: [a] -> Bool
null []    = True
null (_:_) = False

third :: [a] -> a
third (_:_:x:_) = x

--------------------------------------------------------------------------------
-- * A reminder about recursion

-- fac n = n! = 1 * 2 * 3 * … * n

fac :: Integer -> Integer {-
fac n | n==0  = 1
      | n> 0  = fac(n-1) * n -}

-- with pattern matching on numbers
fac 0         = 1
fac n | n> 0  = fac(n-1) * n

--------------------------------------------------------------------------------
-- * Recursive functions on lists

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs


sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

{- Example
sum [6,7,8]
sum (6 : (7 : (8 : [])))
6 + sum (7 : (8 : []))
6 + 7 + sum (8 : [])
6 + 7 + 8 + sum []
6 + 7 + 8 + 0
21
-}

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
--reverse [6,7,8] = [8,7] ++ [6]
 
{- reverse [6,7,8] == [8,7,6]
   reverse [7,8] = [8,7]
-}
--------------------------------------------------------------------------------
-- * Sorting

-- | Insert a value in a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x<y       = x : y : ys
                | otherwise = y : insert x ys

-- sorting a list (insertion sort)
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

--------------------------------------------------------------------------------
-- * Testing properties of list functions


--prop_length

--prop_sum

--prop_reverse

--nonprop_reverse

--prop_insert_1

--prop_insert_2
