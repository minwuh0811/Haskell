-- Introduction to Functional Programming 2019.
-- Some small examples to introduce Haskell.

{-
This started as a skeleton, the function definitions were filled in
during the first lecture.
-}

import Test.QuickCheck
-------------------------------------------------------------------------------
-- * Price calculation

-- Concepts: simple data, types, def by cases, testing

-- The price of prawns: 245kr/kg

unitPrice = 245.00   -- kr/kg
discount  = 50       -- discount if you buy more than 10kg

price kg = unitPrice*kg

-- two arguments:

average x y = (x+y)/2

--------------------------------------------------------------------------------
-- * Definition by cases

-- Example of definition by cases

absolute x | x<0   = -x
           | x>=0  = x

absolute' x = if x<0 then -x else x


price2 kg | kg<10  = price kg
          | kg>=10 = price 10 + discountPrice (kg-10) -- sensible version
                     --discountPrice kg               -- strange version
-- TODO: the same number 10 appears in many places, make a definition!

discountPrice kg = (unitPrice-discount)*kg

-- Property-based testing
-- The property we test is that if you buy more, then you pay more
prop_price2 kg extra = price2 (abs kg) <= price2 (abs kg + abs extra)

--------------------------------------------------------------------------------
-- * Definition by recursion

-- | Computing factorial of a number
-- 5! = 5 * 4 * 3 * 2 * 1

fac :: Integer -> Integer
fac n | n==0  = 1
      | n> 0  = fac(n-1) * n

{- fac 3
   fac (3-1) * 3
   fac 2 * 3
   fac (2-1) * 2 * 3
   fac 1 * 2 * 3
   fac (1-1) * 1 * 2 * 3
   fac 0 * 1 * 2 * 3
   1 * 1 * 2 * 3
   6
-}

-- | power2 k computes 2 to the power k
-- power2 4 = 2 * 2 * 2 * 2

power2 n | n==0  = 1
         | n> 0  = 2 * power2(n-1)
         

--------------------------------------------------------------------------------
-- * Lists, enumerations, list comprehensions

upto n = [1..n]

fac' n = product [1..n]

power2table n = [ power2 i | i<-[0..n] ]

sumsquares n = sum [ i*i | i<-[1..n] ]

factors n = [ f | f<-[1..n], isFactorOf f n ]

isFactorOf f n = n `mod` f == 0

primes n = [ p | p <- [1..n], factors p == [1,p] ]
