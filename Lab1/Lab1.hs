{- Lab 1
   Authors:Min Wu, Narin Mohamad, Noor Alweli, Andrea Bertl
   Lab group:8
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = (k+1)

-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k
   | k < 0 = error "power: negative argument"
   | k==0  = 1
   | k > 0 = product [n|i<-[1..k]]

power1a :: Integer -> Integer -> Integer   
power1a n k
   | k < 0 = error "power: negative argument"
   | k==0  = 1
   | k > 0 = product (replicate (fromInteger k) n)

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k 
   | even k  = power2 (n*n) (k `div` 2)
   | odd k = n * power2 (n) (k-1) 

   -- D -------------------------
{- 
Test case 1 (n=1,4,18 and k=0)
k is 0 results should be 1
testCase [1,4,18] [0]
[1,1,1]

Test case 2 (n=1,4,18 and k= 1, 3, 5)
k larger than 0 and k is odd number
testCase [1,4,18] [1,3,5]
[True,True,True,True,True,True,True,True,True]

Test case 3 (n=1,4,18 and k= 2, 4, 6) 
k larger than 0 and k is even number
testCase [1,4,18] [2,4,6]
[True,True,True,True,True,True,True,True,True]
Test Case 4 (n=1,4,8 and k=-1)
k<0 return error
 -}
 
-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k= power n k == power2 n k

-- Test functions: 
comparePower3 :: Integer -> Integer -> Bool
comparePower3 n k= comparePower2 n k == comparePower1 n k

testCase::[Integer]->[Integer]->[Integer]
testCase xn xk = [power n k|n<-xn,k<-xk,comparePower3 n k]

--Extra part F
space=10
string::[String]
string=["n"++concat (replicate 9 " ")++"power"++concat (replicate 5 " ")++
   "power1"++concat (replicate 4 " ")++"power2"++concat (replicate 4 " ")]
printOut::Integer->Integer->String
printOut n k= show(k)++concat(replicate(10-length(show(k))) " ")++
   show(power n k) ++concat (replicate(10-length(show(power n k))) " ")++
   show(power1 n k)++concat (replicate(10-length(show(power1 n k))) " ")++
   show(power2 n k)++concat (replicate(10-length(show(power2 n k))) " ")
table::Integer->Integer->IO()
table n k = putStr(unlines (string ++ [printOut n ks|ks<-[0..k]]))

