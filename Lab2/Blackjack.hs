module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

--Task A1
hand2 :: Hand
hand2 = Card (Numeric 2) Hearts : (Card Jack Spades : [])

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades:[]) -- add the remaining steps here
            , 1 + 1 + size [] -- add the remaining steps here
            , 1 + 1 +0 -- add the remaining steps here
            , 2
            ]
-- define my favorite card here
aCard1 :: Card
aCard1 = Card Ace Hearts
-- define another card here
aCard2 :: Card
aCard2 = Card (Numeric 10) Diamonds

aHand :: Hand
aHand = [aCard1 , aCard2, aCard1] -- a Hand with two Cards, aCard1 and aCard2

-- Task 2A
displayCard :: Card -> String
displayCard c=show(rank(c)) ++  " of " ++ show(suit(c)) 
--display hands by list comprehension
display :: Hand -> String
display h=unlines([displayCard c| c<-h])
--display hands by recursion
display1:: Hand ->String
display1 [] = ""
display1 (h:hand)=unlines[displayCard h] ++ display1 hand 

-- Task 3A
--calculates the value of a Rank
valueRank :: Rank -> Int
valueRank (Numeric x) = x
valueRank Ace = 11
valueRank _ = 10
--calculates the value of a Card
valueCard :: Card -> Int
valueCard c= valueRank (rank (c))
--the number of aces in a given hand by list comprehension
numberOfAces :: Hand -> Int
numberOfAces h = size [c|c<-h,rank(c)==Ace] 
--the number of aces in a given hand by recursion
numberofAces1:: Hand ->Int
numberofAces1 []=0
numberofAces1 (h:hand)
    |rank(h)==Ace = 1 + numberofAces1 hand
    |otherwise = 0 + numberofAces1 hand
--calculates the value of the hand by list comprehension
sumValueCards :: Hand -> Int
sumValueCards h = sum [valueCard c|c<-h]
--calculates the value of the hand by recursion
sumValueCards1 ::Hand->Int
sumValueCards1 [] = 0
sumValueCards1 (h:hand) =valueCard h + sumValueCards1 hand
--calculates the actual value of the hand if the value is larger than 21 then Ace represent 1 otherwise Ace represent 11
value :: Hand -> Int
value h 
    | sumValueCards h > 21  = (sumValueCards1 h - ((numberOfAces h)*10))
    | otherwise = sumValueCards1 h

--Task 4A
gameOver :: Hand -> Bool
--a function that, given a hand, checks if the player is bust. If larger than 21 is bust otherwise is not.
gameOver h
    |value h > 21 =False
    |otherwise =True
--a function that, given one hand for the guest and one for the bank (in that order), checks which player has won.
winner :: Hand -> Hand -> Player
winner guest bank 
    |gameOver guest && value guest > value bank =Guest
    |gameOver guest && value bank > 21 =Guest
    |otherwise =Bank

--Test Cases
--Test Case1 if guest is not bust and larger than bank, guest win (passed)
gHand1 :: Hand
gHand1 = [aCard1 , aCard2]
bHand1 :: Hand
bHand1 = [aCard1 , aCard2, aCard1]
--Test Case2 if guest equal to bank, bank win(passed) 
gHand2 :: Hand
gHand2 = [aCard1 , aCard2, aCard1]
bHand2 :: Hand
bHand2 = [aCard1 , aCard2, aCard1]
--Test Case3 if guest is bust, bank win(passed) 
gHand3 :: Hand
gHand3 = [aCard2 , aCard2, aCard2]
bHand3 :: Hand
bHand3 = [aCard1 , aCard2, aCard1]
--Test Case4 if guest and bank both bust, bank win(passed)
gHand4 :: Hand
gHand4 = [aCard2 , aCard2, aCard2]
bHand4 :: Hand
bHand4 = [aCard2 , aCard2, aCard1, aCard1]
--Test Case5 if guest is not bust but bank is bust, guess win(passed)
gHand5 :: Hand
gHand5 = [aCard2 , aCard2]
bHand5 :: Hand
bHand5 = [aCard2 , aCard2, aCard1, aCard1]
