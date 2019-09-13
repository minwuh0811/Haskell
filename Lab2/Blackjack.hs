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
displayCard (Card (Numeric n) suit)=show(n) ++  " of " ++ show(suit) 
displayCard c =show(rank(c)) ++  " of " ++ show(suit(c)) 
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
    |value h > 21 =True
    |otherwise =False
--a function that, given one hand for the guest and one for the bank (in that order), checks which player has won.
winner :: Hand -> Hand -> Player
winner guest bank 
    |not (gameOver guest) && value guest > value bank =Guest
    |not (gameOver guest) && gameOver bank =Guest
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

--Task B1
--a list of all possible ranks
fullRank::[Rank]
fullRank = [Jack,Queen,King,Ace] ++ [Numeric n| n <- [2..10]]                   
--a list of all possible suit
fullSuit::[Suit]
fullSuit=[Hearts, Spades, Diamonds, Clubs ]
--a list of all possible Deck
fullDeck :: Deck
fullDeck = [Card r s | s<-fullSuit, r<-fullRank]
--test
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

--Task B2
draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand=error "draw: The deck is empty."
draw (d:deck) hand = (deck,d:hand)

--Task B3
playBank' :: Deck -> Hand -> Hand
playBank' _ bankHand 
    | (value bankHand) >= 16 = bankHand
playBank' deck bankHand = playBank' deck' bankHand' 
        where (deck', bankHand') = draw deck bankHand

playBank :: Deck -> Hand
playBank deck = playBank' deck []

--Task B4
--random Number return between 0 to 52
doubleToInt :: Double -> Deck -> Int
doubleToInt rand deck = floor(rand*(size deck)) 
--remove n-th card from deck
deckRemove :: Int -> Deck -> Deck
-- deckRemove 0 deck = tail deck
-- deckRemove n deck = head deck ++ (deckRemove (n-1) tail deck)
deckRemove n deck = take (n) deck ++ drop (n+1) deck
--help function for the shuffle
takeCard :: Int -> Deck -> Card
--takeCard n deck = (deck!!n, deckRemove n deck)
takeCard n deck=deck!!n
-- shuffle function to shuffle the card         
shuffle :: [Double] -> Deck -> Deck
shuffle [] deck = deck
shuffle _ [] = []
shuffle (rand:randlist) deck = (shuffle randlist remainingDeck) ++ [pickedCard]
    where n = doubleToInt rand deck
          pickedCard = takeCard n deck
          remainingDeck = deckRemove n deck
--takeCard (doubleToInt rand deck) deck ++ (shuffle randlist (deckRemove (doubleToInt rand deck) deck))

--Task B5
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = size (shuffle randomlist deck)==52

--Task B6
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
