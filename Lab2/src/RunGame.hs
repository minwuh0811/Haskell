module RunGame where

import Data.Char
import Test.QuickCheck
import Cards
import Data.Maybe(fromJust)

-- The interface to the students' implementation.

data Interface = Interface
  { iFullDeck :: Deck
  , iValue    :: Hand -> Int
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Deck -> Hand -> (Deck, Hand)
  , iPlayBank :: Deck -> Hand
  , iShuffle  :: [Double] -> Deck -> Deck
  }

-- A type of players.

data Player = Guest | Bank
              deriving (Show, Eq)

-- Runs a game given an implementation of the interface.

runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  Rand r <- generate arbitrary
  gameLoop i (iShuffle i r (iFullDeck i)) []

-- Play until the guest player is bust or chooses to stop.

gameLoop :: Interface -> Deck -> Hand -> IO ()
gameLoop i deck guest = do
  putStrLn $ "Your current score: " ++ displayHand i guest ++ "\n"
  if iGameOver i guest then do
    finish i deck guest
   else do
    putStr (   "Draw "
            ++ (if null guest then "a " else "another ")
            ++ "card? [y] ")
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (deck', guest') = iDraw i deck guest
      gameLoop i deck' guest'
     else
      finish i deck guest

-- Display the bank's final score and the winner.

finish :: Interface -> Deck -> Hand -> IO ()
finish i deck guest = do
  putStrLn $ "Your final score: " ++ displayHand i guest
  putStrLn $ "The bank's final score: " ++ displayHand i bank
  putStrLn $ "Winner: " ++ show (iWinner i guest bank)
  where
  bank = iPlayBank i deck

-- A helper function for displaying a hand

displayHand :: Interface -> Hand -> String
displayHand i hand = show (iValue i hand) ++ if null hand then "" else " with cards: " ++ iDisplay i hand
