{-# LANGUAGE PatternSynonyms, ViewPatterns, GeneralizedNewtypeDeriving #-}

-- Version 2018-09-10
-- Changes:
-- * quickCheck now works with infinite lists of random numbers.
-- * Record syntax nolonger used
-- * Deck is just a type synonym and not a new type

module Cards where

import Test.QuickCheck

-- A card has a rank and belongs to a suit.

data Card = Card Rank Suit
            deriving (Eq, Show)

-- extract the rank and the suit of a card:
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

-- All the different suits.

data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

-- A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.

data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

-- A hand of cards and a deck of cards are both just a lists of Cards:
type Hand = [Card]
type Deck = [Card]

-- The size of a hand.
-- Note that we could have used the function length.
size :: Num a => Hand -> a
size []           = 0
size (card:hand)  = 1 + size hand


---------------------------------------------------
-- The rest of this file is for quickCheck.
-- You are not required to understand it (yet).

instance Arbitrary Suit where
  arbitrary = elements [ Hearts, Spades, Diamonds, Clubs ]

instance Arbitrary Card where
  arbitrary = do
    suit <- arbitrary
    rank <- arbitrary
    return (Card rank suit)
instance Arbitrary Rank where
  arbitrary = frequency [ (4, elements [Jack,Queen,King,Ace])
                        , (9, do n <- choose (2, 10)
                                 return (Numeric n))
                        ]
                        

-- A generator of infinite lists of numbers from 0.0 to 1.0
-- Needed for assignment F

-- From the student perspective the type is
-- data Rand = Rand [Double]

-- The reality (to make quickcheck behave with infinite lists) is more complicated.
-- Thanks to Nick Smallbone for the code.
-- You will not be expected to understand this at any point...
--------------------

newtype ZeroOne = ZeroOne {getZeroOne :: Double}
instance Show ZeroOne where show = show . getZeroOne
instance Arbitrary ZeroOne
  where arbitrary = ZeroOne `fmap` choose (0.0,1.0)

newtype Rand = MkRand {getRand :: InfiniteList ZeroOne}
  deriving Arbitrary
instance Show Rand where show = show . getRand

pattern Rand xs <- (map getZeroOne . getInfiniteList . getRand -> xs)
