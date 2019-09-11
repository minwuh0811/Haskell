-- | Modelling Playing Cards
-- Examples to introduce data types in Haskell
-- Introduction to Functional Programming 2019.

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}

-- | Every card has a suit:  ♠ ♥ ♦ ♣
data Suit = Spades | Hearts | Diamonds | Clubs   deriving (Eq,Show)


data Colour = Red | Black  deriving Show

-- | Each suit has a colour – red or black
colour :: Suit -> Colour
{-
colour Hearts   = Red
colour Diamonds = Red
colour Spades   = Black
colour Clubs    = Black
-}
colour Hearts   = Red
colour Diamonds = Red
colour _        = Black   -- wildcard, don't care pattern



-- | Cards have ranks: 2, 3 .. 10, Jack, Queen, King, Ace
data Rank = Numeric Int | Jack | Queen | King | Ace  deriving (Eq,Ord,Show)

all_ranks :: [Rank]
all_ranks = [Numeric n | n<-[2..10]] ++ [Jack,Queen,King,Ace]

-- Note: the type Rank also allows some values that are not proper ranks
-- like Numeric 30 and Numeric (-5)


-- | When does one rank beat another rank?
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1>r2

-- | A card has a rank and a suit
data Card = Card Rank Suit  deriving (Eq,Show)

rank :: Card -> Rank
rank (Card rank suit) = rank

suit :: Card -> Suit
suit (Card rank suit) = suit

example_card_1 = Card King Spades
example_card_2 = Card (Numeric 5) Hearts

-- | A card beats another card when it has the same suit and it beats the rank
-- of the other card
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2

-- | Alternative definition
cardBeats' card1 card2 =
     suit card1 == suit card2 && rankBeats (rank card1) (rank card2)


-- | A hand is a list of cards
type Hand = [Card]

example_hand_0 = []
example_hand_1 = [example_card_1]
example_hand_2 = [example_card_1,example_card_2]

-- | A hand can beat another card if one of the cards in the hand can beat
-- the other cards
handBeats :: Hand -> Card -> Bool
handBeats hand card = betterCards hand card /= []

-- | Return the cards that beat the given card.
betterCards :: Hand -> Card -> [Card]
betterCards hand card = [ c | c<-hand, cardBeats c card ]

-- End of lecture 1B (2019) ----------------------------------------------------
--------------------------------------------------------------------------------
-- Start of lecture 2A (2019) --------------------------------------------------

-- | Given a card to beat and a hand, choose a card from the hand that can
-- beat the card to beat, if possible.
-- Choose the lowest card that beats the card to beat
-- If you can follow suit, choose the lowest card of the same suit
-- Otherwise, choose the lowest card

--chooseCard :: Card -> Hand -> Card






-- | Find (one of) the lowest card in a hand
--lowestCard :: Hand -> Card

-- | Return a hand containing only the cards of the given suit
--selectSuit :: Suit -> Hand -> Hand

-- | Does the hand contain a card of the given suit?
--haveSuit :: Suit -> Hand -> Bool
