-- | Modelling a Playing Cards
-- Examples to introduce data types in Haskell
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This is just a skeleton, the definitions will be filled in
during the lecture.
-}

-- | Every card has a suit:  ♠ ♥ ♦ ♣
data Suit = Spades | Hearts | Diamonds | Clubs
        deriving (Show,Eq)


data Colour = Black | Red
         deriving Show

-- | Each suit has a colour – red or black
colour :: Suit -> Colour
colour Spades = Black 
colour Clubs = Black
colour c = Red



-- | Cards have ranks: 2, 3 .. 10, Jack, Queen, King, Ace
data Rank = Numeric Int | Jack | Queen | King | Ace
           deriving (Show,Eq,Ord) 

all_ranks = [Numeric n|n<-[2..10]] ++ [Jack,Queen,King,Ace]

-- | When does one rank beat another rank?
--rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1>r2


-- | Alternatives to the  Rank type?

--data Rank' = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | Jack | Queen | King | Ace
--          deriving Show
--all_ranks'

--data Card = Card Rank Suit
--        deriving Show

-- | A card has a rank and a suit
data Card = Card {rank::Rank, suit::Suit}
         deriving Show 


-- | With field names
--data Card

--rank :: Card -> Rank
--rank (Card r s) = r

--suit :: Card -> Suit
--suit (Card r s) = s


example_card_1 = Card Ace Hearts
example_card_2 = Card { rank=King, suit=Spades } 
-- | A card beats another card when it has the same suit and it beats the rank
-- of the other card
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2

-- | Alternative definition
cardBeats' card1 card2 = suit card1 == suit card2
                       && rankBeats (rank card1) (rank card2)



--type Hand = [Card]

-- | A hand contains zero or more cards
data Hand = Empty | Add Card Hand 
        deriving Show

example_hand_0 = Empty

example_hand_1 = Add example_card_1 example_hand_0

example_hand_2 = Add example_card_2 example_hand_1

-- | A empty cand beats nothing. A non-empty hand can beat a card if the first
-- card can, or if the rest of the hand can
handBeats :: Hand -> Card -> Bool
handBeats Empty           beat = False
handBeats (Add card hand) beat = cardBeats card beat || handBeats hand beat

-- | Return the cards that beat the given card.
betterCards :: Hand -> Card -> Hand
betterCards Empty beat = Empty
betterCards (Add card hand) beat 
                | cardBeats card beat = Add card (betterCards hand beat)
                | otherwise = betterCards hand beat 


-- | Given a card to beat and a hand, choose a card from the hand that can
-- beat the card to beat, if possible.
--    Choose the lowest card that beats the card to beat
-- If you can follow suit,
--    choose the lowest card of the same suit
-- Otherwise, choose the lowest card

chooseCard :: Card -> Hand -> Card
chooseCard beat hand 
        | handBeats hand beat       = lowestCard (betterCards hand beat)
        | haveSuit hand (suit beat) = lowestCard (sameSuit hand (suit beat))
        | otherwise                 = lowestCard hand









-- | Return a hand containing only the cards of the given suit
sameSuit :: Hand -> Suit -> Hand
sameSuit hand suit = betterCards hand (Card suit (Numeric 1)) -- Hacky As Fuck

-- | Does the hand contain a card of the given suit?
haveSuit :: Hand -> Suit -> Bool
haveSuit Empty s = False
haveSuit (Add card hand) s = suit card == s || haveSuit hand s


-- | Find (one of) the lowest card in a hand
lowestCard :: Hand -> Card
lowestCard (Add card Empty) = card
lowestCard (Add card hand) | rank card < rank low = card
                           | otherwise            = low
        where low = lowestCard hand
