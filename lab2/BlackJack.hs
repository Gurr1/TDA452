module BlackJack where
import Cards
import RunGame

-- A0 
-- size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)) =
-- 1 + size (Add (Card Jack Spades) Empty) =
-- 1 + 1 + size (Empty)     = 1 + 1 + 0    = 2

--A1
-- returns the empty hand.
empty :: Hand  
empty = Empty

--A2
-- calculates the final value of the hand
-- incase the hand has gone bust and contains Aces, Aces are counted as ones
value :: Hand -> Integer        
value hand | initialValue hand > 21 = 
    initialValue hand - ((numberOfAces hand) * 10)
value hand = initialValue hand

-- Calulates the initial hand value, before any Ace values are subtracted
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card rank suite) hand) =
     valueRank rank + initialValue hand

-- Calculates how many aces there are in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card rank suite) hand) | 
        valueRank rank == 11 = 1 + numberOfAces hand
numberOfAces (Add (Card rank suite) hand) = numberOfAces hand

-- calculates how much a rank is worth in points.
valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank Ace         = 11
valueRank _           = 10

-- calculates how much a Card is worth in points.
valueCard :: Card -> Integer
valueCard (Card rank suite) = valueRank rank


--A3
--Finds if a hand has gone bust. 
gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
gameOver hand                   = False


--A4
-- returns who has the highest hand.
-- first argument is the guest and second is bank. 
-- if bank has higher hand and is not bust, bank wins
-- if guest has higher and is not bust, guest wins.
-- the last case only covers if guest has higher and is bust. Then Guest wins
winner :: Hand -> Hand -> Player
winner guest bank | value guest <= value bank && not (gameOver bank) = Bank
winner guest bank | not (gameOver guest) = Guest
winner guest bank= Bank

hand19 = Add (Card (Numeric 8) Spades) 
        (Add (Card Ace Hearts) Empty)

handBust = Add (Card (Numeric 10) Spades) 
        (Add (Card King Hearts)
        (Add (Card (Numeric 4) Spades) Empty))

hand15 = Add (Card (Numeric 8) Spades) 
        (Add (Card (Numeric 7) Hearts) Empty)


-- Part 2B1

-- 
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2 = hand2
(<+) hand1 Empty = hand1
(<+) hand1 (Add card Empty) = Add card hand1
(<+) (Add card hand1) hand2 = (hand1 <+ hand2) <+ (Add card Empty)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = 
        p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1) + (size hand2) == size (hand1 <+ hand2)

-- Part 2B2

fullDeck :: Hand
fullDeck = addAllCards allCards Empty

addAllCards :: [Card] -> Hand -> Hand
addAllCards [] hand = hand
addAllCards (x:xs) hand = addAllCards xs (Add x hand)

allCards :: [Card]
allCards = [Card rank suite | rank <- [Numeric 1, Numeric 2, Numeric 3, Numeric 4, Numeric 5,
         Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10,
         Jack, Queen, King, Ace], suite <- [Hearts, Spades, Diamonds, Clubs]]

-- Part 2B3