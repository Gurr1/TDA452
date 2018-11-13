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
value hand | otherwise = initialValue hand

-- Calulates the initial hand value, before any Ace values are subtracted
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card rank suite) hand) =
     valueRank rank + initialValue hand

-- Calculates how many aces there are in a hand
numberOfAces :: Hand -> Integer
numberOfAces (Add (Card rank suite) Empty) = 0
numberOfAces (Add (Card rank suite) hand) | 
        valueRank rank == 11 = 1 + numberOfAces hand
numberOfAces (Add (Card rank suite) hand) | 
        otherwise            = numberOfAces hand

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
              | otherwise  = False


--A4
-- returns who has the highest hand.
-- first argument is the guest and second is bank. 
-- if bank has higher hand and is not bust, bank wins
-- if guest has higher and is not bust, guest wins.
-- the last case only covers if guest has higher and is bust. Then Guest wins
winner :: Hand -> Hand -> Player
winner guest bank | value guest <= value bank && not (gameOver bank) = Bank
winner guest bank | not (gameOver guest) = Guest
winner guest bank | otherwise = Bank

hand_19 = Add (Card (Numeric 8) Spades) 
        (Add (Card Ace Hearts) Empty)

hand_bust = Add (Card (Numeric 10) Spades) 
        (Add (Card King Hearts)
        (Add (Card (Numeric 4) Spades) Empty))

hand_15 = Add (Card (Numeric 8) Spades) 
        (Add (Card (Numeric 7) Hearts) Empty)