module BlackJack where
import Cards
import RunGame
import System.Random

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

--adds the left hand to the right hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2 = hand2
(<+) hand1 Empty = hand1
(<+) (Add card Empty) hand1 = Add card hand1
(<+) (Add card hand1) hand2 =  (Add card Empty) <+ (hand1 <+ hand2)

--confirms that the property "ontop" is mathematically associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = 
        p1<+(p2<+p3) == (p1<+p2)<+p3


-- this compares the size of hand (a)+(b) and (a+b)        
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1) + (size hand2) == size (hand1 <+ hand2)

-- Part 2B2


--creates a deck that contains all the cards with addAllCards&allCards
fullDeck :: Hand
fullDeck = addAllCards allCards Empty


-- adds a list of cards to the hand
addAllCards :: [Card] -> Hand -> Hand
addAllCards [] hand = hand
addAllCards (card:cards) hand = addAllCards cards (Add card hand)


--creates a list of cards of all combinations of ranks&suits 
allCards :: [Card]
allCards = [Card rank suite | rank <- [Numeric 2, Numeric 3, Numeric 4, Numeric 5,
         Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10,
         Jack, Queen, King, Ace], suite <- [Hearts, Spades, Diamonds, Clubs]]

-- Part 2B3


--draws a card from the deck to the hand 
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "cannot draw card from an empty deck"
draw (Add card deck) hand = (deck, (Add card hand))

-- Part 2B4

--deck = shuffle fullDeck


--creates a hand for the bank
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

--adds cards to the bank's hand untill it has 16 or higher score.

playBank' :: Hand -> Hand -> Hand
playBank' deck hand | value hand < 16 = playBank' deck' hand'
                    | otherwise = hand
        where (deck', hand') = draw deck hand

-- Part 2B5


--shuffles a hand by recusivly calling the shuffle' function. 

shuffle :: StdGen -> Hand -> Hand
shuffle rand cards = shuffledHand 
        where (originalHand, shuffledHand) = shuffle' rand (cards, Empty)


shuffle' :: StdGen -> (Hand,Hand) -> (Hand, Hand)
shuffle' rand (Empty, shuffled) = (Empty, shuffled)
shuffle' rand (original, shuffled)   = shuffle' rand2 (original', shuffled')
        where (original', shuffled') = (removeNthCard original n, (Add (getNthCard original n) shuffled))
              (n, rand2)             = randomR (0, (size original) - 1) rand

--removes the nth card from a hand by creating a new hand without the nth card
removeNthCard :: Hand -> Integer -> Hand
removeNthCard Empty number = Empty
removeNthCard (Add card hand) 0 = hand 
removeNthCard (Add card hand) number = Add card (removeNthCard hand (number-1))


--copies the nth card in a hand
getNthCard :: Hand -> Integer -> Card
getNthCard hand number | number >= size hand = error "not in deck"
getNthCard (Add card hand) 0 = card
getNthCard (Add card hand) number = getNthCard hand (number - 1)


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
       c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle rand hand = value (shuffle rand hand) == value hand

main :: IO()
main = runGame implementation

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }