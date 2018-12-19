module Minesweeper where

import Data.Char
import System.Random
import Data.List

data Tile = Num Int | Flag | Empty | Bomb | Unknown
            deriving (Eq, Show)

newtype Field = Field {rows :: [[Tile]] }
                deriving (Eq, Show)

type Pos = (Int, Int)

extractRow :: Field -> [[Tile]]
extractRow (Field rows) = rows

createFieldOf :: Int -> Int -> Tile -> Field
createFieldOf xSize ySize tile = Field (replicate ySize (replicate xSize tile))


--getSpace ::

replaceTile :: Field -> Tile -> Pos -> Field
replaceTile field value (x, y) = Field (extractRow field !!= (x, a !!= (y, value)))
                                      where a = extractRow field !! x

  -- * E2
--Replaces position i with value y and returns a copy of the list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | i >= length xs || i < 0 = error "index out of bounds"
[] !!= (i,y) = []
xs !!= (i,y) = l1 ++ (y : tail l2)
              where (l1,l2) = (splitAt i xs)

--Takes 
createField :: Int -> Int -> StdGen -> Field
createField size nMines r= placeMines f bp
                                  where f = createFieldOf size size Empty
                                        bp = randoml 0 (size-1) nMines r


-- generate list of random numbers on which 

--randoml :: Int -> Int -> Int -> [Pos]
--randoml min max n = repliate 

-- Generates a list of unique position, Should be remade
randoml :: Int -> Int -> Int -> StdGen -> [(Int, Int)]
randoml min max 0 r = []
randoml min max n r | (max - min + 1) * (max - min + 1) < n = error "too many bombs"
randoml min max n r | a `elem` b = randoml min max n r2
                    | otherwise  = a : b
                        where (x, r2) = next r    -- only need the seed
                              a = generateRandomPair min max r
                              b = randoml min max (n-1) r2

prop_list_unique :: Int -> Int -> Int -> StdGen -> Bool
prop_list_unique min max n r = and [a `notElem` l| a <- l]
                                where l = randoml min max n r

prop_correct_length :: Int -> Int -> Int -> StdGen -> Bool
prop_correct_length min max n r = length (randoml min max n r) == n



generateRandomPair :: Int -> Int -> StdGen -> (Int, Int)
generateRandomPair min max r = (x, y)
                                where (x, r2) = randomR (min, max) r
                                      (y, r3) = randomR (min, max) r2

--propBombPositionsCorrect :: Property -> Int -> Int -> Int -> StdGen -> Bool 
--qpropBombPositionsCorrect 
--Places mines on specified locations. 
placeMines :: Field -> [(Int,Int)] -> Field
placeMines field [] = field
placeMines field (bp:bps) = placeMines (replaceTile field Bomb bp) bps

reveal :: Pos -> Field -> Tile
reveal (x, y) (Field field) | x > length field || y > length (field !! 0) || x < 0 || y < 0 = error "out of bounds"
                    | otherwise = (field !! x) !! y

numberBombsInProximity :: Pos -> Field -> Int
numberBombsInProximity (x, y) (Field field) = sum [if reveal (x + c, y + d) (Field field) == Bomb
                                          then 1 else 0 | c <- a, d <- b]
                                          where (a, b) = correctList (x, y) (length field) (length (field !! 0))


correctList :: Pos -> Int -> Int -> ([Int], [Int])
correctList (x, y) xl yl = (b, d)
                        where a = 0: [-1 | not (x == 0)]
                              b = a ++ [1 | not (x == xl - 1)]
                              c = 0: [-1 | not (y == 0)]
                              d = c ++ [1 | not (y == yl - 1)]


click :: Pos -> Field -> Field -> Field
click (x, y) (Field reference) shown | a == 0 && 
                                    where nb = numberBombsInProximity (x, y) (Field reference)
                                          (dx, dy) = correctList (x, y) (length reference) (length (reference !! 0))
                                          rev = reveal (x, y) reference
                                          


