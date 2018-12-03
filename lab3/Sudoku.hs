module Sudoku where

import Data.Char
import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.List
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
    deriving (Eq, Show)
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- A3
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- A2

isSudoku :: Sudoku -> Bool
isSudoku sudoku = hasRightSize (extractRow sudoku) && hasCorrectNumbers (extractRow sudoku)

hasRightSize :: [[Maybe Int]] -> Bool
hasRightSize rows = length rows == 9 && and [length row == 9 | row <- rows]

extractRow :: Sudoku -> [[Maybe Int]]
extractRow (Sudoku rows) = rows

hasCorrectNumbers :: [[Maybe Int]] -> Bool
hasCorrectNumbers rows = and [ and [num == Nothing 
            || (num >= Just 1 && num <= Just 9) | num <- row ] | row <- rows]

-- A3
isFilled :: Sudoku -> Bool
isFilled sudoku = and [ and [num >= Just 1 && num <= Just 9
                | num <- row ] | row <- rows]
                where rows = extractRow sudoku

-- B1

printSudoku :: Sudoku -> IO()
printSudoku sudoku = putStr (concat [ concat [toString num | num <- row]
             ++ "\n" | row <- rows])
            where rows = extractRow sudoku

toString :: Maybe Int -> String
toString (Just num) = show num
toString Nothing = "."

-- B2

readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
                     numbers <- readFile path 
                     return (Sudoku (map ( map convertToMaybeInt) (lines numbers)))


convertToMaybeInt :: Char -> Maybe Int
convertToMaybeInt '.' = Nothing
convertToMaybeInt char | isDigit char = Just (digitToInt char) 
convertToMaybeInt char | otherwise = error "not a valid number" 

-- C1

cell :: Gen (Maybe Int)
cell = elements ([Just i | i <-[1..9]] ++ replicate 81 Nothing)

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
    arbitrary =
      do rows <- vectorOf 9 (vectorOf 9 cell)
         return (Sudoku rows)
  
  -- * C3
  
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku 
  
  ------------------------------------------------------------------------------
  
type Block = [Maybe Int]
  
  
  -- * D1
  
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = not (elem x xs) && (isOkayBlock xs)
  
  
  -- * D2
  
blocks :: Sudoku -> [Block]
blocks b= getRows b ++ getColumns b ++ squareBlock (transpose (extractRow b))

getRows :: Sudoku -> [Block]
getRows = extractRow

getColumns :: Sudoku -> [Block]
getColumns rows = transpose (extractRow rows)

squareBlock :: [Block] -> [Block]
squareBlock [] = []
squareBlock sudoku = squareBlock'(take 3 sudoku) ++ squareBlock (drop 3 sudoku)

-- takes three blocks from each col and adds them together, then 
squareBlock' :: [Block] -> [Block]
squareBlock' (row1 : row2 : row3 : em)
  | null row1 = []
  | otherwise = (take 3 row1 ++ take 3 row2 ++ take 3 row3) :
        squareBlock' (drop 3 row1 : drop 3 row2 : drop 3 row3 : em)
  
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = hasCorrectnBlocks e 
                            && and [hasCorrectCells a | a <- e]
                            where e = extractRow sudoku

hasCorrectnBlocks :: [Block] -> Bool
hasCorrectnBlocks blocks = length blocks == 9

hasCorrectCells :: Block -> Bool
hasCorrectCells block = length block == 9

-- * D3

isOkay :: Sudoku -> Bool
isOkay b = all isOkayBlock (blocks b)
  
  
  ---- Part A ends here --------------------------------------------------------
  ------------------------------------------------------------------------------
  ---- Part B starts here ------------------------------------------------------
  
  
  -- | Positions are pairs (row,column),
  -- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)
  
  -- * E1
  
blanks :: Sudoku -> [Pos]
blanks (Sudoku sudoku) = [(0,a) | a <- sudoku !! 0] 
  
  --prop_blanks_allBlanks :: ...
  --prop_blanks_allBlanks =
  
  
  -- * E2
  
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined
  
    
    