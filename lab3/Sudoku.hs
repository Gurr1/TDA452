module Sudoku where

import Data.Char
import Test.QuickCheck
import Data.List
import Data.Maybe

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

  
  --TODO 









































































































  -- * D2
  
-- Checks if the given block is valid according to sudoku rules
  
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = not (elem x xs) && (isOkayBlock xs)
  
  -- Returns the rows, columns and blocks given a sudoku
blocks :: Sudoku -> [Block]
blocks b = getRows b ++ getColumns b ++ squareBlock (transpose (extractRow b))

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

-- Checks if all blocks in a sudoku are valid
isOkay:: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- Property to validate the amount of blocks and their respective sizes
prop_blocks :: Sudoku -> Bool
prop_blocks s = length blocks' == 27 && all (\b -> length b == 9) blocks'
    where blocks' = blocks s
  

hasCorrectnBlocks :: [Block] -> Bool
hasCorrectnBlocks blocks = length blocks == 9

hasCorrectCells :: Block -> Bool
hasCorrectCells block = length block == 9

-- * D3

  
  
  ---- Part A ends here --------------------------------------------------------
  ------------------------------------------------------------------------------
  ---- Part B starts here ------------------------------------------------------
  
  
  -- | Positions are pairs (row,column),
  -- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)
  
  -- * E1
  
--blanks :: Sudoku -> [Pos]
--blanks (Sudoku sudoku) = [(0,a) | a <- sudoku !! 0] 
blanks :: Sudoku -> [Pos]
--generates a pos pair for each row with the cordinate, then each tile in the row if the cell is nothing
blanks s = [(y,x) | (y, row) <- zip [0..] (rows s), 
                    (x, cell) <- zip [0..] row, 
                    isNothing cell]

hasValue :: Sudoku -> Pos -> Maybe Int
hasValue sudoku (y, x) = ((extractRow sudoku) !! y) !! x

prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank sudoku = and [(hasValue sudoku b) == Nothing | b <- blankSpots]
  where
    blankSpots = blanks sudoku

  

--nothingAt :: Sudoku -> [Pos]  -> Bool
--nothingAt a b = sudoku 
  --prop_blanks_allBlanks :: ...
  --prop_blanks_allBlanks =
  
  
  -- * E2
--Replaces position i with value y and returns a copy of the list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | i >= length xs || i < 0 = error "index out of bounds"
[] !!= (i,y) = []
xs !!= (i,y) = l1 ++ (y : tail l2)
              where (l1,l2) = (splitAt i xs)

-- Finish Writing these

validIndex :: Int -> [a] -> Bool
validIndex i [] = False
validIndex i s | i < 0 || length s <= i = False
validIndex i s | otherwise = True

prop_bangBangEquals_correct :: Integral a => [a] -> (Int, a)-> Property   --FIX THESE TESTS
prop_bangBangEquals_correct xs (i,y) = validIndex i xs ==> hasCorrectLength xs (xs !!= (i, y))

hasCorrectLength :: [a] -> [a] -> Bool
hasCorrectLength list1 list2 = length list1 == length list2

replacesValue :: Integral a => (Int, a) -> [a] -> Bool
replacesValue (i, y) list = ((list !!= (i, y)) !! i) == y

--E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (x, y) value = Sudoku (extractRow sudoku !!= (x, a !!= (y, value)))
                                      where a = extractRow sudoku !! x

validPosition :: Pos -> Bool
validPosition (x, y) | (x < 0  || x > 8) || (y < 0 || y > 8) = False
                      | otherwise = True

prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Property
prop_update_updated sudoku pos value = validPosition pos ==> hasValue (update sudoku pos value) pos == value


-- E4
candidates :: Sudoku -> Pos -> [Int]
candidates s p = filter (cand s p)  [1..9]

cand :: Sudoku -> Pos -> Int -> Bool
cand s p n | isOkay(update s p (Just n)) = True
           | otherwise = False
        
prop_candidates_correct :: Sudoku -> Pos -> Property
prop_candidates_correct s p = validPosition p ==> all isOkay (map (update s p . Just) (candidates s  p ))

solve :: Sudoku -> Maybe Sudoku
solve sudoku = solve' sudoku (blanks sudoku) 


solve' :: Sudoku -> [Pos] -> Maybe Sudoku 
solve' sudoku [] | not (isOkay sudoku) || not (isSudoku sudoku) = Nothing
                 | otherwise = Just sudoku
solve' sudoku (p:ps) = listToMaybe (catMaybes newSudoku)
         where 
          newSudoku = [solve' (update sudoku p (Just c)  ) ps | c <- candidates sudoku p]


readAndSolve :: FilePath -> IO()
readAndSolve path = do a <- (readSudoku path)
                       let sudoku = solve a
                       if isNothing sudoku
                        then error "could not solve sudoku"
                       else printSudoku (fromJust sudoku)

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf start solution = --isOkay solution && 
                              --isFilled solution &&
                              and [fst c == snd c || isNothing (fst c) | c <- b]
                  where b = zip
                            (concat [a | a <- (extractRow start)])
                            (concat [a | a <- (extractRow solution)])