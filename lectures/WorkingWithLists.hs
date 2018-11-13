-- | Working with List
-- Examples to illustrate pattern matching, recursion and testing for lists
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This is just as a skeleton, the definitions will be filled in
during the lecture.
-}

import Prelude hiding ((++),reverse,take,drop,splitAt,zip,unzip)
import qualified Prelude

import Test.QuickCheck

null :: [a] -> Bool
null []      = True
null _       = False    -- Wildcard?

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

cons x xs = x:xs

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reverse :: [a] -> [a]
reverse[]       = []
reverse (x:xs)  = reverse xs ++ [x]
-- complexity? how to make it more efficient?

-- | Take the first n elements of a list
take :: Int -> [a] -> [a]
take n xs | n<=0  = []
take n []         = []
take n (x:xs)     = x:take (n-1) xs


prop_take n xs = n>=0 ==> length (take n xs) == min n (length xs)


-- | Discard the first n elements of a list
drop :: Int -> [a] -> [a]
drop n xs | n<=0 = xs
drop n []        = []
drop n (_:xs)    = drop (n-1) xs

prop_take_drop :: Int -> [Bool] -> Bool
prop_take_drop n xs = take n xs ++ drop n xs == xs

nonprop_take_drop :: Int -> [Bool] -> Bool
nonprop_take_drop n xs = drop n xs ++ take n xs == xs


-- | splitAt n xs = (take n xs,drop n xs)
--splitAt :: Int -> [a] -> ([a],[a])

-- | Combine a pair of list into a list of pairs
zip :: [a] -> [b]   -> [(a,b)]
--zip [] []           =  []
--zip (x:xs) []       =  []
--zip [] (x:xs)       =  []
--zip (x:xs) (y:ys)   =  (x,y):zip xs ys

zip (x:xs) (y:ys)   =  (x,y):zip xs ys
zip _ _             =  []

-- | Split a list of pairs into a pair of lists
unzip :: [(a,b)] -> ([a],[b])
--unzip xys = ([x | (x,y)] <- xys, [y | (x,y) <- xys])
unzip []    = ([], [])
unzip ((x,y):xys) = (x:xs, y:ys)
    where (xs,ys) = unzip xys

prop_zip_unzip :: [(Bool, Int)] -> Bool
prop_zip_unzip xys = zip xs ys == xys
    where (xs,ys) = unzip xys

prop_unzip_zip :: [Bool] -> [Int] -> Bool
prop_unzip_zip xs ys = unzip (zip xs ys) == (take n xs, take n ys)
    where n = min (length xs) (length ys)


-- | "Quicksort"
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
    where
        smaller = [x'|x'<- xs,x'<=x]
        bigger = [x'|x'<-xs,x'>x]

isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = x1<=x2 && isSorted (x2:xs)

-- | insert a new element at the right position in a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (x':xs) | x<=x' = x:x':xs
                 | otherwise = x':insert x xs 
prop_insert :: Int -> [Int] -> Property
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)
                        
-- | Insertion sort (sort a list by using insert)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

prop_qsort :: [Int] -> Bool
prop_qsort xs = qsort xs == isort xs
