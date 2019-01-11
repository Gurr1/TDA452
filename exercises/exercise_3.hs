take1 :: Int -> [a] -> [a]
take1 n _      | n <= 0 =  []
take1 _ []              =  []
take1 n (x:xs)          =  x : take1 (n-1) xs

drop1 :: Int -> [a] -> [a]
drop1 0 x = x
drop1 n [] = []
drop1 n (x:xs) = drop1 (n-1) xs

split_at1 :: Int -> [a] -> ([a], [a])
split_at1 n xs = ((take1 n xs), (drop1 n xs))

zip31 ::  [a] -> [a] -> [a] -> [(a, a, a)]
zip31 a b c | null a || null b || null c = []
zip31 (a:as) (b:bs) (c:cs) = (a, b, c) : zip31 as bs cs

zip32 ::  [a] -> [a] -> [a] -> [(a, a, a)]
zip32 a b c = [(a, b, c) | (a, (b, c)) <-zip a $ zip b c]

--- 1 ---

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] b                      = True
isPermutation (a:as) bs | a `elem` bs = isPermutation as bs
isPermutation a b = False

prop_reverse :: [Int] -> Bool
prop_reverse xs = isPermutation xs (reverse xs)

--- 2 ---

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (a:as) | a `notElem` as = duplicates as
duplicates as = True

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []   = []
removeDuplicates (a:as) = (if a `elem` as then [] else [a]) ++ removeDuplicates as

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

--- 3 ---

pascal :: Int -> [Int]
pascal n = [over (n-1) a | a <- [0..(n-1)]]

over :: Int -> Int -> Int
over n k = factorial n `div` (factorial k * factorial (n - k))

factorial :: Int -> Int
factorial 0 = 1
factorial a = factorial (a-1) * a

--- 4 ---

crossOut :: Int -> [Int] -> [Int]
crossOut a bs = [b | b <- bs, (b `mod` a) /= 0]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (a:as) = a : sieve (crossOut a as)

--- 5 --- 

isPrime :: Int -> Bool
isPrime a = a `elem` sieve [2..100]
 
sumOfPrimes :: Int -> Bool
sumOfPrimes a = or [a == d + c | d <- b,
                                 c <- b]
                where b = sieve [2..100]

prop_all4_to100 :: [Bool]
prop_all4_to100 = (map sumOfPrimes [4, 6..100])

--- 6 ---

occursIn :: Eq a =>  a -> [a] -> Bool
occursIn x xs = or [x == y | y <- xs]

allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = and [x `occursIn` ys | x <- xs]

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = isPermutation xs ys

numOccurences :: Eq a => a -> [a] -> Int
numOccurences x xs = sum [if x == y then 1 else 0 | y <- xs]

bag :: Eq a => [a] -> [(a, Int)]
bag xs = removeDuplicates [(x, numOccurences x xs) | x <- xs]

--- 7 ---

positions :: [a] -> [(a, Int)]
positions xs = zip xs [1..]

firstPosition :: Eq a => a -> [a] -> Int
firstPosition x xs = head [p | (y, p) <- positions xs, y == x]

remove1st :: Eq a => a -> [a] -> [a]
remove1st x xs = a ++ (drop 1 b)
                    where (a, b) = splitAt (firstPosition x xs) xs

remove :: Eq a => Int -> a -> [a] -> [a]
remove 0 x xs = xs
remove n x xs = remove (n-1) x (remove1st x xs)