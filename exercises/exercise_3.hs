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
