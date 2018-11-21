
maxi :: Ord a => a -> a -> a
maxi x y | x >= y = x
         | x < y  = y

sumsq :: Integer -> Integer
sumsq 0 = 0
sumsq x = sumsq (abs(x)-1) + (x * x)

alt_sumsq :: Integer -> Integer
alt_sumsq x = sum [x * x | x <- [1..x]]

prop_sumsq n = (sumsq n) == div (n * (n + 1) * (2*n + 1)) 6

x ~== y = abs(x - y) < 1e-4

fibon :: Integer -> Integer
fibon 1 = 1
fibon 2 = 1
fibon n = fibon(n-1) + fibon(n-2)


calcPossibleFactors :: Integer -> [Integer]
calcPossibleFactors n = [1..(floor (sqrt ( fromIntegral n ) ) ) + 1]

smallestFactor :: Integer -> Integer
smallestFactor a = minimum [b | b <- calcPossibleFactors a, (mod a b == 0) && not (b == 1)]

multiply :: Num a => [a] -> a
multiply [] = 1
multiply (a:as) = a * multiply as 