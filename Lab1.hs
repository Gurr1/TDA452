-- Part 1:

-- It takes k + 1 steps. 

-- Part 2

power1 :: Integer -> Integer -> Integer
power1 n 0 = 1
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product (replicate (fromInteger k) n)


-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative argument"
power2 n k | even k = power2 (n*n) (div k 2)
power2 n k | odd k = n * power2 n (k-1)


-- Part 4

--A) Inputs we till test on are k = [0, 1, 3, 6, 100, 105]
-- We choose these because 0 is an edge case, 1 is the smallest 
-- possible normal case, 3 and 6 are small, even and odd numbers
-- 100 and 105 are large, even and odd numbers
-- n = [-5, -1, 0, 1, 6, 70]
-- We chose these because:
-- 0 is a possible breaker, 1 is the smallest possible number, 
-- 6 is a small possitive number, 70 is a large possitive number.

ncases = [0, 1, 3, 6, 100, 105]
kcases = [0, 1, 6, 70]      -- negative number will cause exception
-- therefore they are left out

-- B)

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power2 n k == power1 n k)

-- C)

prop_test :: [Integer] -> [Integer] -> Bool
prop_test ns ks = and [prop_powers n k | n <- ns, k <- ks] 


prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)