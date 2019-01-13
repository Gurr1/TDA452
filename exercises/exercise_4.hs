--- 0 ---

first :: IO()
first = do putStr "enter a number: "
           y <- getLine
           getNumbers (read y) 0

getNumbers :: Int -> Int -> Int
getNumbers n = y 
               where do y <- getLine
               