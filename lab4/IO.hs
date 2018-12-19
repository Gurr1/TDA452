module IO where 

import Minesweeper 
import Data.Char
import Data.List
import System.Random

printField :: Field -> IO()
printField field = putStr (concat [concat [toCharacter num | num <- row]
                    ++ "\n" | row <- rows])
                    where rows = extractRow field

toCharacter :: Tile -> [Char]
toCharacter Unknown = "[■]"
toCharacter Flag = "[⚑]"
toCharacter Empty = "[□]"
toCharacter Bomb = "[¤]"
toCharacter (Num a) = "[" ++ show a ++ "]"

--toTile :: [Char] -> Tile
--toTile 

play :: Field -> Field -> IO()
play reference shown =  do printField shown
                           putStrLn "choose a x position"
                           x <- getLine
                           putStrLn "choose a y position"
                           y <- getLine
                           putStrLn "Click or Flag? (C/F)"
                           tile <- getLine
                           if not (tile == "C" || tile ==  "F" || tile == "c" || tile == "f")
                            then
                                play reference shown 
                           else do
                                putStrLn ("placing a " ++ tile  ++ " on position" ++ "(" ++ x ++ " " ++ y ++ ")")
                                putStrLn "are you sure (Y/n)"
                                q <- getLine
                                if q == "N" || q == "n"
                                 then
                                    play reference shown
                                else
                                    if tile == "F" || tile == "f" then
                                        if reveal (read x, read y) shown == Flag then
                                            play reference $ replaceTile shown Empty (read x, read y) 
                                        else  
                                            play reference $ replaceTile shown Flag (read x, read y)
                                    else    -- tile is 
                                        if reveal (read x, read y) reference == Bomb then do
                                            putStrLn "you lost"
                                            printField reference
                                        else 
                                            play reference $ replaceTile shown (reveal (read x, read y) reference) (read x, read y)
