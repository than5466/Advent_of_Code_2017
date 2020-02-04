import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split

toInt :: [String] -> [Int]
toInt = map read

addOne:: [Int] -> Int -> Int -> [Int]
addOne lst pos val = take pos lst ++ val : drop (pos+1) lst

recursv :: [Int] -> Int -> Int -> Int -> Int
recursv lst len current x =
    if current >= len || current < 0
        then x
        else recursv (addOne lst current ((lst !! current)+1)) len ((current+(lst !! current))) x+1


main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let lst = toInt(words contents)
        let len = length lst
        let current_index = 0
        let x = 0
        let ans = recursv lst len current_index x

        print ans

        hClose handle