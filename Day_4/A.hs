import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

if_valid :: [String] -> Int
if_valid strings =
    if (length strings) == 1
        then 1
        else if (strings !! ((length strings)-1)) == (strings !! ((length strings)-2))
            then 0
            else if_valid (take ((length strings)-1) strings)


count :: [String] -> Int
count [] = 0
count strings = (if_valid (quicksort  (splitOn " " (strings !! ((length strings) -1))))) + (count (take ((length strings)-1) strings))
    

main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let ans = count (splitOn "\n" contents)
        print ans
        
        hClose handle
        
