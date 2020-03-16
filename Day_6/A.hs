import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Data.List
import Data.Ord

toInt :: [String] -> [Int]
toInt = map read

addOne:: [Int] -> Int -> Int -> [Int]
addOne lst pos var = take pos lst ++ (var+1) : drop (pos+1) lst

resetPos:: [Int] -> Int -> [Int]
resetPos lst pos = take pos lst ++ 0 : drop (pos+1) lst

another_f :: [Int] -> Int -> Int -> [Int]
another_f lst n i =
    if n == 0
        then lst
        else another_f (addOne lst i (lst !! i)) (n-1) (mod (i+1) 16)

second_f :: [Int] -> Int -> Int -> [Int]
second_f lst m i =
    if (lst !! i) == m
        then another_f (resetPos lst i) m (mod (i+1) 16)
        else second_f lst m (i+1)

f :: [Int] -> [Int]
f lst = second_f lst (maximum lst) 0


withRec :: [Int] -> [Int] -> Int -> Int
withRec lst snap n = 
    if n > 0 && lst == snap
        then n
        else withRec (f lst) snap (n+1)


main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let lst = toInt(words contents)
        let snap = toInt(words contents)

        let ans = withRec lst snap 0

        print ans

        hClose handle