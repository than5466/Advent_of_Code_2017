import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split

check_divisor :: [String] -> Int -> Int -> Int -> Int
check_divisor s a b n =
    if mod (read (s !! a)::Int) (read (s !! b)::Int) == 0
        then div (read (s !! a)::Int)  (read (s !! b)::Int)
        else if mod (read (s !! b)::Int) (read (s !! a)::Int) == 0
            then div (read (s !! b)::Int)  (read (s !! a)::Int)
            else if b == n 
                then check_divisor s (a+1) (a+2) n
                else check_divisor s a (b+1) n

divisor :: [String] -> Int
divisor s = check_divisor s 0 1 ((length s) -1)


checksum :: [String] -> Int
checksum s = 
    if length s == 0
    then 0
    else (divisor (splitOn "\t" (s !! (length s - 1)))) + checksum (take ((length s)-1) s)

main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let ans = checksum (splitOn "\n" contents)
        print ans
        
        hClose handle
        
