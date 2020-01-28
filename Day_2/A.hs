import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split


max_func :: [String] -> Integer -> Integer
max_func s x = 
    if length s == 0
    then x
    else max_func (take ((length s)-1) s) (max_value (read (s !!((length s) -1)) :: Integer) x)

max_value :: Integer -> Integer -> Integer
max_value x y = 
    if x > y
    then x
    else y

min_func :: [String] -> Integer -> Integer
min_func s x = 
    if length s == 0
    then x
    else min_func (take ((length s)-1) s) (min_value (read (s !!((length s)-1)) :: Integer) x)

min_value :: Integer -> Integer -> Integer
min_value x y = 
    if x < y
    then x
    else y

checksum :: [String] -> Integer
checksum s = 
    if length s == 0
    then 0
    else (max_func (splitOn "\t" (s !! (length s - 1))) 0 - min_func(splitOn "\t" (s !! (length s - 1))) 1000) + checksum (take ((length s)-1) s)

main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let ans = checksum (splitOn "\n" contents)
        print ans
        
        hClose handle
        
