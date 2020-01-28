import System.IO  
import Control.Monad
import Data.Char


checksum :: String -> Int
checksum length = 0
checksum s1 = if
    s1 !! ((length s1) - 1) == s1 !! ((length s1) - 2)
    then digitToInt (s1 !! ((length s1) - 1)) + checksum (take ((length s1)-1) s1) 
    else 0 + checksum (take ((length s1)-1) s1) 


main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let input_string = words contents !! 0
        let len = length input_string
        let first = input_string !! 0
        let second = input_string !! (len-1)
        let x = if first == second
            then digitToInt first
            else 0
        
        let ans = x + checksum input_string
        print ans
        
        hClose handle
        
