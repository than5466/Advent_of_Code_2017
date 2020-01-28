import System.IO  
import Control.Monad
import Data.Char


slice :: String -> Int
slice length = 0
slice s1 = if
    s1 !! ((length s1) - 1) == s1 !! ((length s1) - 2)
    then digitToInt (s1 !! ((length s1) - 1)) + slice (take ((length s1)-1) s1) 
    else 0 + slice (take ((length s1)-1) s1) 


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
        
        let ans = x + slice input_string
        print ans
        
        hClose handle
        
