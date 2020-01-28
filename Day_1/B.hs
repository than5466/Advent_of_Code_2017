import System.IO  
import Control.Monad
import Data.Char


checksum :: String -> Int -> Int
checksum s x = if length s == x
    then 0
    else if s !! ((length s) - 1) == s !! ((length s) - 1 - x)
        then 2 * digitToInt (s !! ((length s) - 1)) + checksum (take ((length s)-1) s) x
        else 0 + checksum (take ((length s)-1) s) x


main = do  
        handle <- openFile "data.txt" ReadMode
        contents <- hGetContents handle
        let input_string = words contents !! 0
        let len = length input_string
        let n = div len 2
        let first = input_string !! 0
        let second = input_string !! (len-1)
        
        let ans = checksum input_string n
        print ans
        
        hClose handle
        
