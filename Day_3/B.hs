import System.IO  

calculate_rest :: [Int] -> Int -> Int
calculate_rest mylist a =
    if (length mylist) < (((2*a-1)*(2*a-1))+2*a-1)
        then (mylist !! ((length mylist) - 1)) + (mylist !! ((length mylist)-1-8*(a-1)+1)) + (mylist !! ((length mylist)-1-8*(a-1))) + (mylist !! ((length mylist)-1-8*(a-1)-1))
        else if (length mylist) < (((2*a-1)*(2*a-1))+4*a-1)
            then (mylist !! ((length mylist) - 1)) + (mylist !! ((length mylist)-1-8*(a-1)-1)) + (mylist !! ((length mylist)-1-8*(a-1)-2)) + (mylist !! ((length mylist)-1-8*(a-1)-3))
            else if (length mylist) < (((2*a-1)*(2*a-1))+6*a-1)
                then (mylist !! ((length mylist) - 1)) + (mylist !! ((length mylist)-1-8*(a-1)-3)) + (mylist !! ((length mylist)-1-8*(a-1)-4)) + (mylist !! ((length mylist)-1-8*(a-1)-5))
                else (mylist !! ((length mylist) - 1)) + (mylist !! ((length mylist)-1-8*(a-1)-5)) + (mylist !! ((length mylist)-1-8*(a-1)-6)) + (mylist !! ((length mylist)-1-8*(a-1)-7))

calculate_if_before_corner :: [Int] -> Int -> Int
calculate_if_before_corner mylist a =
    if (length mylist) == ((2*(a+1)-1)*(2*(a+1)-1))
        then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*a-1)*(2*a-1)+1))) + (mylist !! ((2*a-1)*(2*a-1)))
        else if (length mylist) == (((2*a-1)*(2*a-1))+2*a-1)
            then (mylist !! ((length mylist) - 1)) + (mylist !! ((2*(a-1)-1)*(2*(a-1)-1)+2*(a-1))) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+2*(a-1)-1))
            else if (length mylist) == (((2*a-1)*(2*a-1))+4*a-1)
                then (mylist !! ((length mylist) - 1)) + (mylist !! ((2*(a-1)-1)*(2*(a-1)-1)+4*(a-1))) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+4*(a-1)-1))
                else if (length mylist) == (((2*a-1)*(2*a-1))+6*a-1)
                    then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+6*(a-1))) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+6*(a-1)-1))
                    else calculate_rest mylist a

calculate_if_after_corner :: [Int] -> Int -> Int
calculate_if_after_corner mylist a =
    if (length mylist) == ((2*a-1)*(2*a-1))+2
        then (mylist !! ((length mylist) - 1)) + (mylist !! ((2*(a-1)-1)*(2*(a-1)-1)+1)) + (mylist !! ((length mylist) - 2)) + (mylist !! ((2*(a-1)-1)*(2*(a-1)-1)+2))
        else if (length mylist) == (((2*a-1)*(2*a-1))+2*a+1)
            then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+2*(a-1))) + (mylist !! ((length mylist) - 2)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+2*(a-1)+1))
            else if (length mylist) == (((2*a-1)*(2*a-1))+4*a+1)
                then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+4*(a-1))) + (mylist !! ((length mylist) - 2)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+4*(a-1)+1))
                else if (length mylist) == (((2*a-1)*(2*a-1))+6*a+1)
                    then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+6*(a-1))) + (mylist !! ((length mylist) - 2)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+6*(a-1)+1))
                    else calculate_if_before_corner mylist a

calculate_if_corner :: [Int] -> Int -> Int
calculate_if_corner mylist a =
    if (length mylist) == ((2*a-1)*(2*a-1))+1
        then (mylist !! ((length mylist) - 1)) + (mylist !! ((2*(a-1)-1)*(2*(a-1)-1)+1))
        else if (length mylist) == (((2*a-1)*(2*a-1))+2*a)
            then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+2*(a-1)))
            else if (length mylist) == (((2*a-1)*(2*a-1))+4*a)
                then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+4*(a-1)))
                else if (length mylist) == (((2*a-1)*(2*a-1))+6*a)
                    then (mylist !! ((length mylist) - 1)) + (mylist !! (((2*(a-1)-1)*(2*(a-1)-1))+6*(a-1)))
                    else calculate_if_after_corner mylist a


calculate :: [Int] -> Int -> Int -> Int
calculate mylist a n =
    if (mylist !! ((length mylist)-1)) > n
        then (mylist !!((length mylist)-1))
        else if (length mylist) == ((2*a+1)*(2*a+1))+1
            then calculate (mylist ++ [(calculate_if_corner mylist (a+1))]) (a+1) n
            else calculate (mylist ++ [(calculate_if_corner mylist a)]) a n


find_next :: Int -> Int
find_next n = calculate [0,1,1,2,4,5,10,11,23,25] 1 n

main = do  
        let input = 312051
        print (find_next input)