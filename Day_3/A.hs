import System.IO  

absol :: Int -> Int
absol x =
    if x > 0
        then x
        else -x

additional_steps :: Int -> Int -> Int -> Int
additional_steps a b c =
    if (c-2*b) > a
        then additional_steps a b (c-2*b)
        else absol(c-a-b)

calculate :: Int -> Int -> Int
calculate a b =
    if a <= (b * b)
        then (div (b-1) 2) + (additional_steps a (div (b-1) 2) (b*b))
        else calculate a (b+2)


distance :: Int -> Int
distance a = (calculate a 1)



main = do  
        let input = 312051
        print (distance input)