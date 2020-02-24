maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Integer -> Integer -> [Integer]
replicate' 0 _ = []
replicate' x y
    | x <= 0 = []
    | otherwise = x:replicate' (x-1) y 

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [a] -> [a]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = x:y:zip' xs ys
