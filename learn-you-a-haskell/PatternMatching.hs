lucky :: (Integral a) => a -> String
lucky 13 = "Lucky you"
lucky x = "You are no lucky"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

first :: (a, b, c) -> a  
first (x, _, _) = x

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

