-- class (Functor f) => Applicative f where
--    pure :: a -> f a
--    <*> :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     (Just f) <*> something = fmap f something

app = Just (+) <*> Just 1 <*> Just 2

-- Just 3

app2 = pure (+) <*> Just 3 <*> Just 5 

-- Just 8

app3 = (+) <$> Just 1 <*> Just 5

-- Just 6

-- instance Applicative [] where
--    pure x = [x]
--    fs <*> xs = [f x | f <- fs, x <- xs]

app4 = pure "Hey" :: [String]

-- ["Hey"]

app5 = [(*0),(+100)] <*> [1,2,3]

-- [0,0,0,101,102,103]

-- instance Applicative IO where
--    pure = return
--    a <*> b = do
--        f <- a
--        x <- b
--        return (f x)

myAction :: IO String  
myAction = (++) <$> getLine <*> getLine

-- instance Applicative (-> r) where
--    pure x = (\_ -> x)
--    f <*> g = \x -> f x (g x)

app6 = (+) <$> (+3) <*> (*100) $ 5

-- 508

-------------------------------------------------------------------------------------------------

liftA2 :: (Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 f a b = f <$> a <*> b

l = fmap (\x -> [x]) (Just 4)

-- Just [4]

l2 = liftA2 (:) (Just 3) (Just [4])

-- Just [3,4]

l3 = (:) <$> Just 3 <*> Just [4]

-- Just [3,4]

sequanceA :: (Applicative f) => [f a] -> f [a]
sequanceA [] = pure []
sequanceA (x:xs) = (:) <$> x <*> sequanceA xs

s1 = sequenceA [Just 3, Just 2, Just 1]

-- Just [3,2,1]

s2 = sequenceA [(+3),(+2),(+1)] 3 

-- [6,5,4]

s3 = sequenceA [[1,2,3],[4,5,6]]

-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

s4 = sequenceA [(>4),(<10),odd] 7 

-- [True, True, True]

s5 = and $ sequenceA [(>4),(<10),odd] 7

-- True