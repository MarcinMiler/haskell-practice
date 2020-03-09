import Data.Monoid

-- class Monoid where
--     mempty :: m
--     mappend :: m -> m -> m
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty

-- instance Monoid [a] where  
--     mempty = []  
--     mappend = (++)

list = [1,2,3] `mappend` [4,5,6]

-- [1,2,3,4,5,6]

string = ("one" `mappend` "two") `mappend` "tree"

-- "onetwotree"

string2 = "pang" `mappend` mempty 

-- "pang"

list2 = mconcat [[1,2],[3,6],[9]]

-- [1,2,3,6,9]

-- newtype Product a = Product { getProduct :: a }  
--     deriving (Eq, Ord, Read, Show, Bounded)

-- instance Num a => Monoid (Product a) where  
--     mempty = Product 1  
--     Product x `mappend` Product y = Product (x * y) 

product = getProduct $ Product 3 `mappend` Product 9

-- 27

product2 = getProduct . mconcat . map Product $ [3,4,2]

-- 24

sum = getSum $ Sum 2 `mappend` Sum 9

-- 11

sum2 = getSum . mconcat . map Sum $ [1,2,3]

-- 6

-- newtype Any = Any { getAny :: Bool }  
--     deriving (Eq, Ord, Read, Show, Bounded)

-- instance Monoid Any where  
--         mempty = Any False  
--         Any x `mappend` Any y = Any (x || y)

any = getAny $ Any True `mappend` Any False

-- True

any2 = getAny . mconcat . map Any $ [False, False, False, True]

-- True

any3 = getAny $ mempty `mappend` mempty

-- False

-- newtype All = All { getAll :: Bool }  
--         deriving (Eq, Ord, Read, Show, Bounded)

-- instance Monoid All where  
--         mempty = All True  
--         All x `mappend` All y = All (x && y)

all = getAll . mconcat . map All $ [True, True, True]

-- True

-- instance Monoid Ordering where  
--     mempty = EQ  
--     LT `mappend` _ = LT  
--     EQ `mappend` y = y  
--     GT `mappend` _ = GT

compare = 1 `compare` 2

-- LT

compare2 = LT `mappend` GT

-- LT

compare3 = mempty `mappend` GT

-- GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare "aa" "aaa"

-- LT

lengthCompare "zzz" "aaa"

-- GT

-- instance (Monoid a) => Monoid (Maybe a) where
--     mempty = Nothing
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `append` Just m2 = Just (m1 `mappend` m2) 

m = Nothing `mappend` Just "andy"

-- Just "andy"

m2 = Just (Sum 3) `mappend` Just (Sum 4)

-- Just (Sum { getSum = 7 })