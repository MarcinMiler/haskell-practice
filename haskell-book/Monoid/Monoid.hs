import Data.Monoid

-- A monoid is a binary associative operation with an identity

a = mappend [1,2,3] [4,5,6]
a1 = mappend [1..5] []
a2 = mappend [] [1..5]
a3 = mappend [1..5] mempty

--class Monoid m where
--    mempty :: m
--    mappend :: m -> m -> m
--    mconcat :: [m] -> m
--    mconcat = foldr mappend mempty

a4 = mconcat [[1..3], [4..6]]
-- [1,2,3,4,5,6]

a5 = foldr (++) [] [[1..3], [4..6]]
a6 = foldr mappend mempty [[1..3], [4..6]]

--instance Monoid [a] where
--    mempty = []
--    mappend = (++)

-- mappend 1 2 <- wont work

b1 = mappend (Sum 1) (Sum 5)
-- Sum { getSum = 6 }
b2 = getSum $ mappend (Sum 1) (Sum 5) 
-- 6
b3 = getProduct $ mappend (Product 5) (Product 5)
-- 25

-- newType Sum a = Sum { getSum :: a }
-- newType Product a = Product { getProduct :: a }

c1 = mappend [1] [2] == [1] <> [2]
-- True

c2 = getSum $ mappend (Sum 1) (mappend (Sum 2) (Sum 3 ))
-- 6
c3 = getSum $ Sum 1 <> Sum 2 <> Sum 3
-- 6
c4 = mconcat [Sum 1, Sum 2, Sum 3, Sum 4]
-- 10

d1 = All True <> All True
-- All { getAll = True }
d2 = All True <> All False
-- All { getAll = false }

d3 = Any True <> Any False
-- Any { getAny = True }
d4 = Any False <> Any False
-- Any { getAny = False }

e1 = First (Just 1) <> First (Just 2)
-- First { getFirst = Just 1 }
e2 = Last (Just 1) <> Last (Just 2)
-- Last { getLast = Just 2 }

e3 = First Nothing <> First (Just 1)
-- First { getFirst = Just 1 }

data Booly a =
      False'
    | True'
    deriving (Show, Eq)

-- conjuction
instance Semigroup (Booly a) where
    False' <> _    = False'
    _ <> False'    = False'
    True' <> True' = True'

instance Monoid (Booly a) where
    mempty = False'

-- mempty :: Booly String
-- False'

data Optional a =
      Nada
    | Only a
    deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> Only a = Only a
    Only a <> Nada = Only a
    Only a <> Only b = Only (a <> b)

instance Semigroup a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

instance Functor Optional where
    fmap f Nada = Nada
    fmap f (Only a) = Only (f a)


z1 = getSum <$> Only (Sum 1) `mappend` Only (Sum 1)
-- Only (2)

