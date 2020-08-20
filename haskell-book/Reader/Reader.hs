{-# LANGUAGE InstanceSigs #-}
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = (rev . cap)

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

monadicTupled :: [Char] -> ([Char], [Char])
monadicTupled = do
    capitalized <- cap
    reversed <- rev
    return (capitalized, reversed)

monadicTupled2 :: [Char] -> ([Char], [Char])
monadicTupled2 = do
    cap >>= (\c -> rev >>= (\r -> return (c, r)))

newtype Reader r a =
    Reader { runReader :: r -> a }

-- instance Functor (Reader r) where
--     fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)

ask :: (r -> a) -> Reader r a
ask f = Reader f

newtype HumanName =
    HumanName String
    deriving (Show, Eq)

newtype DogName =
    DogName String
    deriving (Show, Eq)

newtype Address =
    Address String
    deriving (Show, Eq)

data Person =
    Person {
          humanName :: HumanName
        , dogName :: DogName
        , address :: Address
    } deriving (Show, Eq)

data Dog =
    Dog {
          dogsName :: DogName
        , dogsAddress :: Address
    } deriving (Show, Eq)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (Reader rab) <*> (Reader ra) =
        Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
    return = pure

    (Reader ra) >>= arb =
        Reader $ \r -> runReader (arb $ ra r) $ r
