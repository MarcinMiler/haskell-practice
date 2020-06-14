--class Functor f where
--    fmap :: (a -> b) -> f a -> f b

-- Fun hint, you can check type for fmap for diffrent types
-- Prelude> :set -STypeApplications
-- Prelude> :type fmap @Maybe
-- Prelude> fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

data Optional a =
      Nada
    | Only a
    deriving (Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Only a) = Only (f a)

data Two a b =
    Two a b
    deriving (Show)

data Or a b =
      First a
    | Second b
    deriving (Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

data Possibly a =
      LolNope
    | Yep a
    deriving (Show)

instance Functor Possibly where
    fmap _ (LolNope) = LolNope
    fmap f (Yep a) = Yep (f a)

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

x = getConstant $ fmap (+1) (Constant 1)
-- 1

data Wrap f a =
    Wrap (f a)
    deriving (Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

x1 = fmap (+1) (Wrap (Just 1))
-- Wrap (Just 2)

