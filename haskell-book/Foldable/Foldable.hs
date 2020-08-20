import Data.Foldable
import Data.Monoid

--class Foldable (t :: * -> *) where
--    fold :: Monoid m => t m -> m
--    foldMap :: Monoid m => (a -> m) -> t a -> m

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a =
      Only a
    | Nada
    deriving (Show, Eq, Ord)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Only x) = f x z

    foldl _ z Nada = z
    foldl f z (Only x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Only x) = f x

-- foldMap (+1) Nada :: Product Int
-- Product { getProduct = 1 }
