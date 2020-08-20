data Sum a b =
      First a
    | Second b
    deriving (Show, Eq)

instance Functor (Sum a) where
    fmap f (First x) = First x
    fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
    pure a = Second a
    (Second f) <*> (Second a) = Second (f a) 

instance Monad (Sum a) where
    return = pure
    (Second a) >>= f = f a
