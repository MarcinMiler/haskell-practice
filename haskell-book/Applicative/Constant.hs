newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Show, Eq, Ord)

instance Functor (Constant a) where
    fmap f (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant f) <*> (Constant a) = (Constant (mappend f a))
