newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicate m) => Applicative (MaybeT m) where
    pure a = MaybeT $ pure $ pure a

    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT $ f y

