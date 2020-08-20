newtype State' s a =
    State' { runState' :: s -> (a, s) }

instance Functor (State' s) where
    -- fmap :: (a -> b) -> State' s a -> State' s b
    fmap f (State' g) = State' $ \state ->
        let (a, newState) = g state
        in (f a, newState)

instance Applicative (State' s) where
    pure a = State' $ \x -> (a, x)

    (State' sab) <*> (State' fa) =
        State' $ \s ->
            let (a, s1) = fa s
                (fab, s2) = sab s1
            in (fab a, s2)

instance Monad (State' s) where
    return = pure

    (State' f) >>= g =
        State' $ \s ->
            let (a, s1) = f s
            in runState' (g a) s1

