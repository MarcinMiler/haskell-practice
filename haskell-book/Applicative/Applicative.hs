import Control.Applicative

--class Functor f => Applicative f where
--    pure :: a -> m a
--    (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--      pure = Just
--      Nothing <*> _ = Nothing
--      (Just f) <*> (Just a) = Just (f a)
--      or
--      (Just f) <*> a = fmap f a

-- Some my thoughts

x = (+) <$> Just 1
-- (+) <$> Just 1 :: Num a => Maybe (a -> a)

x1 = x <*> Just 2
-- Just 3
-- fmap lift function to given context and <*> apply another argument wrapped in context

x2 = (+) <$> Just 1 <*> Just 2
-- Just 3

x3 = Just (+) <*> Just 1 <*> Just 2
-- Just 3

x4 = (+1) <$> Just 1
-- Just 2

x5 = pure (+1) <*> Just 1
-- Just 2

x6 = liftA2 (+) (Just 1) (Just 2)
-- Just 3
