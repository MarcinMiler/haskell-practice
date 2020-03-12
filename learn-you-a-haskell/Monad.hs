-- class Monad m where
--     return :: a -> m a

--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y

--     fail :: String -> m a
--     fail msg = error msg

-- class (Functor f) => Applicative f where
--    pure :: a -> f a
--    <*> :: f (a -> b) -> f a -> f b

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

applyMaybe1 = Just 3 `applyMaybe` \x -> Just (x+1)

-- Just 4

-- instance Monad Maybe where
--     return x = Just x

--     Nothing >>= f = Nothing
--     Just x >>= f  = f x

--     fail _        = Nothing

monadMaybe = return "lol" :: Maybe String

-- Just "lol"

monadMaybe1 = Just 1 >>= (\x -> return (x + 1))

-- Just 2

monadMaybe2 = Nothing >>= (\x -> return (x + 1))

-- Nothing

--------------------------------------------------------------------

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

land = landRight 1 (0,0) >>= landLeft 2

-- Just (2,1)

land2 = Nothing >>= landLeft 2

-- Nothing

land3 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

-- Just (2,4)

land4 = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

-- Nothing

-- do notation

res = Just 3 >>= (\x -> Just (show x ++ "!"))

-- Just "3!"

res2 :: Maybe String  
res2 = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)

-- Jusst "3!"

routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second

-- Just (3,2)

-- do failing

res3 :: Maybe Char
res3 = do
    (x:xs) <- Just ""
    return x

-- Nothing

----------------------------------------------------------------------------------------

-- List Monad

-- instance Monad [] where
--     return x = [x]

--     xs >>= f = concat (map f xs)

--     fail _ = []

listMonad = [3,4,5] >>= \x -> [x,-x]

-- [3,-3,4,-4,5,-5]

listMonadFail = [] >>= \x -> ["bad"]

-- []

listMonad2 = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

-- [(1,'a'), (1,'b'), (2,'a'),(2,'b')]

listMonad3 = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

-- [(1,'a'), (1,'b'), (2,'a'),(2,'b')]

listMonad4 = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

-- [(1,'a'), (1,'b'), (2,'a'),(2,'b')]

-- class Monad m => MonadPlus a where
--     mzero :: m a
--     mplus = m a -> m a -> m a

-- instance MonadPlus where
--     mzero = []
--     mplus = (++)

-------------------------------------------------------------------------------

-- Monads laws

-- Left identity

-- return x >>= f == f x

-- Right identity

-- m >>= return == m

-- Associativity

-- (m >>= f) >>= g == m >>= (\x -> f x >>= g)