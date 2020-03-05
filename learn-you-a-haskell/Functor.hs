-- Functor laws

-- identity
-- function composition associativity

-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

main = do 
    line <- fmap reverse getLine  
    putStrLn $ "You said " ++ line ++ " backwards!" 

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

-- Regular fmap :: (a -> b) -> f a -> f b

-- fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- instance Functor ((->) r) where
--     fmap f g = (.)

composition = fmap (*3) (+100) 1

composition2 = (*3) `fmap` (+100) $ 1  

composition3 = (*3) . (+100) $ 1  

-- 300