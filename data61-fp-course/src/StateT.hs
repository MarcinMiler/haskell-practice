{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module StateT where

import Core
import Functor
import Applicative
import Monad
import List
import Optional

newtype StateT s f a =
    StateT {
        runStateT :: s -> f (a, s)
    }

instance Functor f => Functor (StateT s f) where
    (<$>) ::
        (a -> b)
        -> StateT s f a
        -> StateT s f b
    (<$>) f sfa =
        StateT $ \s ->
            (\(a, s') -> (f a, s')) <$> runStateT sfa s

instance Monad f => Applicative (StateT s f) where
    pure :: a -> StateT s f a
    pure a = StateT $ \s -> pure (a, s)
    
    (<*>) ::
        StateT s f (a -> b)
        -> StateT s f a
        -> StateT s f b
    (<*>) sfab sfa =
        StateT $ \s ->
            (\(ab, s') -> (\(a, s'') -> (ab a, s''))
                <$> runStateT sfa s') 
                =<< runStateT sfab s

instance Monad f => Monad (StateT s f) where
    return = pure

    (=<<) ::
        (a -> StateT s f b)
        -> StateT s f a
        -> StateT s f b
    (=<<) f sfa =
        StateT $ \s ->
           (\(a, s') -> runStateT (f a) s') =<< runStateT sfa s
            
