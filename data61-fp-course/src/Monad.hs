{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Monad where

import Core
import Functor
import Applicative
import Optional
import List

class Applicative m => Monad m where
    return :: a -> m a

    (=<<) :: (a -> m b) -> m a -> m b

instance Monad Optional where
    return :: a -> Optional a
    return = pure

    (=<<) :: (a -> Optional b) -> Optional a -> Optional b
    (=<<) f (Full a) = f a
    (=<<) _ Empty    = Empty 

instance Monad ((->) t) where
    return :: a -> (t -> a)
    return a = (\t -> a)

    (=<<) :: (a -> t -> b)
          -> (t -> a)
          -> (t -> b)
    (=<<) atb ta = \t -> atb (ta t) t

join :: Monad m => m (m a) -> m a
join = (id =<<)

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = flip (=<<)

(<**>) :: Monad m => m (a -> b) -> m a -> m b
(<**>) mab ma = mab >>= \m' -> ma >>= \ma' -> pure (m' ma')

(<=<) ::
    Monad f =>
    (b -> f c)
    -> (a -> f b)
    -> a
    -> f c
(<=<) bfc afb a = bfc =<< afb a
