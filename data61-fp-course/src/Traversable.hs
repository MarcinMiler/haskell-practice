{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Traversable where

import Core
import Functor
import Applicative
import List
import Optional

class Functor t => Traversable t where
    traverse :: Applicative k =>
        (a -> k b)
        -> t a
        -> k (t b)

optional :: (a -> b) -> b -> Optional a -> b
optional f b oa =
    (f <$> oa) ?? b

instance Traversable Optional where
    traverse f = 
        optional (\a -> Full <$> f a) (pure Empty)

instance Traversable List where
    traverse f =
        foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

sequenceA ::
    (Applicative f, Traversable t) =>
    t (f a)
    -> f (t a)
sequenceA tfa =
    traverse id tfa
