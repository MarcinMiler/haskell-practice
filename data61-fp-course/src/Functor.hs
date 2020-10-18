{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Functor where

import Core
import List
import Optional

import qualified Prelude as P

class Functor f where
    (<$>) ::
        (a -> b)
        -> f a
        -> f b

infixl 4 <$>

instance Functor List where
    (<$>) = map

instance Functor Optional where
    (<$>) f Empty = Empty
    (<$>) f (Full a) = Full $ f a

instance Functor ((->) t) where
    (<$>) :: (a -> b) -> (t -> a) -> (t -> b)
    (<$>) = (.)

(<$) :: Functor f => a -> f b -> f a
(<$) a = (const a <$>)