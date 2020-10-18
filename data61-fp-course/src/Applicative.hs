{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Applicative where

import Core
import Functor
import Optional
import List

import qualified Prelude as P

class Functor f => Applicative f where
    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

instance Applicative Optional where
    pure = Full

    Empty    <*> _  = Empty
    (Full f) <*> fa = f <$> fa  

instance Applicative List where
    pure :: a -> List a
    pure = (:. Nil)

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) f a = flatMap (flip map a) f

instance Applicative ((->) t) where
    pure :: a -> (t -> a)
    pure = const

    (<*>) :: (t -> (a -> b)) -> (t -> a) -> (t -> b)
    (<*>) tab ta t = tab t $ ta t

lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a1 = ((f <$> a1) <*>)

(*>) :: Applicative f => f a -> f b -> f b
(*>) = lift2 $ flip const

(<*) :: Applicative f => f a -> f b -> f a
(<*) = lift2 const

sequence :: Applicative f => List (f a) -> f (List a)
sequence = foldRight (lift2 (:.)) (pure Nil)

