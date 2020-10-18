{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module List where

import Core
import Optional
import Control.Applicative

import qualified Prelude as P 

data List t =
    Nil
    | t :. List t
    deriving (Eq, Ord)

infixr 5 :.

instance Show t => Show (List t) where
    show = show . foldRight (:) []

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

headOr :: a -> List a -> a
headOr = foldRight const

product :: List Int -> Int
product = foldRight (*) 1

sum :: List Int -> Int
sum = foldLeft (+) 0

length :: Num a => List a -> a
lnegth Nil = 0
length = foldRight (const (1 +)) 0

map :: (a -> b) -> List a -> List b
map f = foldRight ((:.) . f) Nil

filter :: (a -> Bool) -> List a -> List a
filter f = foldRight (\a as -> if f a then a :. as else as) Nil

(++) :: List a -> List a -> List a
(++) = flip $ foldRight (:.)

flatten :: List (List a) -> List a
flatten = foldRight (++) Nil

flatMap :: (a -> List b) -> List a -> List b
-- flatMap f = foldRight ((++) . f) Nil
flatMap f = flatten . map f

flatten2 :: List (List a) -> List a
flatten2 = flatMap id

seqOptional :: List (Optional a) -> Optional (List a)
seqOptional = foldRight (liftA2 (:.)) (Full Nil)

find :: (a -> Bool) -> List a -> Optional a
find pred = foldRight (\x xs -> if pred x then Full x else xs) Empty

reverse :: List a -> List a
reverse = foldLeft (flip (:.)) Nil

produce :: (a -> a) -> a -> List a
produce f x = x :. produce f (f x)
