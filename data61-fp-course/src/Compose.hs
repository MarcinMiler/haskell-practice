{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose where

import Core
import Functor
import Applicative
import Monad

data Compose f g a =
    Compose (f (g a))
    deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    (<$>) f (Compose x) = Compose ((f <$>) <$> x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure

    (<*>) (Compose fgab) (Compose fga) = 
        Compose (lift2 (<*>) fgab fga)

