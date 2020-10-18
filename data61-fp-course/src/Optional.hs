{-# LANGUAGE NoImplicitPrelude #-}

module Optional where

import Core

import qualified Prelude as P
import qualified Control.Applicative as A
    
data Optional a =
      Full a
    | Empty
    deriving (Show)

instance P.Functor Optional where
    fmap f Empty = Empty
    fmap f (Full a) = Full $ f a

instance A.Applicative Optional where
    pure = Full

    Empty <*> _ = Empty
    (Full f) <*> a = P.fmap f a
    -- (<*>) = M.ap

instance P.Monad Optional where
    return = A.pure

    (Full a) >>= f = f a

(??) :: Optional a -> a -> a
Full a ?? _ = a
Empty  ?? fallback = fallback

(<+>) :: Optional a -> Optional a -> Optional a
Full a <+> _ = Full a
Empty  <+> op = op
