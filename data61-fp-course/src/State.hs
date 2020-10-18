{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module State where

import qualified Data.Set as S

import Core
import Functor
import Applicative
import Monad
import List
import Optional

newtype State s a =
    State {
        runState ::
            s
            -> (a, s)
    }

instance Functor (State s) where
    (<$>) :: (a -> b) -> State s a -> State s b
    (<$>) f sa = 
        State $ \s -> 
            let (a, s') = runState sa s 
            in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (<*>) ::
        State s (a -> b)
        -> State s a
        -> State s b
    (<*>) sab sa =
        State $ \s ->
            let (fab, s') = runState sab s
                (a, s2)   = runState sa s'
            in (fab a, s2)

instance Monad (State s) where
    return = pure

    (=<<) ::
        (a -> State s b)
        -> State s a
        -> State s b
    (=<<) asb sa =
        State $ \s ->
            let (a, s') = runState sa s
            in runState (asb a) s'

exec :: State s a -> s -> s
exec sa s =
    let (_, s') = runState sa s
    in s'

eval :: State s a -> s -> a
eval sa s =
    let (a, _) = runState sa s
    in a

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM pred =
    foldRight (\a -> 
        lift2 (\b oa -> if b then Full a else oa) (pred a)
    ) (pure Empty)

-----------------------------

firstRepeat :: Ord a => List a -> Optional a
firstRepeat xs = eval (findM (\a -> State (\s -> (S.member a s, S.insert a s))) xs) S.empty


