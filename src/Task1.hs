{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Hide built-in bind definition
import Prelude hiding ((>>=))

import Data.Functor.Identity

-- * Join monad

-- | Monad based on 'join' operation
-- instead of usual bind operator '(>>=)'.
class Applicative m => JoinMonad m where
  join :: m (m a) -> m a

-- * Equivalent views

infixl 1 >>=
(>>=) :: JoinMonad m => m a -> (a -> m b) -> m b
ma >>= f = (join . fmap f) ma

infixr 1 >=>
(>=>) :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a = f a >>= g

-- * Instances

instance JoinMonad Identity where
  join :: Identity (Identity a) -> Identity a
  join (Identity x) = x

instance JoinMonad Maybe where
  join :: Maybe (Maybe a) -> Maybe a
  join Nothing = Nothing
  join (Just x) = x

instance JoinMonad [] where
  join :: [[a]] -> [a]
  join = concat

instance (Monoid e) => JoinMonad ((,) e) where
  join :: Monoid e => (e, (e, a)) -> (e, a)
  join (e1, (e2, x)) = (e1 <> e2, x)

instance JoinMonad ((->) e) where
  join :: (e -> (e -> a)) -> (e -> a)
  join f e = f e e
