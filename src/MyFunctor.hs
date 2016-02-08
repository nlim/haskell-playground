
{-# LANGUAGE NoImplicitPrelude #-}

module MyFunctor where

class Functor f where
  fmap :: (a -> b) -> f a -> f b


class Functor m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \a -> g (f a)

-- Functor of a function, for some input type r
instance Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  -- fmap f g = \r -> f (g r)
  fmap = (.)

instance Monad ((->) r) where
  return a = \_ -> a
  f >>= g = \r' -> g (f r') r'


