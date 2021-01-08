{-# language TypeFamilies #-}
module Representable where

class Representable f where
  -- a
  type Rep f :: *

  -- alpha :: Reader a x -> F x
  tabulate :: (Rep f -> x) -> f x

  -- beta  :: F x -> Reader a x
  index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Representable Stream where
  type Rep Stream = Int
  tabulate f = Cons (f 0) (tabulate (f . (+1)))
  index (Cons b bs) n | n == 0    = b
                      | otherwise = index bs (n-1)  
