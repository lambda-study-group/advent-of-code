module Algebra where

type Algebra f a = f a -> a 
type CoAlgebra f a = a -> f a

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f a -> CoAlgebra f b -> b -> a
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg 

data ListF e a = NilF | ConsF e a

instance Functor (ListF e) where
  fmap _ NilF        = NilF
  fmap f (ConsF e a) = ConsF e (f a)

data TreeF e a = LeafF | SingleF e a | DoubleF e a a  

instance Functor (TreeF e) where
  fmap _ LeafF = LeafF
  fmap f (SingleF e a) = SingleF e (f a)
  fmap f (DoubleF e l r) = DoubleF e (f l) (f r)
