{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TraversAble21 where
import Data.Foldable
import Data.Functor
import Data.Traversable ()

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
instance Functor OddC  where
    fmap f (Un a) = Un (f a)
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

instance Foldable OddC where
    foldr f ini (Un a) = a `f` ini
    foldr f ini (Bi x y z) = f x (f y (foldr f ini z))

instance Traversable OddC where
    traverse f (Un a) = Un <$> f a
    traverse f (Bi x y z) = Bi <$> f x <*> f y <*> traverse f z 

cnt1 :: OddC Integer
cnt1 = Un 42
cnt3 :: OddC Integer
cnt3 = Bi 1 2 cnt1
cnt5 :: OddC Integer
cnt5 = Bi 3 4 cnt3
{-
instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x1 x2 r) = Bi (f x1) (f x2) (fmap f r)

instance Foldable OddC where
  foldMap f (Un x) = f x
  foldMap f (Bi x1 x2 r) = f x1 `mappend` f x2 `mappend` foldMap f r

instance Traversable OddC where
  sequenceA (Un x) = Un <$> x
  sequenceA (Bi x1 x2 r) = Bi <$> x1 <*> x2 <*> sequenceA r
-}