module Instances where

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree

pure = Leaf

Leaf f <*> _ = Leaf f

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf

instance Functor RoseTree where
  fmap _ RoseLeaf = RoseLeaf
  fmap f (RoseNode a []) = RoseNode (f a) []
  fmap f (RoseNode a bs) = RoseNode (f a) ((fmap . fmap) f bs)
