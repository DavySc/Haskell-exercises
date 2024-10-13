module Equality where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn t1 == TisAn t2 = (==) t1 t2
