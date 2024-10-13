module Zip where

zipWithRecursive f (a : as) (b : bs) = f a b : zipWithRecursive f as bs
zipWithRecursive f [] _ = []
zipWithRecursive f _ [] = []
