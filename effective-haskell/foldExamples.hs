module FoldExamples where

import Prelude hiding (foldl, foldr, map)

foldl f carryValue lst =
  if null lst
    then carryValue
    else foldl f (f carryValue (head lst)) (tail lst)

foldr f carryValue lst =
  if null lst
    then carryValue
    else f (head lst) $ foldr f carryValue (tail lst)
