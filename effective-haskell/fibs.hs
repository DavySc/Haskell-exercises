module Fibs where

fibs = 0 : 1 : helper fibs (tail fibs)
  where
    helper (a:as) (b:bs) =
      a+b : helper as bs
