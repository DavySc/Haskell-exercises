module Hanoi where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n x y z = hanoi (n - 1) x z y ++ [(x, z)] ++ hanoi (n - 1) y x z

-- Not working yet
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n x y z w = hanoi4 k x w z y ++ hanoi (n - k) x y w ++ hanoi4 k y x z w
  where
    n' = fromIntegral n :: Double
    k = n - round (sqrt (2 * n' - 1)) + 1
