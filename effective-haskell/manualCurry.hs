module ManualCurry where

manualCurry :: ((a, b) -> c) -> a -> b -> c
manualCurry f a b = f (a, b)

manualUncurry :: (a -> b -> c) -> ((a, b) -> c)
manualUncurry f (a, b) = f a b
