module CustomReverse where

customReverse :: [a] -> [a]
customReverse = foldl (flip (:)) []

customReverser :: [a] -> [a]
customReverser = foldr insertElem []
  where
    insertElem a reversed = reversed <> [a]
