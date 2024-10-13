module SumBiggest where

import Data.List

sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  let getBiggests :: [Int] -> [Int]
      getBiggests xs = filter (\x -> x == maximum xs) xs

      getSmallests :: [Int] -> [Int]
      getSmallests xs = filter (\x -> x == minimum xs) xs

      allBiggests :: [[Int]]
      allBiggests = map getBiggests allNums

      allSmallests :: [[Int]]
      allSmallests = map getSmallests allNums

      sizePairs :: [([Int], [Int])]
      sizePairs = zip allBiggests allSmallests

      differences :: ([Int], [Int]) -> Int
      differences (xs, ys) = sum xs - sum ys

      differences' :: [String]
      differences' = map (show . differences) sizePairs
   in Data.List.intercalate "," differences'

showBiggest =
  let biggestInfo = sumBiggest [[1, 1, 2, 3, 4, 4], [1, 2, 5, 5], [-1, -2, 5, -10, 5]]
   in print $ "sumBiggest says: " <> biggestInfo
