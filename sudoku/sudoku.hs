module Main where

import Data.List (transpose, (\\))

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

single :: [a] -> Bool
single [_] = True
single _ = False

blank :: Grid
blank = replicate 9 (replicate 9 '.')

easy :: Grid
easy = ["2....1.38", "........5", ".7...6...", ".......13", ".981..257", "31....8..", "9..8...2.", ".5..69784", "4..25...."]

rows :: Matrix a -> [Row a]
rows = id

columns :: Matrix a -> [Row a]
columns = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map columns . pack
 where
  pack = split . map split
  split = chop 3
  unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid grid = all nodups (rows grid) && all nodups (columns grid) && all nodups (boxes grid)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

-- Basic solver
solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- Improvements

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
 where
  choice v = if v == '.' then ['1' .. '9'] else [v]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy columns . pruneBy rows
 where
  pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `listMinus` singles | xs <- xss]
 where
  singles = concat (filter single xss)

listMinus :: Choices -> Choices -> Choices
listMinus xs ys = if single xs then xs else xs \\ ys

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

solve3 :: Grid -> [Grid]
solve3 = filter valid . collapse . fix prune . choices

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
 where
  x' = f x

-- efficient solver

-- Test
main :: IO ()
main = putStrLn (unlines (head (solve3 easy)))
