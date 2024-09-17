module Sudoku where

import Data.List

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

blank :: Grid
blank = replicate 9 (replicate 9 '.')

rows :: Matrix a -> [Row a]
rows = id

columns :: Matrix a -> [Row a]
columns = transpose

groupBy3 :: [a] -> [[a]]
groupBy3 [] = []
groupBy3 (a : b : c : ds) = [a, b, c] : groupBy3 ds

boxes :: Matrix a -> [Row a]
boxes = map concat . groupBy3 . concat . transpose . map groupBy3

valid :: Grid -> Bool
valid grid = all nodups (rows grid) && all nodups (columns grid) && all nodups (boxes grid)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

-- Basic solver
solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice v = if v == '.' then ['1' .. '9'] else [v]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy columns . pruneBy rows
  where
    pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce = undefined
