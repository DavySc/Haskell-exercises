module Nim where

import Data.Char

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num
  | row > 5 || row < 1 = False
  | otherwise = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [adjust r n | (r, n) <- zip [1 .. 5] board]
  where
    adjust r n = if r == row then n - num else n

printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn [concat (replicate x "* ") | x <- board]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  putChar '\n'
  if isDigit x
    then return (digitToInt x)
    else do
      putChar '\n'
      putStrLn "Error: Invalid digit!"
      getDigit prompt

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board currentPlayer = do
  putChar '\n'
  printBoard board
  if finished board
    then do
      putChar '\n'
      putStr "Player: "
      putStr (show (next currentPlayer))
      putStrLn " has won!"
    else do
      putChar '\n'
      putStr "Player "
      print currentPlayer
      row <- getDigit "Indicate which row to remove from: "
      stars <- getDigit "Indicate how many *s to remove: "
      if valid board row stars
        then do
          play (move board row stars) (next currentPlayer)
        else do
          putStrLn "Enter a row number between 1 and 5"
          play board currentPlayer

nim :: IO ()
nim = play initial 1
