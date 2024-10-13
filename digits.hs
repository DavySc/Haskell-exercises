module Digits where

import Data.Text.Lazy.Read (double)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryLeft :: [Integer] -> [Integer]
doubleEveryLeft [] = []
doubleEveryLeft [x] = [x]
doubleEveryLeft (x : y : xs) = x : 2 * y : doubleEveryLeft xs

doubleEveryOther = reverse . doubleEveryLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldr (+) 0

entriesToDigits :: [Integer] -> [Integer]
entriesToDigits x = x >>= toDigits

prepareNumber :: Integer -> Integer
prepareNumber = sumDigits . entriesToDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate x = prepareNumber x `mod` 10 == 10
