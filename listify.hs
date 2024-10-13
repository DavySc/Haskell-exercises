module Listify where

listify :: Char -> String -> [String]
listify _ [] = []
listify separator text = takeWhile (/= separator) text : listify separator ((dropWhile (== separator) . dropWhile (/= separator)) text)
