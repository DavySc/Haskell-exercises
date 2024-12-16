module Main where

import Data.Char
import Data.Text as T

countNonPrintableChars :: String -> Int
countNonPrintableChars = Prelude.length . Prelude.filter (not . isPrint)

countNonPrintableCharsInText :: Text -> Int
countNonPrintableCharsInText = T.length . T.filter (not . isPrint)

main :: IO ()
main = print $ countNonPrintableChars "\v\t\aHello, Haskell!\r\n"
