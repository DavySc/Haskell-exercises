module Parser where

import Control.Applicative (Alternative (..))
import Data.List (nub)
import Distribution.Compat.CharParsing (CharParsing (satisfy))

data Error i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i e a = Parser
  {runParser :: [i] -> Either [Error i e] (a, [i])}

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left [Unexpected hd]

char :: (Eq i) => i -> Parser i e i
char i = Parser.satisfy (== i)
