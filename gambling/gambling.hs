module Gambling where

data Coin = H | T

data Dice = D1 | D2 | D3 | D4 | D5 | D6

data Outcome = Win | Lose

class (Monad m) => MonadGamble m where
  toss :: m Coin
  roll :: m Dice

game :: (MonadGamble m) => m Outcome
game = undefined
