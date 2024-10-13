module RadsToDegrees where

radsToDegrees :: Float -> Int
radsToDegrees radians =
  let degrees = cycle [0 .. 359]
      converted = truncate $ (radians * 360) / (2 * pi)
   in degrees !! converted

epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x : xs) = x : cycleHelper xs
