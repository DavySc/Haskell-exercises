module Velocity where

velocity :: Double -> Double -> Double
velocity meters seconds = meters / seconds

speedLimit :: Double
speedLimit =
  let meters = 299792458 :: Double
      seconds = 1.0 :: Double
   in velocity meters seconds
