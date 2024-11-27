module Velocity where

type Meters = Double

type Seconds = Double

type MetersPerSecond = Double

velocity :: Meters -> Seconds -> MetersPerSecond
velocity meters seconds = meters / seconds

speedLimit :: MetersPerSecond
speedLimit =
  let meters = 299792458 :: Meters
      seconds = 1.0 :: Seconds
   in velocity meters seconds
