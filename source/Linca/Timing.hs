module Linca.Timing (Linca.Timing.time, sleep) where

import Criterion.Measurement
import Control.Concurrent.Thread.Delay

time :: IO Double
time = Criterion.Measurement.getTime

sleep :: Double -> IO ()
sleep duration = Control.Concurrent.Thread.Delay.delay (truncate (duration * 1000000))
