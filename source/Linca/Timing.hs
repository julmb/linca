module Linca.Timing where

import Criterion.Measurement
import Control.Concurrent.Thread.Delay

getTime :: IO Double
getTime = Criterion.Measurement.getTime

delay :: Double -> IO ()
delay duration = Control.Concurrent.Thread.Delay.delay (truncate (duration * 1000000))
