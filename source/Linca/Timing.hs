module Linca.Timing (delay, Application (Application), interval, initial, update, runApplication) where

import Data.Time
import Control.Concurrent
import Control.Monad.State

delay :: NominalDiffTime -> IO ()
delay duration = threadDelay (round (duration * 1000000))

data Application state =
    Application
    {
        interval :: NominalDiffTime,
        initial :: state,
        update :: NominalDiffTime -> StateT state IO ()
    }

loopApplication :: Application state -> UTCTime -> StateT state IO result
loopApplication application lastTime = do
    currentTime <- lift getCurrentTime
    lift $ delay (interval application - diffUTCTime currentTime lastTime)
    updateTime <- lift getCurrentTime
    update application (diffUTCTime updateTime lastTime)
    loopApplication application updateTime

runApplication :: Application state -> IO result
runApplication application = do
    time <- getCurrentTime
    evalStateT (loopApplication application time) (initial application)
