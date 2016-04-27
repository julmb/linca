module Linca.Timing (diffLocalTime, delay, Application (Application), interval, initial, update, runApplication) where

import Data.Time
import Control.Concurrent
import Control.Monad.State

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime time1 time2 = diffUTCTime (localTimeToUTC utc time1) (localTimeToUTC utc time2)

delay :: NominalDiffTime -> IO ()
delay duration = threadDelay (round (duration * 1000000))

data Application state =
	Application
	{
		interval :: NominalDiffTime,
		initial :: state,
		update :: NominalDiffTime -> state -> IO state
	}

loopApplication :: Application state -> UTCTime -> StateT state IO ()
loopApplication application lastTime = do
	currentTime <- lift getCurrentTime
	lift $ delay $ interval application - diffUTCTime currentTime lastTime
	currentTime <- lift getCurrentTime
	let interval = diffUTCTime currentTime lastTime
	get >>= lift . update application interval >>= put
	loopApplication application currentTime

runApplication :: Application state -> IO ()
runApplication application = do
	time <- getCurrentTime
	evalStateT (loopApplication application time) (initial application)
