module Linca.Timing (delay, Application (Application), frameDuration, initialState, updateState, present, runApplication) where

import Data.Time
import Control.Concurrent
import Control.Monad.State

delay :: NominalDiffTime -> IO ()
delay duration = threadDelay (round (duration * 1000000))

data Application state =
	Application
	{
		frameDuration :: NominalDiffTime,
		initialState :: state,
		updateState :: NominalDiffTime -> state -> state,
		present :: state -> IO ()
	}

loopApplication :: Application state -> UTCTime -> StateT state IO ()
loopApplication application lastTime = do
	currentTime <- lift getCurrentTime
	lift $ delay $ frameDuration application - diffUTCTime currentTime lastTime
	currentTime <- lift getCurrentTime
	state <- get
	let newState = updateState application (diffUTCTime currentTime lastTime) state
	lift $ present application newState
	put newState
	loopApplication application currentTime

runApplication :: Application state -> IO ()
runApplication application = do
	initialTime <- getCurrentTime
	evalStateT (loopApplication application initialTime) (initialState application)
