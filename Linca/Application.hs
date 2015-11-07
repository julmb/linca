module Linca.Application (Application (Application), frameDuration, initialState, updateState, getDisplay, runApplication) where

import Control.Monad.State

import Linca.Timing

data Application state display = Application
	{
		frameDuration :: Double,
		initialState :: state,
		updateState :: Double -> state -> state,
		getDisplay :: state -> display
	}

loopApplication :: Application state display -> (display -> IO ()) -> Double -> StateT state IO ()
loopApplication application present lastTime = do
	currentTime <- lift time
	let passedDuration = currentTime - lastTime
	let waitDuration = frameDuration application - passedDuration
	lift (sleep waitDuration)
	currentTime <- lift time

	modify (updateState application (currentTime - lastTime))
	display <- gets (getDisplay application)
	lift (present display)

	loopApplication application present currentTime

runApplication :: Application state display -> (display -> IO ()) -> IO ()
runApplication application present = do
	initialTime <- time
	evalStateT (loopApplication application present initialTime) (initialState application)
