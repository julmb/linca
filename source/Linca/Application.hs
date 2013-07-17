module Linca.Application where

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
	currentTime <- lift getTime
	let passedDuration = currentTime - lastTime
	let waitDuration = frameDuration application - passedDuration
	lift (delay waitDuration)
	currentTime <- lift getTime

	modify (updateState application (currentTime - lastTime))
	display <- gets (getDisplay application)
	lift (present display)

	loopApplication application present currentTime

runApplication :: Application state display -> (display -> IO ()) -> IO ()
runApplication application present = do
	initialTime <- getTime
	execStateT (loopApplication application present initialTime) (initialState application)
	return ()
