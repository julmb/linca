module Linca.Lens ((...=), genericIx) where

import Control.Lens
import Control.Monad.State

infix 4 ...=

(...=) :: MonadState s m => Lens' s s' -> State s' a -> m a
property ...= update = do
	oldState <- use property
	let (value, newState) = runState update oldState
	property .= newState
	return value

genericIx :: (Integral i, Num (Index m), Ixed f m) => i -> IndexedLensLike' (Index m) f m (IxValue m)
genericIx = ix . fromIntegral
