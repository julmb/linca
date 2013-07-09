module Linca.Lens where

import Control.Lens
import Control.Monad.State

infix 4 ..=

(..=) :: MonadState s m => ASetter' s s' -> State s' a -> m ()
property ..= updateProperty = property %= execState updateProperty

genericIx :: (Integral i, Num (Index m), Ixed f m) => i -> IndexedLensLike' (Index m) f m (IxValue m)
genericIx = ix . fromIntegral
