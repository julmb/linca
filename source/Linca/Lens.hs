module Linca.Lens where

import Control.Lens
import Control.Monad.State

infix 4 ..=

(..=) :: MonadState s m => ASetter' s s' -> State s' a -> m ()
property ..= updateProperty = property %= execState updateProperty
