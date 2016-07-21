module Linca.Monad (foldMonad, runT, run) where

import Control.Monad.Cont

foldMonad :: (Foldable t, Monad m) => (x -> a -> m a) -> (t x -> a -> m a)
foldMonad = flip . foldM . flip

runT :: Monad m => ContT r m r -> m r
runT c = runContT c return

run :: Cont r r -> r
run c = runCont c id
