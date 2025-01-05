module Linca.Monad (foldMonad) where

import Control.Monad

foldMonad :: (Foldable t, Monad m) => (x -> a -> m a) -> (t x -> a -> m a)
foldMonad = flip . foldM . flip
