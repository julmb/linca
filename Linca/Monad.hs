module Linca.Monad (foldMonad) where

import Linca.List

foldMonad :: (Foldable t, Monad m) => (x -> a -> m a) -> t x -> a -> m a
foldMonad f xs a = fold (\x a -> a >>= f x) xs (return a)
