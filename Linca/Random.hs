{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linca.Random (randomR') where

import Data.Ratio
import Control.Monad.State
import System.Random
import Linca.Range

randomR' :: (RandomGen g, Random a, Eq a) => (a, a) -> g -> (a, g)
randomR' (minimum, maximum) = runState $ do
	value <- state $ randomR (minimum, maximum)
	if value /= maximum
	then return value
	else state $ randomR' (minimum, maximum)

instance (Random t, Integral t) => Random (Ratio t) where
	random = runState $ do
		let denominator = 1000000
		numerator <- state $ randomR (0, denominator)
		return $ numerator % denominator
	randomR (minimum, maximum) = runState $ do
		fraction <- state random
		return $ interpolateLinear (range minimum maximum) (fraction :: Rational)
