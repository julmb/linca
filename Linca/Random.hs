{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linca.Random () where

import Data.Ratio
import Control.Monad.State
import System.Random
import Linca.Scalar

instance (Random t, Integral t) => Random (Ratio t) where
	random = runState $ do
		let denominator = 1000000
		numerator <- state $ randomR (0, denominator)
		return $ numerator % denominator
	randomR (minimum, maximum) = runState $ do
		fraction <- state random
		return $ toRange minimum maximum (fraction :: Rational)
