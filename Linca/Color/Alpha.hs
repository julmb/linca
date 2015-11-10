module Linca.Color.Alpha (AlphaColor, alphaColor, base, alpha, opaque) where

import Control.Monad.State
import System.Random
import Linca.Range
import Linca.Color.Color

data AlphaColor = AlphaColor { base :: Color, alpha :: Rational } deriving (Eq, Show, Read)

alphaColor :: Color -> Rational -> AlphaColor
alphaColor base alpha
	| not $ contains' unitRange alpha = rangeError "alphaColor" "alpha" unitRange alpha
	| otherwise = AlphaColor base alpha

opaque :: Color -> AlphaColor
opaque color = alphaColor color 1

instance Random AlphaColor where
	random = runState $ do
		base <- state $ random
		return $ opaque base
	randomR (minimum, maximum) = runState $ do
		base <- state $ randomR (base minimum, base maximum)
		alpha <- state $ randomR (alpha minimum, alpha maximum)
		return $ alphaColor base alpha
