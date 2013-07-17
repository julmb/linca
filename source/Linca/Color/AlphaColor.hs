module Linca.Color.AlphaColor (AlphaColor, alphaColor, opaque, alpha) where

import Control.Monad.State
import System.Random

import Linca.Color.Color

data AlphaColor = AlphaColor { opaque :: Color, alpha :: Double } deriving (Eq, Show, Read)

alphaColor :: Color -> Double -> AlphaColor
alphaColor = AlphaColor

instance Random AlphaColor where
	randomR (minimum, maximum) = runState $ do
		randomOpaque <- state (randomR (opaque minimum, opaque maximum))
		randomAlpha <- state (randomR (alpha minimum, alpha maximum))
		return (alphaColor randomOpaque randomAlpha)
	random = randomR (alphaColor (hsv 0 1 1) 1, alphaColor (hsv 5.99999 1 1) 1)
