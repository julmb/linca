module Linca.Color.Alpha (AlphaColor, alphaColor, base, alpha, opaque) where

import Control.Monad.State
import System.Random
import Linca.Color.Color

data AlphaColor = AlphaColor { base :: Color, alpha :: Double } deriving (Eq, Show, Read)

alphaColor :: Color -> Double -> AlphaColor
alphaColor base alpha
	| alpha < 0 || alpha > 1 = error "Linca.Color.AlphaColor.alphaColor: parameter alpha was outside of the allowed range"
	| otherwise = AlphaColor base alpha

opaque :: Color -> AlphaColor
opaque color = alphaColor color 1

instance Random AlphaColor where
	randomR (minimum, maximum) = runState $ do
		randomBase <- state (randomR (base minimum, base maximum))
		randomAlpha <- state (randomR (alpha minimum, alpha maximum))
		return (alphaColor randomBase randomAlpha)
	random = randomR (alphaColor (hsv 0 1 1) 1, alphaColor (hsv 5.99999 1 1) 1)
