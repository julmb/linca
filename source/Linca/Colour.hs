module Linca.Colour where

import Data.Colour
import Data.Colour.SRGB
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Control.Monad.State
import System.Random

instance Random (Colour Double) where
	randomR (minimum, maximum) = runState $ do
		randomHue <- state (randomR (hue (toSRGB minimum), hue (toSRGB maximum)))
		randomSaturation <- state (randomR (saturation (toSRGB minimum), saturation (toSRGB maximum)))
		randomValue <- state (randomR (value (toSRGB minimum), value (toSRGB maximum)))
		return (uncurryRGB sRGB (hsv randomHue randomSaturation randomValue))
	random = randomR (uncurryRGB sRGB (hsv 0 1 1), uncurryRGB sRGB (hsv 359 1 1))

instance Random (AlphaColour Double) where
	randomR (minimum, maximum) = runState $ do
		randomAlpha <- state (randomR (alphaChannel minimum, alphaChannel maximum))
		randomColour <- state (randomR (minimum `over` black, maximum `over` black))
		return (withOpacity randomColour randomAlpha)
	random = randomR (withOpacity (uncurryRGB sRGB (hsv 0 1 1)) 0, withOpacity (uncurryRGB sRGB (hsv 359 1 1)) 1)
