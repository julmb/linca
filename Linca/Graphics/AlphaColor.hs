module Linca.Graphics.AlphaColor (AlphaColor, alphaColor, base, alpha, opaque) where

import Linca.Error
import Linca.Graphics.Color

data AlphaColor = AlphaColor { base :: Color, alpha :: Rational } deriving (Eq, Show, Read)

alphaColor :: Color -> Rational -> AlphaColor
alphaColor base alpha
	| alpha < 0 || alpha > 1 = rangeError "alphaColor" "alpha"
	| otherwise = AlphaColor base alpha

opaque :: Color -> AlphaColor
opaque color = alphaColor color 1
