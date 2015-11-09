module Linca.Color.Color (Color, rgb, hsv, red, green, blue, hue, saturation, value) where

import Numeric.Natural
import Control.Monad.State
import System.Random
import Linca.Scalar

data Color = RGB Double Double Double | HSV Double Double Double deriving (Eq, Show, Read)

rgb :: Double -> Double -> Double -> Color
rgb red green blue
	| red   < 0 || red   > 1 = error "Linca.Color.Color.rgb: parameter red was outside of the allowed range"
	| green < 0 || green > 1 = error "Linca.Color.Color.rgb: parameter green was outside of the allowed range"
	| blue  < 0 || blue  > 1 = error "Linca.Color.Color.rgb: parameter blue was outside of the allowed range"
	| otherwise = RGB red green blue

hsv :: Double -> Double -> Double -> Color
hsv hue saturation value
	| hue        < 0 || hue        >= 6 = error "Linca.Color.Color.hsv: parameter hue was outside of the allowed range"
	| saturation < 0 || saturation >  1 = error "Linca.Color.Color.hsv: parameter saturation was outside of the allowed range"
	| value      < 0 || value      >  1 = error "Linca.Color.Color.hsv: parameter value was outside of the allowed range"
	| otherwise = HSV hue saturation value

red :: Color -> Double
red (RGB r _ _) = r
red (HSV h s v) = red (toRGB (HSV h s v))

green :: Color -> Double
green (RGB _ g _) = g
green (HSV h s v) = green (toRGB (HSV h s v))

blue :: Color -> Double
blue (RGB _ _ b) = b
blue (HSV h s v) = blue (toRGB (HSV h s v))

hue :: Color -> Double
hue (RGB r g b) = hue (toHSV (RGB r g b))
hue (HSV h _ _) = h

saturation :: Color -> Double
saturation (RGB r g b) = saturation (toHSV (RGB r g b))
saturation (HSV _ s _) = s

value :: Color -> Double
value (RGB r g b) = value (toHSV (RGB r g b))
value (HSV _ _ v) = v

toRGB :: Color -> Color
toRGB (RGB red green blue) = RGB red green blue
toRGB (HSV hue saturation value)
	| hueIndex == 0 = RGB top rising bottom
	| hueIndex == 1 = RGB falling top bottom
	| hueIndex == 2 = RGB bottom top rising
	| hueIndex == 3 = RGB bottom falling top
	| hueIndex == 4 = RGB rising bottom top
	| hueIndex == 5 = RGB top bottom falling
    | otherwise = undefined
	where
		(hueIndex, hueFraction) = normalize 1 (0 :: Natural, hue)
		top = value
		bottom = unitReverse saturation * value
		rising = unitReverse (unitReverse hueFraction * saturation) * value
		falling = unitReverse (unitForward hueFraction * saturation) * value

toHSV :: Color -> Color
toHSV (RGB red green blue)
	| chroma == 0 = HSV 0 0 value
	| otherwise   = HSV hue saturation value
	where
		chroma = value - minimum [red, green, blue]
		hueRaw
			| value == red   = (green - blue) / chroma + 0
			| value == green = (blue - red)   / chroma + 2
			| value == blue  = (red - green)  / chroma + 4
			| otherwise = undefined
		(_, hue) = normalize 6 (0 :: Integer, hueRaw)
		saturation = chroma / value
		value = maximum [red, green, blue]
toHSV (HSV hue saturation value) = HSV hue saturation value

instance Random Color where
	randomR (minimum, maximum) = runState $ do
		randomHue <- state (randomR (hue minimum, hue maximum))
		randomSaturation <- state (randomR (saturation minimum, saturation maximum))
		randomValue <- state (randomR (value minimum, value maximum))
		return (hsv randomHue randomSaturation randomValue)
	random = randomR (hsv 0 1 1, hsv 5.99999 1 1)
