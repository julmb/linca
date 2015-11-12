module Linca.Color.Color (Color, rgb, hsv, red, green, blue, hue, saturation, value) where

import Numeric.Natural
import Linca.Error
import Linca.Scalar
import Linca.Map

data Color = RGB Rational Rational Rational | HSV Rational Rational Rational deriving (Eq, Show, Read)

rgb :: Rational -> Rational -> Rational -> Color
rgb red green blue
	| red   < 0 || red   > 1 = rangeError "rgb" "red"
	| green < 0 || green > 1 = rangeError "rgb" "green"
	| blue  < 0 || blue  > 1 = rangeError "rgb" "blue"
	| otherwise = RGB red green blue

hsv :: Rational -> Rational -> Rational -> Color
hsv hue saturation value
	| hue        < 0 || hue        >= 6 = rangeError "hsv" "hue"
	| saturation < 0 || saturation >  1 = rangeError "hsv" "saturation"
	| value      < 0 || value      >  1 = rangeError "hsv" "value"
	| otherwise = HSV hue saturation value

red :: Color -> Rational
red (RGB r _ _) = r
red (HSV h s v) = red (toRGB (HSV h s v))

green :: Color -> Rational
green (RGB _ g _) = g
green (HSV h s v) = green (toRGB (HSV h s v))

blue :: Color -> Rational
blue (RGB _ _ b) = b
blue (HSV h s v) = blue (toRGB (HSV h s v))

hue :: Color -> Rational
hue (RGB r g b) = hue (toHSV (RGB r g b))
hue (HSV h _ _) = h

saturation :: Color -> Rational
saturation (RGB r g b) = saturation (toHSV (RGB r g b))
saturation (HSV _ s _) = s

value :: Color -> Rational
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
		top     = value
		bottom  = reverseMap saturation * value
		rising  = reverseMap (reverseMap hueFraction * saturation) * value
		falling = reverseMap (forwardMap hueFraction * saturation) * value

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
