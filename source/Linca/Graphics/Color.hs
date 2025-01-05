module Linca.Graphics.Color (Color, rgb, hsv, red, green, blue, hue, saturation, value) where

import Numeric.Natural
import Linca.Scalar
import Linca.Range

hexaRange :: Range Rational
hexaRange = rangeIE 0 6

data Color = RGB Rational Rational Rational | HSV Rational Rational Rational deriving (Eq, Show, Read)

rgb :: Rational -> Rational -> Rational -> Color
rgb red green blue
    | violates unitRange red   = error $ rangeErrorMessage "rgb" "red"   unitRange red
    | violates unitRange green = error $ rangeErrorMessage "rgb" "green" unitRange green
    | violates unitRange blue  = error $ rangeErrorMessage "rgb" "blue"  unitRange blue
    | otherwise = RGB red green blue

hsv :: Rational -> Rational -> Rational -> Color
hsv hue saturation value
    | violates hexaRange hue        = error $ rangeErrorMessage "hsv" "hue"        hexaRange hue
    | violates unitRange saturation = error $ rangeErrorMessage "hsv" "saturation" unitRange saturation
    | violates unitRange value      = error $ rangeErrorMessage "hsv" "value"      unitRange value
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
        result position = (1 - (1 - position) * saturation) * value
        bottom  = result 0
        top     = result 1
        rising  = result hueFraction
        falling = result (1 - hueFraction)

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
