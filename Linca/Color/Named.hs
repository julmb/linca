module Linca.Color.Named where

import Linca.Color

black :: Color
black = hsv 0 0 0

white :: Color
white = hsv 0 0 1

red :: Color
red = hsv 0 1 1

yellow :: Color
yellow = hsv 1 1 1

green :: Color
green = hsv 2 1 1

cyan :: Color
cyan = hsv 3 1 1

blue :: Color
blue = hsv 4 1 1

magenta :: Color
magenta = hsv 5 1 1

transparent :: AlphaColor
transparent = alphaColor (hsv 0 0 0) 0
