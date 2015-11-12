module Linca.Map (forwardMap, reverseMap, sineMap, powerMap) where

import Linca.Scalar
import Linca.Range

forwardMap :: Num value => value -> value
forwardMap value = value

reverseMap :: Num value => value -> value
reverseMap value = 1 - value

sineMap :: RealFrac value => value -> value
sineMap = fromRange output . (sine :: Double -> Double) . toRange input where
	input = range (3 / 4) (5 / 4)
	output = range (0 - 1) (0 + 1)

powerMap :: (Real exponent, RealFrac value) => exponent -> value -> value
powerMap exponent value = realToFrac (val ** exp) where
	exp = realToFrac exponent :: Double
	val = realToFrac value :: Double
