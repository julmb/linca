module Linca.Map (sineMap, powerMap) where

import Linca.Scalar

sineMap :: RealFrac value => value -> value
sineMap = fromRange (0 - 1) (0 + 1) . sine . toRange (3 / 4) (5 / 4)

powerMap :: (Real exponent, RealFrac value) => exponent -> value -> value
powerMap exponent value = realToFrac (val ** exp) where
	exp = realToFrac exponent :: Double
	val = realToFrac value :: Double
