module Linca.Basic (indices, equal, power) where

import Numeric.Natural
import Linca.Error

indices :: Natural -> [Natural]
indices count = [0 .. count - 1]

equal :: Eq a => a -> a -> a
equal a b
	| a /= b = error $ errorMessage "equal" "parameters a and b were not equal"
	| otherwise = a

power :: Natural -> (a -> a) -> (a -> a)
power 0 _ = id
power n f = f . power (n - 1) f
