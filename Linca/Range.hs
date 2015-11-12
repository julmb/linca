module Linca.Range
(
	Range, range, lower, upper,
	empty, contains, clamp, size,
	unitRange, fromRange, toRange
)
where

import Text.Printf
import Linca.Error

data Range value = Range { lower :: value, upper :: value } deriving (Eq, Show, Read)

range :: Ord value => value -> value -> Range value
range lower upper
	| lower > upper = error $ printf "range: parameter lower was larger than parameter upper"
	| otherwise = Range lower upper


empty :: Eq value => Range value -> Bool
empty range = lower range == upper range

contains :: Ord value => Range value -> value -> Bool
contains range value = value >= lower range && value <= upper range

clamp :: Ord value => Range value -> value -> value
clamp range value
	| value < lower range = lower range
	| value > upper range = upper range
	| otherwise = value

size :: Num value => Range value -> value
size range = upper range - lower range


unitRange :: (Ord value, Num value) => Range value
unitRange = range 0 1

fromRange :: (Real value, Fractional position) => Range value -> value -> position
fromRange range value
	| not $ contains range value = rangeError "fromRange" "value"
	| otherwise = realToFrac (value - lower range) / realToFrac (size range)

toRange :: (Real position, Fractional value) => Range value -> position -> value
toRange range position
	| not $ contains unitRange position = rangeError "toRange" "position"
	| otherwise = lower range + realToFrac position * size range