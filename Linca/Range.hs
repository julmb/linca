module Linca.Range (Range, range, start, end, unit, contains, outside, rangeError, size, midpoint, clamp, intermediateValues, intermediateValuesSymmetric) where

import Numeric.Natural
import Text.Printf
import Linca.Basic

data Range value = Range { start :: value, end :: value } deriving (Eq, Show, Read)

range :: Ord value => value -> value -> Range value
range start end
	| start > end = error $ printf "Linca.Range.range: parameter start was larger than parameter end"
	| otherwise = Range start end

unit :: (Ord value, Num value) => Range value
unit = range 0 1

contains :: Ord value => Range value -> value -> Bool
contains range value = value >= start range && value < end range

outside :: Ord value => Range value -> value -> Bool
outside range value = not $ contains range value

rangeError :: (Ord value, Show value) => String -> String -> Range value -> value -> t
rangeError location parameter range value
	| contains range value = error $ printf "Linca.Range.rangeError: parameter value (%s) was not outside of the allowed range (%s)" (show value) (show range)
	| otherwise = error $ printf "%s: parameter %s (%s) was outside of the allowed range (%s)" location parameter (show value) (show range)

size :: Num value => Range value -> value
size range = end range - start range

midpoint :: Fractional value => Range value -> value
midpoint range = (start range + end range) / 2

clamp :: Ord value => Range value -> value -> value
clamp range value
	| value < start range = start range
	| value > end range = end range
	| otherwise = value

intermediateValues :: RealFrac a => Range a -> Natural -> [a]
intermediateValues range count = do
	index <- indices count
	let position = fromIntegral index / fromIntegral count
	return (start range + position * size range)

intermediateValuesSymmetric :: RealFrac a => Range a -> Natural -> [a]
intermediateValuesSymmetric range count
	| count == 1 = [midpoint range]
	| otherwise = do
		index <- indices count
		let position = fromIntegral index / (fromIntegral count - 1)
		return (start range + position * size range)
