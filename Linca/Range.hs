module Linca.Range (Range, range, start, end, unitRange, contains, contains', rangeError, rangeError', size, clamp, midpoint, interpolateLinear, intermediateValues, intermediateValues') where

import Numeric.Natural
import Data.Ratio
import Text.Printf
import Linca.List

data Range value = Range { start :: value, end :: value } deriving (Eq, Show, Read)

range :: Ord value => value -> value -> Range value
range start end
	| start > end = error $ printf "range: parameter start was larger than parameter end"
	| otherwise = Range start end

unitRange :: (Ord value, Num value) => Range value
unitRange = range 0 1

contains :: Ord value => Range value -> value -> Bool
contains range value = value >= start range && value < end range

contains' :: Ord value => Range value -> value -> Bool
contains' range value = value >= start range && value <= end range

rangeError :: Show value => String -> String -> Range value -> value -> t
rangeError location parameter range value = error $ printf "%s: parameter %s (%s) was outside of the allowed range (%s)" location parameter (show value) (show range)

rangeError' :: String -> String -> t
rangeError' location parameter = error $ printf "%s: parameter %s was outside of the allowed range" location parameter

size :: Num value => Range value -> value
size range = end range - start range

clamp :: Ord value => Range value -> value -> value
clamp range value
	| value < start range = start range
	| value > end range = end range
	| otherwise = value

midpoint :: Fractional value => Range value -> value
midpoint range = (start range + end range) / 2

interpolateLinear :: (Fractional value, Real position) => Range value -> position -> value
interpolateLinear range position
	| not $ contains' unitRange position = rangeError' "interpolateLinear" "position"
	| otherwise = start range + realToFrac position * size range

intermediateValues_ :: Fractional value => Range value -> Natural -> Natural -> [value]
intermediateValues_ range length count = do
	index <- indices count
	return $ interpolateLinear range (index % length)

intermediateValues :: Fractional value => Range value -> Natural -> [value]
intermediateValues range count = intermediateValues_ range count count

intermediateValues' :: Fractional value => Range value -> Natural -> [value]
intermediateValues' range count
	| count == 1 = [midpoint range]
	| otherwise = intermediateValues_ range count (count - 1)
