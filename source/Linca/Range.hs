module Linca.Range (Range, range, start, end, unit, contains, Linca.Range.length, midpoint, clamp) where

data Range value = Range { start :: value, end :: value } deriving (Eq, Show, Read)

range :: Ord value => value -> value -> Range value
range start end
	| start > end = error "Linca.Range.range: parameter start was larger than parameter end"
	| otherwise = Range start end

unit :: (Ord value, Num value) => Range value
unit = range 0 1

contains :: Ord value => value -> Range value -> Bool
contains value range = value >= start range && value <= end range

length :: Real value => Range value -> value
length range = end range - start range

midpoint :: RealFrac value => Range value -> value
midpoint range = (start range + end range) / 2

clamp :: Ord value => Range value -> value -> value
clamp range value
	| value < start range = start range
	| value > end range = end range
	| otherwise = value
