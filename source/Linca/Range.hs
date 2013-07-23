module Linca.Range (Range (Range), start, end, unit, contains, Linca.Range.length, midpoint) where

data Range value = Range { start :: value, end :: value } deriving (Eq, Show, Read)

unit :: Num a => Range a
unit = Range 0 1

contains :: Ord value => value -> Range value -> Bool
contains value range
	| start range > end range = error "Linca.Range.contains: parameter range is not a well-formed ordered range"
	| otherwise = value >= start range && value <= end range

length :: Real value => Range value -> value
length range
	| start range > end range = error "Linca.Range.length: parameter range is not a well-formed ordered range"
	| otherwise = end range - start range

midpoint :: RealFrac value => Range value -> value
midpoint range
	| start range > end range = error "Linca.Range.midpoint: parameter range is not a well-formed ordered range"
	| otherwise = (start range + end range) / 2
