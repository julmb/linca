module Linca.Scalar (unitForward, unitReverse, normalize) where

unitForward :: Num a => a -> a
unitForward value = value

unitReverse :: Num a => a -> a
unitReverse value = 1 - value

normalize :: (Num value, Ord value, Num index) => value -> (index, value) -> (index, value)
normalize length (index, value)
	| value < 0       = normalize length (index - 1, value + length)
	| value >= length = normalize length (index + 1, value - length)
	| otherwise       = (index, value)
