module Linca.Scalars (unitForward, unitReverse, fraction, normalizeCircular, intermediateValues, intermediateValuesSymmetric) where

import Linca.List
import Linca.Range as Range

unitForward :: Num a => a -> a
unitForward value = value

unitReverse :: Num a => a -> a
unitReverse value = 1 - value

fraction :: RealFrac a => a -> a
fraction = snd . properFraction

normalizeCircular :: RealFrac a => a -> a -> a
normalizeCircular length value
	| length <= 0 = error "Linca.Scalars.normalizeCircular: parameter length was less than or equal to zero"
	| otherwise   = periods * length
	where
		fractionalPeriods = fraction (value / length)
		periods = if fractionalPeriods < 0 then fractionalPeriods + 1 else fractionalPeriods

intermediateValues :: RealFrac a => Range a -> Integer -> [a]
intermediateValues range count = do
	index <- indices count
	let position = fromIntegral index / fromIntegral count
	return ((start range) + position * (Range.length range))

intermediateValuesSymmetric :: RealFrac a => Range a -> Integer -> [a]
intermediateValuesSymmetric range count
	| count == 1 = [midpoint range]
	| otherwise = do
		index <- indices count
		let position = fromIntegral index / (fromIntegral count - 1)
		return ((start range) + position * (Range.length range))
