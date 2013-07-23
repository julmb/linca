module Linca.Scalars (unitForward, unitReverse, fraction, normalizeCircular) where

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
