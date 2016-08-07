module Linca.Scalar (clamp, sine, divide, divideMidpoint, divideSymmetric, normalize, fromRange, toRange, fromByte, toByte) where

import Numeric.Natural
import Data.Word
import Linca.Error
import Linca.Basic
import Linca.Range

clamp :: Ord value => value -> value -> value -> value
clamp lower upper value
	| lower > upper = localError "clamp" "parameter lower was larger than parameter upper"
	| value < lower = lower
	| value > upper = upper
	| otherwise = value

sine :: Double -> Double
sine value = sin (value * 2 * pi)

divide :: Natural -> [Rational]
divide count = do
	index <- indices count
	return $ fromIntegral index / fromIntegral count

divideMidpoint :: Natural -> [Rational]
divideMidpoint count = do
	index <- indices count
	return $ (1 / 2 + fromIntegral index) / fromIntegral count

divideSymmetric :: Natural -> [Rational]
divideSymmetric 1 = [1 / 2]
divideSymmetric count = do
	index <- indices count
	return $ fromIntegral index / (fromIntegral count - 1)

normalize :: (Integral index, Real value) => value -> (index, value) -> (index, value)
normalize length (index, value)
	| value < 0       = normalize length (index - 1, value + length)
	| value >= length = normalize length (index + 1, value - length)
	| otherwise       = (index, value)

fromRange :: (Real value, Fractional position) => value -> value -> value -> position
fromRange lower upper value
	| lower >= upper = localError "fromRange" "parameter lower was larger than or equal to parameter upper"
	| violates (rangeII lower upper) value = rangeError' "fromRange" "value"
	| otherwise = realToFrac (value - lower) / realToFrac (upper - lower)

toRange :: (Ord value, Real position, Fractional value) => value -> value -> position -> value
toRange lower upper position
	| lower > upper = localError "toRange" "parameter lower was larger than parameter upper"
	| violates unitRange position = rangeError' "toRange" "position"
	| otherwise = lower + realToFrac position * (upper - lower)

fromByte :: Word8 -> Rational
fromByte byte = fromIntegral byte / 0xFF

toByte :: Rational -> Word8
toByte value
	| violates unitRange value = rangeError "toByte" "value" unitRange value
	| value == 1 = 0xFF
	| otherwise = truncate (value * 0x100)
