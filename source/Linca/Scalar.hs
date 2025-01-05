module Linca.Scalar (clamp, square, squareRoot, sine, divide, divideMidpoint, divideSymmetric, normalize, fromRange, toRange, fromByte, toByte) where

import Numeric.Natural
import Data.Maybe
import Data.List
import Data.Word
import Linca.Error
import Linca.Range
import Linca.List

clamp :: Ord value => value -> value -> value -> value
clamp lower upper value
	| lower > upper = error $ errorMessage "clamp" "parameter lower was larger than parameter upper"
	| value < lower = lower
	| value > upper = upper
	| otherwise = value

square :: Natural -> Natural
square n = n * n

squareRoot :: Natural -> Natural
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n = fromJust $ find test $ iterate step initial where
	test k = square k <= n && n < square (k + 1)
	[k, l] = suffix 2 $ takeWhile (n >=) (1 : iterate square 2)
	initial = squareRoot (n `div` l) * k
	step k = (k + n `div` k) `div` 2

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
	| violates (rangeEN 0) length = error $ rangeErrorMessage' "normalize" "length"
	| value < 0                   = normalize length (index - 1, value + length)
	| value >= length             = normalize length (index + 1, value - length)
	| otherwise                   = (index, value)

fromRange :: (Real value, Fractional position) => value -> value -> value -> position
fromRange lower upper value
	| lower >= upper = error $ errorMessage "fromRange" "parameter lower was larger than or equal to parameter upper"
	| violates (rangeII lower upper) value = error $ rangeErrorMessage' "fromRange" "value"
	| otherwise = realToFrac (value - lower) / realToFrac (upper - lower)

toRange :: (Ord value, Real position, Fractional value) => value -> value -> position -> value
toRange lower upper position
	| lower > upper = error $ errorMessage "toRange" "parameter lower was larger than parameter upper"
	| violates unitRange position = error $ rangeErrorMessage' "toRange" "position"
	| otherwise = lower + realToFrac position * (upper - lower)

fromByte :: Word8 -> Rational
fromByte byte = fromIntegral byte / 0xFF

toByte :: Rational -> Word8
toByte value
	| violates unitRange value = error $ rangeErrorMessage "toByte" "value" unitRange value
	| value == 1 = 0xFF
	| otherwise = truncate (value * 0x100)
