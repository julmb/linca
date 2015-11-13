module Linca.Scalar (sine, divide, divideMidpoint, divideSymmetric, normalize, fromByte, toByte) where

import Numeric.Natural
import Data.Word
import Linca.Error
import Linca.Basic

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

fromByte :: Word8 -> Rational
fromByte byte = fromIntegral byte / 0xFF

toByte :: Rational -> Word8
toByte value
	| value < 0 || value > 1 = rangeError "toByte" "value"
	| value == 1 = 0xFF
	| otherwise = truncate (value * 0x100)
