module Linca.Scalar (sine, divide, divideMidpoint, divideSymmetric, normalize, fromByte, toByte) where

import Numeric.Natural
import Data.Word
import Linca.Error
import Linca.Basic

sine :: Floating value => value -> value
sine value = sin (value * 2 * pi)

divide :: Fractional value => Natural -> [value]
divide count = do
	index <- indices count
	return $ fromIntegral (index :: Natural) / fromIntegral count

divideMidpoint :: Fractional value => Natural -> [value]
divideMidpoint count = do
	index <- indices count
	return $ (1 / 2 + fromIntegral (index :: Natural)) / fromIntegral count

divideSymmetric :: Fractional value => Natural -> [value]
divideSymmetric 1 = [1 / 2]
divideSymmetric count = do
	index <- indices count
	return $ fromIntegral (index :: Natural) / (fromIntegral count - 1)

normalize :: (Num value, Ord value, Num index) => value -> (index, value) -> (index, value)
normalize length (index, value)
	| value < 0       = normalize length (index - 1, value + length)
	| value >= length = normalize length (index + 1, value - length)
	| otherwise       = (index, value)

fromByte :: Fractional value => Word8 -> value
fromByte byte = fromIntegral byte / 0xFF

toByte :: Real value => value -> Word8
toByte value
	| value < 0 || value > 1 = rangeError "toByte" "value"
	| value == 1 = 0xFF
	| otherwise = truncate ((realToFrac value :: Rational) * 0x100)
