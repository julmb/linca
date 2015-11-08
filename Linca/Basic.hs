module Linca.Basic (enum, power, replaceAt, rotateLeft, rotateRight, indices, fold, crc16) where

import Numeric.Natural
import Data.Bits
import Data.List
import Data.Word

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

power :: (a -> a) -> Natural -> (a -> a)
power _ 0 = id
power f n = f . power f (n - 1)

replaceAt :: Natural -> a -> [a] -> [a]
replaceAt index item list
	| index < 0                      = error "Linca.List.replaceAt: negative index"
	| index > genericLength list - 1 = error "Linca.List.replaceAt: index too large"
	| otherwise = head ++ [item] ++ tail
	where
		head = genericTake index list
		tail = genericDrop (index + 1) list

rotateLeft :: [a] -> [a]
rotateLeft list = tail list ++ [head list]

rotateRight :: [a] -> [a]
rotateRight list = [last list] ++ init list

indices :: Natural -> [Natural]
indices count = [0 .. count - 1]

fold :: Foldable t => (x -> a -> a) -> t x -> a -> a
fold = flip . foldl . flip

crc16 :: Word8 -> Word16 -> Word16
crc16 byte = power step 8 . initial where
	initial value = fromIntegral byte `xor` value
	step value = if testBit value 0 then shiftR value 1 `xor` 0xA001 else shiftR value 1
