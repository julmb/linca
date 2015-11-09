module Linca.Basic (empty, update, enum, power, retrieve, replaceAt, rotateLeft, rotateRight, indices, fold, foldMonad, crc16) where

import Numeric.Natural
import Data.Maybe
import Data.Bits
import Data.List
import Data.Word

empty :: a -> b
empty _ = undefined

update :: Eq a => a -> b -> (a -> b) -> (a -> b)
update a b f x
	| x == a = b
	| otherwise = f x

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

power :: (a -> a) -> Natural -> (a -> a)
power _ 0 = id
power f n = f . power f (n - 1)

retrieve :: Eq a => [(a, b)] -> a -> b
retrieve table value = fromJust (lookup value table)

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

foldMonad :: (Foldable t, Monad m) => (x -> a -> m a) -> t x -> a -> m a
foldMonad f xs a = fold (\x a -> a >>= f x) xs (return a)

crc16 :: Word8 -> Word16 -> Word16
crc16 byte = power step 8 . initial where
	initial value = fromIntegral byte `xor` value
	step value = if testBit value 0 then shiftR value 1 `xor` 0xA001 else shiftR value 1
