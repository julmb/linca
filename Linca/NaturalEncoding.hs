module Linca.NaturalEncoding where

import Numeric.Natural
import Data.Word
import Data.Char
import Linca.Scalar

data Countable a = Finite Natural (a -> Natural) (Natural -> a) | Infinite (a -> Natural) (Natural -> a)

size :: Countable a -> Maybe Natural
size (Finite n _ _) = Just n
size (Infinite _ _) = Nothing

encode :: Countable a -> a -> Natural
encode (Finite _ e _) = e
encode (Infinite e _) = e

decode :: Countable a -> Natural -> a
decode (Finite _ _ d) = d
decode (Infinite _ d) = d


bool :: Countable Bool
bool = Finite 2 encode decode where
	encode b = if b then 0 else 1
	decode n = n == 0

byte :: Countable Word8
byte = Finite 0x100 fromIntegral fromIntegral

char :: Countable Char
char = Finite 0x5F encode decode where
	encode c = fromIntegral (ord c - 0x20)
	decode k = chr (fromIntegral k + 0x20)

natural :: Countable Natural
natural = Infinite id id

integer :: Countable Integer
integer = Infinite encodeInteger decodeInteger where
	encodeInteger i = if i < 0 then fromIntegral (abs i) * 2 - 1 else fromIntegral (abs i) * 2
	decodeInteger n = if even n then fromIntegral (n `div` 2) else - fromIntegral ((n + 1) `div` 2)


pair :: Countable a -> Countable b -> Countable (a, b)
pair (Finite size1 encode1 decode1) (Finite size2 encode2 decode2) = Finite (size1 * size2) encode decode where
	encode (x, y) = encode1 x + encode2 y * size1
	decode n = (decode1 (n `mod` size1), decode2 (n `div` size1))
pair (Finite size1 encode1 decode1) (Infinite encode2 decode2) = Infinite encode decode where
	encode (x, y) = encode1 x + encode2 y * size1
	decode n = (decode1 (n `mod` size1), decode2 (n `div` size1))
pair (Infinite encode1 decode1) (Finite size2 encode2 decode2) = Infinite encode decode where
	encode (x, y) = encode1 x * size2 + encode2 y
	decode n = (decode1 (n `div` size2), decode2 (n `mod` size2))
pair (Infinite encode1 decode1) (Infinite encode2 decode2) = Infinite encode decode where
	encode (x, y) = n (encode1 x) (encode2 y) where
		n x y = ((x + y) * (x + y + 1)) `div` 2 + y
	decode n = (decode1 x, decode2 y) where
		k = (squareRoot (8 * n + 1) - 1) `div` 2
		l = (k * (k + 1)) `div` 2
		y = n - l
		x = k - y

option :: Countable a -> Countable (Maybe a)
option a = case a of
	Finite n _ _ -> Finite (n + 1) encodeMaybe decodeMaybe
	Infinite _ _ -> Infinite encodeMaybe decodeMaybe
	where
		encodeMaybe Nothing = 0
		encodeMaybe (Just x) = encode a x + 1
		decodeMaybe 0 = Nothing
		decodeMaybe n = Just $ decode a (n - 1)

list :: Countable a -> Countable [a]
list a = Infinite encodeList decodeList where
	cons = pair a (list a)
	encodeList [] = 0
	encodeList (x : xs) = encode cons (x, xs) + 1
	decodeList 0 = []
	decodeList n = uncurry (:) $ decode cons (n - 1)
