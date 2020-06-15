module Linca.NaturalEncoding where

import Numeric.Natural
import Linca.Scalar

class Countable a where
	encode :: a -> Natural
	decode :: Natural -> a

instance Countable Natural where
	encode = id
	decode = id
instance (Countable a, Countable b) => Countable (a, b) where
	encode (x, y) = ((nx + ny) * (nx + ny + 1)) `div` 2 + ny where
		nx = encode x
		ny = encode y
	decode n = (decode x, decode y) where
		u = squareRoot (8 * n + 1)
		v = if even u then u - 2 else u - 1
		w = v `div` 2
		t = (square w + w) `div` 2
		y = n - t
		x = w - y
instance Countable a => Countable [a] where
	encode [] = 0
	encode (x : xs) = encode (x, xs) + 1
	decode 0 = []
	decode n = x : xs where
		(x, xs) = (decode :: Countable a => Natural -> (a, [a])) (n - 1)
