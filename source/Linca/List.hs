module Linca.List where

import Data.List

replaceAt :: Integer -> a -> [a] -> [a]
replaceAt index item list =
	let
		head = genericTake (index - 1) list
		tail = genericDrop index list
	in head ++ [item] ++ tail
