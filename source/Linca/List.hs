module Linca.List where

import Data.List

replaceAt :: Integer -> a -> [a] -> [a]
replaceAt index item list =
	let
		intIndex = fromIntegral index
		head = take (intIndex - 1) list
		tail = drop intIndex list
	in head ++ [item] ++ tail
