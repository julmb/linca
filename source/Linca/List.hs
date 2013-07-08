module Linca.List where

import Data.List

replaceAt :: Integer -> a -> [a] -> [a]
replaceAt index item list | index < 0                      = error "Linca.List.replaceAt: negative index"
replaceAt index item list | index > genericLength list - 1 = error "Linca.List.replaceAt: index too large"
replaceAt index item list =
	let
		head = genericTake index list
		tail = genericDrop (index + 1) list
	in head ++ [item] ++ tail
