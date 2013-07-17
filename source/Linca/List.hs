module Linca.List where

import Data.List

replaceAt :: Integer -> a -> [a] -> [a]
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

indices :: Integer -> [Integer]
indices count = [0 .. (count - 1)]
