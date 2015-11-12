module Linca.List (enum, retrieve, replaceAt, rotateLeft, rotateRight, fold) where

import Numeric.Natural
import Data.Maybe
import Data.List
import Text.Printf

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

retrieve :: Eq a => [(a, b)] -> a -> b
retrieve table = fromJust . flip lookup table

replaceAt :: Natural -> a -> [a] -> [a]
replaceAt index item list
	| index >= length = error $ printf "Linca.List.replaceAt: parameter index (%d) was greater than or equal to the length of the list (%s)" index length
	| otherwise = head ++ [item] ++ tail
	where
		length = genericLength list
		head = genericTake (index + 0) list
		tail = genericDrop (index + 1) list

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft list = tail list ++ [head list]

rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight list = [last list] ++ init list

fold :: Foldable t => (x -> a -> a) -> t x -> a -> a
fold = flip . foldl . flip
