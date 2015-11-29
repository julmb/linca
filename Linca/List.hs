module Linca.List (enum, retrieve, replace, rotateLeft, rotateRight, fold) where

import Numeric.Natural
import Data.Maybe
import Data.List
import Text.Printf

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

retrieve :: Eq a => [(a, b)] -> a -> b
retrieve table = fromJust . flip lookup table

replace :: Natural -> [a] -> [a] -> [a]
replace offset chunk original
	| offset + genericLength chunk > genericLength original = error $
		printf "replace: offset + chunk length (%d) was larger than the original length (%d)" (offset + genericLength chunk) (genericLength original :: Natural)
	| otherwise = genericTake offset original ++ chunk ++ genericDrop (offset + genericLength chunk) original

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft list = tail list ++ [head list]

rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight list = [last list] ++ init list

fold :: Foldable t => (x -> a -> a) -> t x -> a -> a
fold = flip . foldl . flip
