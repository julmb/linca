module Linca.List (naturals, indices, enum, retrieve, replace, rotateLeft, rotateRight, prefix, suffix, fold, clusterConsecutive, clusterBy, bwt, ibwt) where

import Numeric.Natural
import Data.Maybe
import Data.List
import Text.Printf
import Linca.Error

naturals :: [Natural]
naturals = enumFrom 0

indices :: Natural -> [Natural]
indices count = [0 .. count - 1]

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

retrieve :: Eq a => [(a, b)] -> a -> b
retrieve table = fromJust . flip lookup table

replace :: Natural -> [a] -> [a] -> [a]
replace offset chunk original
	| offset + genericLength chunk > genericLength original = error $ errorMessage "replace" $
		printf "offset + chunk length (%u) was larger than the original length (%u)" (offset + genericLength chunk) (genericLength original :: Natural)
	| otherwise = genericTake offset original ++ chunk ++ genericDrop (offset + genericLength chunk) original

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft list = tail list ++ [head list]

rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight list = [last list] ++ init list

rotate :: Natural -> [a] -> [a]
rotate = genericDrop <> genericTake

rotations :: [a] -> [[a]]
rotations xs = init (zipWith (++) (tails xs) (inits xs))

prefix :: Natural -> [a] -> [a]
prefix length list = genericTake length list

suffix :: Natural -> [a] -> [a]
suffix length list = genericDrop (genericLength list - length) list

fold :: Foldable t => (x -> a -> a) -> (t x -> a -> a)
fold = flip . foldl . flip

clusterConsecutive :: (a -> a -> Bool) -> [a] -> [[a]]
clusterConsecutive equal = go [] [] where
	go gs [] [] = gs
	go gs ys [] = gs ++ [ys]
	go gs [] (x : xs) = go gs [x] xs
	go gs ys (x : xs) = if equal x (last ys) then go gs (ys ++ [x]) xs else go (gs ++ [ys]) [] (x : xs)

clusterBy :: (a -> a -> Ordering) -> (a -> a -> Bool) -> [a] -> [[a]]
clusterBy compare equal = clusterConsecutive equal . sortBy compare

bwt :: Ord a => [a] -> [Maybe a]
bwt xs = map last $ sort $ rotations $ Nothing : map Just xs

ibwt :: Ord a => [Maybe a] -> [a]
ibwt xs = text where
	adjust table = sort $ zipWith (:) xs table
	empty = genericReplicate (genericLength xs) []
	table = genericIndex (iterate adjust empty) (genericLength xs)
	text = catMaybes $ fromJust $ find (\row -> head row == Nothing) table
