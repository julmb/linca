module Linca.Range (Range, rangeII, rangeIE, contains, violates, unitRange, rangeError', rangeError) where

import Text.Printf
import Linca.Error

data Bound value = None | Inclusive value | Exclusive value
data Range value = Range (Bound value) (Bound value)

rangeII :: Ord value => value -> value -> Range value
rangeII lower upper
	| lower > upper = localError "rangeII" "parameter lower was larger than parameter upper"
	| otherwise = Range (Inclusive lower) (Inclusive upper)

rangeIE :: Ord value => value -> value -> Range value
rangeIE lower upper
	| lower > upper = localError "rangeIE" "parameter lower was larger than parameter upper"
	| otherwise = Range (Inclusive lower) (Exclusive upper)

instance Show value => Show (Range value) where
	show (Range None None) = "{..}"
	show (Range (Inclusive lower) None) = printf "{%s ..}" (show lower)
	show (Range None (Inclusive upper)) = printf "{.. %s}" (show upper)
	show (Range (Exclusive lower) None) = printf "{%s <..}" (show lower)
	show (Range None (Exclusive upper)) = printf "{..< %s}" (show upper)
	show (Range (Inclusive lower) (Inclusive upper)) = printf "{%s .. %s}" (show lower) (show upper)
	show (Range (Exclusive lower) (Inclusive upper)) = printf "{%s <.. %s}" (show lower) (show upper)
	show (Range (Inclusive lower) (Exclusive upper)) = printf "{%s ..< %s}" (show lower) (show upper)
	show (Range (Exclusive lower) (Exclusive upper)) = printf "{%s <..< %s}" (show lower) (show upper)

contains :: Ord value => Range value -> value -> Bool
contains (Range None None) _ = True
contains (Range (Inclusive lower) None) value = lower <= value
contains (Range None (Inclusive upper)) value = value <= upper
contains (Range (Exclusive lower) None) value = lower < value
contains (Range None (Exclusive upper)) value = value < upper
contains (Range (Inclusive lower) (Inclusive upper)) value = lower <= value && value <= upper
contains (Range (Exclusive lower) (Inclusive upper)) value = lower < value && value <= upper
contains (Range (Inclusive lower) (Exclusive upper)) value = lower <= value && value < upper
contains (Range (Exclusive lower) (Exclusive upper)) value = lower < value && value < upper

violates :: Ord value => Range value -> value -> Bool
violates range = not . contains range

unitRange :: Real value => Range value
unitRange = rangeII 0 1

rangeError' :: String -> String -> result
rangeError' location name = localError location $ printf "value %s was outside of the allowed range" name

rangeError :: Show value => String -> String -> Range value -> value -> result
rangeError location name range value = localError location $ printf "value %s (%s) was outside of the allowed range (%s)" name (show value) (show range)
