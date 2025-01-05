module Linca.Basic (equal, power) where

import Numeric.Natural
import Linca.Error

equal :: Eq a => a -> a -> a
equal a b
    | a /= b = error $ errorMessage "equal" "parameters a and b were not equal"
    | otherwise = a

power :: Natural -> (a -> a) -> (a -> a)
power 0 _ = id
power n f = f . power (n - 1) f
