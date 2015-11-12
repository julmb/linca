module Linca.Error (rangeError) where

import Text.Printf

rangeError :: String -> String -> t
rangeError location valueName = error $ printf "%s: value %s was outside of the allowed range" location valueName
