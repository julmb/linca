module Linca.Error (localError) where

import Text.Printf

localError :: String -> String -> result
localError location message = error $ printf "%s: %s" location message
