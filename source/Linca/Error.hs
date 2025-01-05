module Linca.Error (errorMessage) where

import Text.Printf

errorMessage :: String -> String -> String
errorMessage location message = printf "%s: %s" location message
