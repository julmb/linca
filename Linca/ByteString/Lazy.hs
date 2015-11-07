module Linca.ByteString.Lazy (fold) where

import Data.Word
import Data.ByteString.Lazy

fold :: (Word8 -> a -> a) -> ByteString -> a -> a
fold = flip . Data.ByteString.Lazy.foldl . flip
