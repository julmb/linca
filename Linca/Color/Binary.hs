module Linca.Color.Binary (fromRGB24, toRGB24) where

import Data.Word
import Linca.Color.Color

fromRGB24 :: (Word8, Word8, Word8) -> Color
fromRGB24 (red, green, blue) = rgb (fromByte red) (fromByte green) (fromByte blue) where
	fromByte byte = fromIntegral byte / 0xFF

toRGB24 :: Color -> (Word8, Word8, Word8)
toRGB24 color = (toByte (red color), toByte (green color), toByte (blue color)) where
	toByte 1 = 0xFF
	toByte value = truncate (value * 0x100)
