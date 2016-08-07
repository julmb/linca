module Linca.Cryptography (crc16) where

import Data.Bits
import Data.Word
import Linca.Basic

crc16 :: Word8 -> Word16 -> Word16
crc16 byte = power 8 step . initial where
	initial value = fromIntegral byte `xor` value
	step value = if testBit value 0 then shiftR value 1 `xor` 0xA001 else shiftR value 1
