module Linca.ByteString.Lazy (extend, fold, replace) where

import Numeric.Natural
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import Linca.Error

extend :: BS.ByteString -> BL.ByteString -> BL.ByteString
extend chunk rest = BL.fromStrict chunk <> rest

fold :: (Word8 -> a -> a) -> (BL.ByteString -> a -> a)
fold = flip . BL.foldl . flip

replace :: Natural -> BL.ByteString -> BL.ByteString -> BL.ByteString
replace offset chunk original
    | offset + fromIntegral (BL.length chunk) > fromIntegral (BL.length original) = error $ errorMessage "replace" $
        printf "offset + chunk length (%u) was larger than the original length (%u)" (offset + fromIntegral (BL.length chunk)) (BL.length original)
    | otherwise = BL.take (fromIntegral offset) original <> chunk <> BL.drop (fromIntegral offset + BL.length chunk) original
