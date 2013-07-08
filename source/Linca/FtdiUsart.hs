module Linca.FtdiUsart where

import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Data.ByteString as BS
import Bindings.Libftdi

data Device = Device { context :: Ptr C'ftdi_context }

openDevice :: IO Device
openDevice =
	do
		newContext <- c'ftdi_new
		let device = Device { context = newContext }
		c'ftdi_usb_open (context device) 0x0403 0x6001
		return device

closeDevice :: Device -> IO ()
closeDevice device =
	do
		c'ftdi_usb_close (context device)
		c'ftdi_free (context device)

setBaudrate :: Device -> Integer -> IO ()
setBaudrate device baudrate =
	do
		c'ftdi_set_baudrate (context device) (fromIntegral baudrate)
		return ()

sendData :: Device -> BS.ByteString -> IO ()
sendData device byteString =
	do
		let dataList = map fromIntegral (BS.unpack byteString)
		let dataLength = fromIntegral (BS.length byteString)
		withArray dataList (\pointer -> c'ftdi_write_data (context device) pointer dataLength)
		return ()
