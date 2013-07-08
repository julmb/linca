module Linca.FtdiUsart where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array
import qualified Data.ByteString as BS
import Bindings.Libftdi

data Device = Device { context :: Ptr C'ftdi_context }

getErrorString :: Ptr C'ftdi_context -> IO String
getErrorString context =
	do
		errorString <- c'ftdi_get_error_string context
		peekCString errorString

openDevice :: IO Device
openDevice =
	do
		newContext <- c'ftdi_new
		if newContext == nullPtr
			then error "Linca.FtdiUsart.openDevice: cound not initialize new ftdi context"
			else
				do
					let device = Device { context = newContext }
					result <- c'ftdi_usb_open (context device) 0x0403 0x6001
					if result < 0
						then
							do
								errorString <- getErrorString (context device)
								error ("Linca.FtdiUsart.openDevice: could not open USB device, error code: " ++ show result ++ ", error message: " ++ errorString)
						else return device

closeDevice :: Device -> IO ()
closeDevice device =
	do
		result <- c'ftdi_usb_close (context device)
		if result < 0
			then
				do
					errorString <- getErrorString (context device)
					error ("Linca.FtdiUsart.closeDevice: could not close USB device, error code: " ++ show result ++ ", error message: " ++ errorString)
			else c'ftdi_free (context device)

setBaudrate :: Device -> Integer -> IO ()
setBaudrate device baudrate =
	do
		result <- c'ftdi_set_baudrate (context device) (fromIntegral baudrate)
		if result < 0
			then
				do
					errorString <- getErrorString (context device)
					error ("Linca.FtdiUsart.setBaudrate: could not set baudrate, error code: " ++ show result ++ ", error message: " ++ errorString)
			else return ()

sendData :: Device -> BS.ByteString -> IO ()
sendData device byteString =
	do
		let dataList = map fromIntegral (BS.unpack byteString)
		let dataLength = fromIntegral (BS.length byteString)
		result <- withArray dataList (\pointer -> c'ftdi_write_data (context device) pointer dataLength)
		if result < 0
			then
				do
					errorString <- getErrorString (context device)
					error ("Linca.FtdiUsart.sendData: could not send data, error code: " ++ show result ++ ", error message: " ++ errorString)
			else return ()
