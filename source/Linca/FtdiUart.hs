module Linca.FtdiUart (Device, openDevice, closeDevice, setBaudrate, sendData) where

import Control.Monad
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import qualified Data.ByteString as BS
import Bindings.Libftdi

data Device = Device { context :: Ptr C'ftdi_context }

getErrorString :: Device -> IO String
getErrorString device = do
	errorString <- c'ftdi_get_error_string (context device)
	peekCString errorString

checkResult :: Device -> String -> CInt -> IO ()
checkResult device action result =
	when (result < 0) $ do
		errorString <- getErrorString device
		error ("Linca.FtdiUart." ++ action ++ ": " ++ errorString ++ ", error code: " ++ show result )

openDevice :: IO Device
openDevice = do
	newContext <- c'ftdi_new
	when (newContext == nullPtr) $ error "Linca.FtdiUart.openDevice: cound not initialize new ftdi context"
	let device = Device { context = newContext }
	result <- c'ftdi_usb_open (context device) 0x0403 0x6001
	checkResult device "openDevice" result
	return device

closeDevice :: Device -> IO ()
closeDevice device = do
	result <- c'ftdi_usb_close (context device)
	checkResult device "closeDevice" result
	c'ftdi_free (context device)

setBaudrate :: Device -> Integer -> IO ()
setBaudrate device baudrate = do
	result <- c'ftdi_set_baudrate (context device) (fromIntegral baudrate)		
	checkResult device "setBaudrate" result

sendData :: Device -> BS.ByteString -> IO ()
sendData device byteString = do
	let dataList = map fromIntegral (BS.unpack byteString)
	let dataLength = fromIntegral (BS.length byteString)
	result <- withArray dataList (\pointer -> c'ftdi_write_data (context device) pointer dataLength)
	checkResult device "sendData" result
