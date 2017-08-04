module Main where

import Network.Socket hiding (recvFrom)
import Control.Monad
import Codec.Picture
import System.IO (hPutStr, stderr)
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Telepulssi
import ImageTools

udpServer :: Integral a => a -> (BS.ByteString -> IO ()) -> IO ()
udpServer udpPort handler = do
  withSocketsDo $ do
    sock <- socket AF_INET Datagram 0
    bind sock (SockAddrInet (fromIntegral udpPort) iNADDR_ANY)
    forever $ do
      (msg, _) <- recvFrom sock 65507
      handler msg

-- FIXME use my serial handling code instead of `stty -F /dev/ttyACM0 19200 raw`
main = udpServer 1337 pngToTelepulssi

pngToTelepulssi bs = case decodeImage bs of
  Left _    -> hPutStr stderr "Invalid image format received\n"
  Right img -> case telepulssify (dynToGrayscale img) of
    Left e  -> hPutStr stderr $ e ++ "\n"
    Right a -> BL.putStr a
