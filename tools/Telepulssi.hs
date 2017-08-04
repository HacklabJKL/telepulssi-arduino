{-# LANGUAGE OverloadedStrings #-}
module Telepulssi where

import qualified Data.ByteString.Lazy.Char8 as B
import Codec.Picture.Types
import Control.Monad
import Data.Binary.Bits.Put (BitPut, runBitPut, putWord8)
import Data.Binary.Put (runPut)
import Data.Bits

-- |Write 4 intensity levels, 2 bits per pixel with no extra padding.
binarize :: () -> a -> a -> Pixel8 -> BitPut ()
binarize _ _ _ v = putWord8 2 $ v `shiftR` 6 

-- |Elovalo escape style.
escape = B.intercalate "~\0" . B.split '~'

-- |Converts grayscale image to serial format.
telepulssify :: Image Pixel8 -> Either String B.ByteString
telepulssify img = case (imageWidth img, imageHeight img) of
  -- Generate raw binary 2-bit grayscale data and produce final serial
  -- data.
  (40, 7) -> let binaryImg = runPut $ runBitPut $ pixelFoldM binarize () img
             in Right $ "~F" `B.append` escape binaryImg
  _ -> Left "Image must be 40×7"
