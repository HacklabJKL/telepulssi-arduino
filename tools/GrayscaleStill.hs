{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Data.Binary.Bits.Put (BitPut, runBitPut, putWord8)
import Data.Binary.Put (runPut)
import Data.Bits
import System.Environment (getArgs)

-- |Write 4 intensity levels, 2 bits per pixel with no extra padding.
binarize :: () -> a -> a -> Pixel8 -> BitPut ()
binarize _ _ _ v = putWord8 2 $ v `shiftR` 6 

-- |Elovalo escape style.
escape = B.intercalate "~\0" . B.split '~'

-- |Load image and return pixel in serial format.
telepulssify f = do
  imgDyn <- readImage f >>= either fail return
  let img = case imgDyn of
        ImageRGB8  a -> extractLumaPlane a
        ImageRGBA8 a -> extractLumaPlane a
        ImageY8    a -> extractLumaPlane a
        ImageYA8   a -> extractLumaPlane a
        _ -> error "Not grayscale image"
  when (imageWidth img /= 40 || imageHeight img /= 7) $ fail "Image must be 40×7"
  return $ escape $ runPut $ runBitPut $ pixelFoldM binarize () img 

main = do
  args <- getArgs
  case args of
    [f] -> telepulssify f >>= B.putStr
    _ -> fail "Image file required"
