{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Codec.Picture
import System.Environment (getArgs)
import Telepulssi
import ImageTools

main = do
  args <- getArgs
  case args of
    [f] -> do
      -- Load image or fail
      img <- readImage f >>= either fail return
      -- Convert to grayscale and then to Telepulssi format
      bs <- either fail return $ telepulssify $ dynToGrayscale img
      -- Push binary to stdout
      B.putStr bs
    _   -> fail "Image file required"
