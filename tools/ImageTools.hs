module ImageTools where

import Codec.Picture
import Codec.Picture.Types

-- |Converts image to 8-bit grayscale.
dynToGrayscale :: DynamicImage -> Image Pixel8
dynToGrayscale img = case img of
  -- Some shortcuts
  ImageY8  a -> extractLumaPlane a
  ImageYA8 a -> extractLumaPlane a
  -- And generic form
  a          -> extractLumaPlane $ convertRGB8 a

