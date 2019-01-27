module Iridescence where

import Data.Either (either)
import System.IO (hFlush, stdout)
import Codec.Picture
  ( PaletteCreationMethod(..)
  , PaletteOptions(..)
  , convertRGB8
  , palettize
  , readImage
  , writePng
  )
import Codec.Picture.Types (ColorSpaceConvertible(..))

-- | Palletization options.
opts :: PaletteOptions
opts = PaletteOptions MedianMeanCut True 10

app :: IO ()
app = do
  putStr " [~] reference image: "
  hFlush stdout
  file <- getLine
  img <- readImage file
  case img of
    Left _ -> putStrLn " [~] failed to read image."
    Right rgb -> do
      let palette = (snd . palettize opts . convertRGB8) rgb
      writePng "palette.png" palette
