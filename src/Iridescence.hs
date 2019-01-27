module Iridescence where

import Data.List (sort)
import Numeric (showHex)
import System.IO (hFlush, stdout)
import Codec.Picture
  ( PaletteCreationMethod(..)
  , PaletteOptions(..)
  , PixelRGB8(..)
  , convertRGB8
  , palettize
  , readImage
  , writePng
  )
import Codec.Picture.Types (generateImage, pixelFold)

-- | Palletization options.
opts :: PaletteOptions
opts = PaletteOptions MedianMeanCut True 8

-- | Bound an (ordinal) value.
clamp :: (Ord a) => a -> a -> a -> a
clamp a b = min b . max a

-- | Darken a pixel.
darken :: Float -> PixelRGB8 -> PixelRGB8
darken a (PixelRGB8 r g b) =
  let ratio = clamp 0 1 a
      f x = fromIntegral . floor $ a * fromIntegral x
  in  PixelRGB8 (f r) (f g) (f b)

-- | Lighten a pixel.
lighten :: Float -> PixelRGB8 -> PixelRGB8
lighten a (PixelRGB8 r g b) =
  let ratio = clamp 0 1 a
      f x = fromIntegral . floor $
        fromIntegral x + (255 - fromIntegral x) * ratio
  in  PixelRGB8 (f r) (f g) (f b)

-- | Get a hex string from an RGB pixel.
hex :: PixelRGB8 -> String
hex (PixelRGB8 r g b) =
  let f x = showHex (fromIntegral x) ""
  in  concat [f r, f g, f b]

app :: IO ()
app = do
  putStr " [~] reference image: "
  hFlush stdout
  file <- getLine
  img <- readImage file
  case img of
    Left _ -> putStrLn " [~] failed to read image."
    Right rgb -> do
      let rawPalette = (snd . palettize opts . convertRGB8) rgb
          colors :: [PixelRGB8]
          colors = sort $ pixelFold (const . const . flip (:)) [] rawPalette
          lightcolors = lighten 0.4 <$> colors
          theme = colors ++ lightcolors
          palette = generateImage (const . (!!) theme) 16 1
      putStrLn " [~] colors: \n"
      putStrLn $ concat (flip (++) "\n" . hex <$> theme)
      writePng "palette.png" palette
