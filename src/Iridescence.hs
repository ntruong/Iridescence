module Iridescence where

import Data.Fixed (mod')
import Data.List (maximum, minimum, sort)
import Numeric (showHex)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, writeFile)
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
opts = PaletteOptions MedianMeanCut True 6

-- | Bound an (ordinal) value.
clamp :: (Ord a) => a -> a -> a -> a
clamp a b = min b . max a

-- | HSL as a type.
type HSL = (Float, Float, Float)

-- | Convert RGB to HSL.
rgb2hsl :: PixelRGB8 -> HSL
rgb2hsl (PixelRGB8 r g b) =
  let r' = fromIntegral r / 255
      g' = fromIntegral g / 255
      b' = fromIntegral b / 255
      colors = [r', g', b']
      maxH = maximum colors
      minH = minimum colors
      h' c x y = (60 * (c + (x - y) / (maxH - minH))) `mod'` 360
      h
        | maxH == minH = 0
        | maxH == r'   = h' 0 g' b'
        | maxH == g'   = h' 2 b' r'
        | maxH == b'   = h' 4 r' g'
        | otherwise    = 0
      s
        | maxH == 0 = 0
        | minH == 1 = 0
        | otherwise = (maxH - l) / min l (1 - l)
      l = (maxH + minH) / 2
  in  (h, s, l)

-- | Convert HSL to RGB.
hsl2rgb :: HSL -> PixelRGB8
hsl2rgb (h, s, l) =
  let c = s * (1 - abs (2 * l - 1))
      h' = h / 60
      x = c * (1 - abs (h' `mod'` 2 - 1))
      m = l - c / 2
      (r', g', b')
        | 0 <= h' && h' <  1 = (c, x, 0)
        | 1 <= h' && h' <  2 = (x, c, 0)
        | 2 <= h' && h' <  3 = (0, c, x)
        | 3 <= h' && h' <  4 = (0, x, c)
        | 4 <= h' && h' <  5 = (x, 0, c)
        | 5 <= h' && h' <= 6 = (c, 0, x)
        | otherwise          = (0, 0, 0)
      r = floor (255 * (r' + m))
      g = floor (255 * (g' + m))
      b = floor (255 * (b' + m))
  in  PixelRGB8 r g b

-- | Saturate a pixel to a given saturation.
saturate :: Float -> PixelRGB8 -> PixelRGB8
saturate s' x =
  let (h, s, l) = rgb2hsl x
  in  hsl2rgb (h, s', l)

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

-- | Blend two pixels together.
blend :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend (PixelRGB8 r g b) (PixelRGB8 x y z) =
  let f = flip div 2
  -- We use (f x + f y) instead of f (x + y) to avoid overflow.
  in  PixelRGB8 (f r + f x) (f g + f y) (f b + f z)

-- | Get a hex string from an RGB pixel.
hex :: PixelRGB8 -> String
hex (PixelRGB8 r g b) =
  let f x = showHex (fromIntegral x) ""
      p x = case (length . f) x of
        1 -> "0" ++ (f x)
        _ -> f x
  in  concat [p r, p g, p b]

app :: FilePath -> IO ()
app file = do
  img <- readImage file
  case img of
    Left _ -> do
      hPutStrLn stderr $ "could not read file \"" ++ file ++ "\""
      exitFailure
    Right rgb -> do
      let rawimg = (snd . palettize opts . convertRGB8) rgb
          rawcolors = sort (pixelFold (const . const . flip (:)) [] rawimg)
          colors :: [PixelRGB8]
          colors = [darken 0.8 (head rawcolors)]
            ++ (saturate 0.5 . lighten 0.2 <$> rawcolors)
            ++ [blend (PixelRGB8 255 255 255) (last rawcolors)]
          lightcolors = lighten 0.2 <$> colors
          theme = colors ++ lightcolors
          png = generateImage (const . (!!) theme) 16 1
      writeFile "colors.conf" $ concat (flip (++) "\n" . hex <$> theme)
      writePng "palette.png" png
