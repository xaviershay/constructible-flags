{-# LANGUAGE BangPatterns #-}

module TestImageUtils
  ( imagesEqual,
    diffImage,
    writePngImage,
  )
where

import Codec.Picture

-- | Pixel-by-pixel equality check
imagesEqual :: Image PixelRGBA8 -> Image PixelRGBA8 -> Bool
imagesEqual a b =
  imageWidth a == imageWidth b
    && imageHeight a == imageHeight b
    && allPixelsEqual 0
  where
    allPixelsEqual !idx
      | idx >= w * h = True
      | otherwise =
          let x = idx `mod` w
              y = idx `div` w
              p1 = pixelAt a x y
              p2 = pixelAt b x y
           in p1 == p2 && allPixelsEqual (idx + 1)
    w = imageWidth a
    h = imageHeight a

-- | Produce a simple absolute-difference visualisation
diffImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
diffImage a b = generateImage gen w h
  where
    w = min (imageWidth a) (imageWidth b)
    h = min (imageHeight a) (imageHeight b)
    gen x y =
      let (PixelRGBA8 r1 g1 b1 _) = pixelAt a x y
          (PixelRGBA8 r2 g2 b2 _) = pixelAt b x y
          dr = abs (fromIntegral r1 - fromIntegral r2) :: Int
          dg = abs (fromIntegral g1 - fromIntegral g2) :: Int
          db = abs (fromIntegral b1 - fromIntegral b2) :: Int
       in PixelRGBA8 (fromIntegral dr) (fromIntegral dg) (fromIntegral db) 255

-- | Helper to write a DynamicImage to PNG
writePngImage :: FilePath -> DynamicImage -> IO ()
writePngImage path img = writePng path (convertRGBA8 img)
