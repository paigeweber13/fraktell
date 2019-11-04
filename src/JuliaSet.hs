module JuliaSet where

import Data.Complex
import Graphics.Image

-- This will work for any function with one complex number z as input.
-- Constants and coefficients must be set through partial application 

-- in the output, x is the real portion of the result and y is the imaginary
-- portion, with these values laid out as one would expect with a cartesian
-- grid
visualizeJuliaSet :: (Complex Double -> Complex Double) -- f
                -> Double -- escapeRadius
                -> Int -- width in pixels
                -> Int -- height in pixels
                -> Int -- maxIter
                -> String -- output file name
                -> IO ()
visualizeJuliaSet f escapeRadius width height maxIter outputFilename
  = writeImage outputFilename makeJuliaImage
    where
      makeJuliaImage = makeImageR RSU (width, height) g
      g = pixelToJuliaSetValue f escapeRadius width height maxIter

pixelToJuliaSetValue :: (Complex Double -> Complex Double) -- f
                     -> Double -- escapeRadius
                     -> Int -- width
                     -> Int -- height
                     -> Int -- maxIter
                     -> (Int, Int) -- i, j
                     -> Pixel RGB Double -- result: pixel value
pixelToJuliaSetValue f escapeRadius width height maxIter (i, j)
    = toPixelRGB (PixelHSI pixelValue 0.5 0.5)
      where
        pixelValue :: Double
        pixelValue = (fromIntegral (julia f (x :+ y) escapeRadius maxIter))/
                    (fromIntegral maxIter+1)
        x = -escapeRadius + (2*escapeRadius/(fromIntegral width)) * 
          (fromIntegral j)
        y = -(-escapeRadius + (2*escapeRadius/(fromIntegral height)) *
          (fromIntegral i))

-- complex and real parts of z must be between -escapeRadius and escapeRadius
julia :: (Complex Double -> Complex Double) -- f
      -> Complex Double -- z
      -> Double -- escapeRadius
      -> Int -- maxIter
      -> Int -- result
julia f z escapeRadius maxIter = juliaRecursive f z escapeRadius 1 maxIter

-- not meant to be called by user; use julia instead
juliaRecursive :: (Complex Double -> Complex Double)
      -> Complex Double
      -> Double -- escapeRadius
      -> Int -- i
      -> Int -- maxIter
      -> Int -- result
juliaRecursive f z escapeRadius i maxIter
  | i > maxIter = maxIter
  | Data.Complex.magnitude(z') > escapeRadius = i
  | otherwise = juliaRecursive f z' escapeRadius (i+1) maxIter
    where
      z' = f z
