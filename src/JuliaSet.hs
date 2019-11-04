module JuliaSet where

import Data.Complex
import Graphics.Image

-- make this work for arbitrary funciton (use arbitrary number of params?)
--    alternatively, assume that whatever function will only take one value, z,
--    as input, and other parameters can be applied through partial application

-- in the output, x is the real portion of the result and y is the imaginary
-- portion
computeJuliaSet :: (Complex Double -> Complex Double)
                -> Double 
                -> Int
                -> Int
                -> Int
                -> IO ()
computeJuliaSet f escapeRadius width height maxIter
  = writeImage "images/output.png" makeJuliaImage
    where
      makeJuliaImage = makeImageR VU (width, height) g
      g = pixelToJuliaSetValue f escapeRadius width height maxIter

pixelToJuliaSetValue :: (Complex Double -> Complex Double) -- f
                     -> Double -- escapeRadius
                     -> Int -- width
                     -> Int -- height
                     -> Int -- maxIter
                     -> (Int, Int) -- i, j
                     -> Pixel Y Double -- result: pixel value
pixelToJuliaSetValue f escapeRadius width height maxIter (i, j)
    = PixelY grayValue
      where
        grayValue = (fromIntegral (julia f (x :+ y) escapeRadius maxIter))/
                    (fromIntegral maxIter)
        x = -escapeRadius + (2*escapeRadius/(fromIntegral width)) * 
          (fromIntegral i)
        y = -escapeRadius + (2*escapeRadius/(fromIntegral height)) *
          (fromIntegral j)

-- complex and real parts of z must be between -escapeRadius and escapeRadius
julia:: (Complex Double -> Complex Double) -- f
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
  | i > maxIter = 0
  | Data.Complex.magnitude(z') > escapeRadius = i
  | otherwise = juliaRecursive f z' escapeRadius (i+1) maxIter
    where
      z' = f z
