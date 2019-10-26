module JuliaSet where

import Data.Complex

-- type EscapeRadius = Double
-- type Width = Double
-- type Height = Double

-- make this work for arbitrary funciton (use arbitrary number of params?)
-- computeJuliaSet :: (Double -> Complex Double) -> Double -> Double -> Double
computeJuliaSet :: (Complex Double -> Complex Double)
                -> Double 
                -> Double 
                -> Double 
                -> Int
                -> [[Double]]
computeJuliaSet f escapeRadius width height maxIter = 
  output
  where output = [[]]
