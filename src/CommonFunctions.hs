module CommonFunctions where

import Data.Complex

-- f(z) = z^2 + c
f1 :: Complex Double -> Double -> Complex Double
f1 (a :+ b) z = (z**2 :+ 0) + (a :+ b)

-- below is valid haskell
-- f1 :: Double -> Double -> Double
-- f1 a b = a**2 + b