module CommonFunctions where

import Data.Complex

-- f(z) = z^2 + c
f1 :: Complex Double -> Complex Double -> Complex Double
f1 c z = z**2 + c

-- below is valid haskell
-- f1 :: Double -> Double -> Double
-- f1 a b = a**2 + b