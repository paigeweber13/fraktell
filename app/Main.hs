module Main where

import Data.Complex

import CommonFunctions
import JuliaSet

main :: IO ()
main = do
  -- visualizeJuliaSet f 2 1000 1000 1000
  visualizeJuliaSet f 2 1000 1000 100
  where
    f = f1 ((-0.4) :+ 0.65)
