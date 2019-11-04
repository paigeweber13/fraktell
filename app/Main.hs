module Main where

import Data.Complex
import System.Environment

import CommonFunctions
import JuliaSet

main :: IO ()
main = do
  args <- getArgs
  if length args < 4
  then usage
  else do
    putStrLn "visualizing julia set..."
    visualizeJuliaSet f 
      (read (args !! 0) :: Double)
      (read (args !! 1) :: Int)
      (read (args !! 2) :: Int)
      (read (args !! 3) :: Int)
    putStrLn "Done! Output to"
  where
    f = f1 ((-0.4) :+ 0.65)

usage = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " escape_radius width height "
    ++ "max_iter func_num [constant1 [constant2] ... ]")
  putStrLn ("number of constants must correspond to the number of constants "
    ++ "function func_num takes")
