module Main where

import Data.Complex
import System.Environment

import CommonFunctions
import JuliaSet

main :: IO ()
main = do
  args <- getArgs
  outputFilename <- (args !! 4)
  if length args < 5
  then usage
  else if length args < 6
    then f = f1 ((-0.4) :+ 0.65)
    else f = parseFunctionParams (read (args !! 6) :: Int)
  putStrLn "visualizing julia set..."
  visualizeJuliaSet f 
    (read (args !! 0) :: Double)
    (read (args !! 1) :: Int)
    (read (args !! 2) :: Int)
    (read (args !! 3) :: Int)
    outputFilename
  putStrLn ("Done! Output to " ++ outputFilename)
  where
    f = f1 ((-0.4) :+ 0.65)

-- takes string cli args and returns processed args for visualizeJuliaSet
parseArgs :: [String] -> (
  (Complex Double -> Complex Double), Double, Int, Int, Int, String)
parseArgs args
  | (length args) < 5 = usage

parseFunctionParams :: Integer
                    -> [Complex]
                    -> (Complex Double -> Complex Double)
parseFunctionParams func_num params
    | 

usage = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " escape_radius width height "
    ++ "max_iter output_filename [func_num")
  putStrLn ("                    [constant1 [constant2] ... ]]")
  putStrLn ""
  putStrLn ("number of constants must correspond to the number of constants "
    ++ "function func_num takes")
