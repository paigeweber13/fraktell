module Main where

import Data.Complex
import System.Environment

import CommonFunctions
import JuliaSet

main :: IO ()
main = do
  args <- getArgs
  if length args < 5 then do
    usage
  else do
    processedArgs <- parseArgs args
    outputFilename <- (args !! 4)
    putStrLn "visualizing julia set..."
    visualizeJuliaSet 
      (processedArgs !! 0)
      (processedArgs !! 1)
      (processedArgs !! 2)
      (processedArgs !! 3)
      (processedArgs !! 4)
    putStrLn ("Done! Output to " ++ outputFilename)

-- takes string cli args and returns processed args for visualizeJuliaSet
parseArgs :: [String] -> (
  (Complex Double -> Complex Double), Double, Int, Int, Int, String)
parseArgs args
  -- | (length args) < 5 = do
  --     usage
  --     putStrLn "using default values " ++ (show (
  --       default_func, 1.5, 1000, 1000, 100, "images/output.png"))
  --     (default_func, 1.5, 1000, 1000, 100, "images/output.png")
  | (length args) < 6 = (default_func, r, width, height, maxIter, 
                         outputFilename)
  | (length args) < 7 = ((parseFunctionParams func_num []), r, width, height,
                         maxIter, outputFilename)
  | (length args) < 8 = ((parseFunctionParams func_num params), r, width,
                         height, maxIter, outputFilename)
  where
    default_func = f1 ((-0.4) :+ 0.65)
    r = read (args !! 0) :: Double
    width = read (args !! 1) :: Int
    height = read (args !! 2) :: Int
    maxIter = read (args !! 3) :: Int
    outputFilename = args !! 4
    func_num = read (args!!5) :: Int
    params = map (read::String -> Complex Double) (slice 6 (length args) args)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

parseFunctionParams :: Int
                    -> [Complex Double]
                    -> (Complex Double -> Complex Double)
parseFunctionParams func_num params
    | func_num == 1 && params == [] = f1 ((-0.4) :+ 0.65)
    | func_num == 1 = f1 (params!!0)

usage = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " escape_radius width height "
    ++ "max_iter output_filename [func_num")
  putStrLn ("                    [constant1 [constant2] ... ]]")
  putStrLn ""
  putStrLn ("number of constants must correspond to the number of constants "
    ++ "function func_num takes")
