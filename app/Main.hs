module Main where

-- third party imports
import Criterion.Main
import Data.Complex
import Data.Tuple
import Prelude as P
import Graphics.Image
import Graphics.Image.Interface as II
import System.Environment

-- local imports
import CommonFunctions
import JuliaSet

main :: IO ()
main = do
  args <- getArgs
  if length args < 6 then do
    usage
  else do
    -- let outputFilename = args !! 4
    -- putStrLn "visualizing julia set..."
    runWithCliArgs args
    -- putStrLn ("Done! Image output to " ++ outputFilename)

visualizeJuliaBench f r width height maxIter outputFilename arrType
  | arrType == "VU" = putStrLn (show (index 00 (II.compute 
                      (makeImageR RPU (width, height) g))))
  | arrType == "VS" = putStrLn (show (index 00 (II.compute 
                      (makeImageR RPU (width, height) g))))
  | arrType == "RSU" = putStrLn (show (index 00 (II.compute 
                       (makeImageR RPU (width, height) g))))
  | arrType == "RPU" = putStrLn (show (index 00 (II.compute 
                       (makeImageR RPU (width, height) g))))
  | arrType == "RSS" = putStrLn (show (index 00 (II.compute 
                       (makeImageR RPU (width, height) g))))
  | arrType == "RPS" = putStrLn (show (index 00 (II.compute 
                       (makeImageR RPU (width, height) g))))
  | otherwise = putStrLn (show (index 00 (II.compute 
                         (makeImageR RPU (width, height) g))))
  where
    g = pixelToJuliaSetValue f r width height maxIter

-- visualizeJuliaBench = defaultMain [
--     bgroup "visualize julia set" [
--       -- bench "f1 ((-0.4) :+ 0.65) VU" $ whnf makeImageVU g,
--       -- bench "f1 ((-0.4) :+ 0.65) VS" $ whnf makeImageVS g,
--       bench "f1 ((-0.4) :+ 0.65) RSU" $ whnf makeImageRSU g,
--       bench "f1 ((-0.4) :+ 0.65) RPU" $ whnf makeImageRPU g,
--       bench "f1 ((-0.4) :+ 0.65) RSS" $ whnf makeImageRSS g,
--       bench "f1 ((-0.4) :+ 0.65) RPS" $ whnf makeImageRPS g
--     ]
--   ]
--   where
--     default_func = f1 ((-0.4) :+ 0.65)
--     g = pixelToJuliaSetValue default_func 1.5 width height 100
--     -- makeImageVU = makeImageR VU (width, height)
--     -- makeImageVS = makeImageR VS (width, height)
--     makeImageRSU = makeImageR RSU (width, height)
--     makeImageRPU = makeImageR RPU (width, height)
--     makeImageRSS = makeImageR RSS (width, height)
--     makeImageRPS = makeImageR RPS (width, height)
--     width = 10000
--     height = 10000

-- takes string cli args and returns processed args for visualizeJuliaSet
runWithCliArgs :: [String] -> IO()
runWithCliArgs args
  -- | (length args) < 5 = do
  --     usage
  --     putStrLn "using default values " ++ (show (
  --       default_func, 1.5, 1000, 1000, 100, "images/output.png"))
  --     (default_func, 1.5, 1000, 1000, 100, "images/output.png")
  | (length args) < 7 = run default_func r width height
                          maxIter outputFilename "RPU" 
  | (length args) < 8 = run default_func r width height
                          maxIter outputFilename arrayType
  | (length args) < 9 = run (parseFunctionParams func_num [])
                          r width height maxIter outputFilename arrayType
  | (length args) < 10 = run (parseFunctionParams func_num params)
                          r width height maxIter outputFilename arrayType
  where
    default_func = f1 ((-0.4) :+ 0.65)
    benchOrGen = (args !! 0)
    r = P.read (args !! 1) :: Double
    width = P.read (args !! 2) :: Int
    height = P.read (args !! 3) :: Int
    maxIter = P.read (args !! 4) :: Int
    outputFilename = args !! 5
    arrayType = args !! 6
    func_num = P.read (args!!7) :: Int
    params = P.map (P.read::String -> Complex Double) 
                         (slice 8 (length args) args)

run benchOrGen f r width height maxIter outputFilename arrayType
  | benchOrGen == 'b' = visualizeJuliaBench f r width height maxIter 
                                          outputFilename arrayType
  | benchOrGen == 'g' = visualizeJuliaSet f r width height maxIter 
                                          outputFilename arrayType

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

parseFunctionParams :: Int
                    -> [Complex Double]
                    -> (Complex Double -> Complex Double)
parseFunctionParams func_num params
    | func_num == 1 && params == [] = f1 ((-0.4) :+ 0.65)
    | func_num == 1 = f1 (params!!0)

usage = do
  let prog = "stack run"
  -- prog <- getProgName
  putStrLn ("Usages: ") 
  putStrLn ("    " ++ prog)
  putStrLn ("    " ++ prog ++ " escape_radius width height "
            ++ "max_iter output_filename [arrType]")
  putStrLn ("              [func_num [constant1 [constant2] ... ]]")
  putStrLn ("    " ++ prog ++ " -- --output benchmark.html")
  putStrLn ""
  putStrLn "usage 1 (no argments) displays this help"
  putStrLn ("usage 2 visualizes the julia set created with the " ++
           "given parameters")
  putStrLn "usage 3 benchmarks the program"
  putStrLn ""
  putStrLn "notes on usage 2:"
  putStrLn ("number of constants must correspond to the number of constants "
    ++ "function func_num takes")
  putStrLn ("example: " ++ prog ++ " 1.5 1000 1000 100 "
    ++ "\"images/output.png\"")
  putStrLn ("example: " ++ prog ++ " 1.5 1000 1000 100 "
    ++ "\"images/output.png\" RPU 1 \"(0.285 :+ 0)\"")
