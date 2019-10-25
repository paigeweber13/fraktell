import Control.Exception (evaluate)
import Data.Complex
import Test.Hspec
import Test.QuickCheck

import CommonFunctions

main :: IO ()
main = hspec $ do
  -- test CommonFunctions
  describe "CommonFunctions.f1" $ do
    it "f(c, z) where c = 1 + 1i and z = 2 is 5 + 1i" $
      f1 (1.0 :+ 1.0) 2.0 `shouldBe` (5.0 :+ 1.0)

    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
