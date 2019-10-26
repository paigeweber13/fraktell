import Control.Exception (evaluate)
import Data.Complex
import Test.Hspec
import Test.QuickCheck

import CommonFunctions

main :: IO ()
main = hspec $ do
  -- test CommonFunctions
  describe "CommonFunctions.f1" $ do
    it "f1 should work with only real numbers" $
      f1 1 2 `shouldBe` 5

    it "f1 should work with imaginary constant and real variable" $
      f1 (1 :+ 1) 2 `shouldBe` (5 :+ 1)

    it "f1 should work with real constant and imaginary variable" $
      f1 2 (2 :+ 3) `shouldBe` ((-3) :+ 12)

    it "f1 should work with only complex numbers" $
      f1 (7.2 :+ 9.2) (3.2 :+ 1.9) `shouldBe` (13.83 :+ 21.36)

    it "should use partial application to create function with constant c" $ do
      let f2 = f1 (3.1 :+ 4.8)
      f2 5 `shouldBe` (28.1 :+ 4.8)
      f2 5.5 `shouldBe` (33.35 :+ 4.8)
      f2 6 `shouldBe` (39.1 :+ 4.8)
