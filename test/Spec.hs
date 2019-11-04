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

    it "f1 should work with complex constant and real variable" $
      f1 (1 :+ 1) 2 `shouldBe` (5 :+ 1)

    it "f1 should work with real constant and complex variable" $
      f1 2 (2 :+ 3) `shouldBe` ((-3) :+ 12)

    it "f1 should work with only complex numbers" $
      f1 (7 :+ 9.25) (3 :+ 2) `shouldBe` (12 :+ 21.25)

    it "should use partial application to create function with constant c" $ do
      let f2 = f1 (16 :+ 4)
      f2 5 `shouldBe` (41 :+ 4)
      f2 5.5 `shouldBe` (46.25 :+ 4)
      f2 6 `shouldBe` (52 :+ 4)
