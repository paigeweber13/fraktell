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

    it "f(c, z) where c = 7.2 + 9.2i and z = 3.2 is 17.44 + 9.2i" $
      f1 (7.2 :+ 9.2) 3.2 `shouldBe` (17.44 :+ 9.2)

    it "should use partial application to create function with constant c" $ do
      let f2 = f1 (3.1 :+ 4.8)
      f2 5 `shouldBe` (28.1 :+ 4.8)
      f2 5.5 `shouldBe` (33.35 :+ 4.8)
      f2 6 `shouldBe` (39.1 :+ 4.8)
