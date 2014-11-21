module GrossoneTest where

import Test.Hspec
import Test.QuickCheck
import Grossone


main :: IO ()
main = hspec $ do
  describe "Grossone Tests" $ do
    it "Return infinitesimal Grossone 1" $ do
      gInfinitesimal  `shouldBe` ([Pair 1 (negate 1)])

    it "testing gPair" $ do
	property $ (\x y -> gPair x y `shouldBe` 
						if (x)==0 
							then []
							else [(Pair (x) y)])

    it "testing gAdd static infinitesimal + infinitesimal " $ do
	 (gAdd gInfinitesimal gInfinitesimal) `shouldBe` ([Pair 2 (negate 1)])

    it "testing gGetGrossDigit " $ do
	  property $ (\x y -> gGetGrossDigit (Pair x y) `shouldBe` x)

    it "testing gGetGrossPower " $ do
	  property $ (\x y -> gGetGrossPower (Pair x y) `shouldBe` y)

    it "testing gAddPairs same power" $ do
	property $ (\x y w -> gAdd (gPair x w) (gPair y w)  `shouldBe` 
									if (x+y)==0 
									then []
									else [(Pair (x+y) w)])

