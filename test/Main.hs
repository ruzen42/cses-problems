module Main (main) where

import Test.Hspec
import Lib
import Test.QuickCheck

test1621 :: Spec 
test1621 = 
  describe "1621 - Distinct Numbers" $ do
    it "0 Test - Return 0 for empty" $ do
      distinctNumsSolve [] `shouldBe` 0

    it "1 Test - Positive nums" $ do 
      distinctNumsSolve [2, 3, 2, 2, 3] `shouldBe` 2

    it "2 Test - Negative nums" $ do 
      distinctNumsSolve [-1, -1, 5, 0] `shouldBe` 3

    it "3 Test - Random nums" $ do
      property $ \xs -> distinctNumsSolve xs <= length (xs :: [Int])

test1068 :: Spec 
test1068 = 
  describe "1068 - Weird Algorithm " $ do
    it "0 Test - Return [1] for 1" $ do
      weirdAlgoSolve 1 `shouldBe` [1]

    it "1 Test - Example" $ do 
      weirdAlgoSolve 3 `shouldBe` [3, 10, 5, 16, 8, 4, 2, 1]

    it "2 Test - Random nums" $ do
      property $ \x -> weirdAlgoSolve x !! 0 == x

test1072 :: Spec 
test1072 = 
  describe "1072 - Two Knights" $ do 
    it "0 Test - Return [0] for 1 and [] for 0" $ do 
      twoKnightsSolve 1 `shouldBe` [0]
      twoKnightsSolve 0 `shouldBe` []

    it "1 Test - Example" $ do 
      twoKnightsSolve 8 `shouldBe` [0, 6, 28, 96, 252, 550, 1056, 1848] 

    it "2 Test - Random nums" $ do 
      property $ forAll (choose (1, 10000)) $ \x -> 
        x == (length $ twoKnightsSolve x) 

  

main :: IO ()
main = hspec $ do
  test1621 
  test1068
  test1072

