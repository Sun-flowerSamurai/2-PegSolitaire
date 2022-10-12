{-|
Description : Test cases for the PegSolitaire functions.
Copyright   : Matt Verhoeven (1728342)
              David Chen (1742477)


-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import PegSolitaire

main :: IO ()
main = hspec $ do
  describe "isWinning" $ do
    it "should consider an empty board losing" $ do
          isWinning ([Empty, Empty, Empty, Empty, Empty]:: Pegs) `shouldBe` (False :: Bool)

    it "should consider a board with (only) one peg winning" $ do
          isWinning ([Peg]:: Pegs) `shouldBe` (True :: Bool)

    it "should consider a board with one peg winning" $ do
          isWinning ([Empty, Empty, Empty, Peg, Empty]:: Pegs) `shouldBe` (True :: Bool)

    it "should consider a board with two consecutive pegs losing" $ do
          isWinning ([Empty, Empty, Empty, Peg, Peg]:: Pegs) `shouldBe` (False :: Bool)

    it "should consider a board with two non-consecutive pegs losing" $ do
          isWinning ([Empty, Empty, Empty, Peg, Empty, Peg]:: Pegs) `shouldBe` (False :: Bool)

    it "should consider a board with multiple pegs losing" $ do
          isWinning ([Peg, Peg, Peg, Empty, Peg, Empty, Peg]:: Pegs) `shouldBe` (False :: Bool)


  describe "generateStates" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "generateLinearStates" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "fromZipper" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "toZipper" $ do
    it "zipper of empty list is undefined" $ do
          evaluate (toZipper []) `shouldThrow` anyErrorCall 
    it "zipper of list with one element " $ do
          toZipper [1] `shouldBe` Zip [] 1 []



  describe "goRight" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "goLeft" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeMoves" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "foldT" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "unfoldT" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeGameTree" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "hasSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "allSolutions" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)


