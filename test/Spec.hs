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
import PegSolitaire (generateLinearStates, generateStates)
import GHC.RTS.Flags (MiscFlags(generateStackTrace))
naturals :: Gen Int
naturals = choose (1, 1000)

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
    it "should produce two states for n = 1" $ do
          generateStates 1 `shouldBe` [[Peg], [Empty]]
    it "should produce the empty list for n = 0" $ do
          generateStates 0 `shouldBe` [[]]

  describe "generateLinearStates" $ do
    it "should produce just one state for n = 1" $ do
          generateLinearStates 1 `shouldBe` [[Empty]]
    it "should produce the empty list for n = 0" $ do
          generateLinearStates 0 `shouldBe` []
    it "should produce the same amount of states as the length of the first state" $ property $
          forAll naturals (\n -> (length . generateLinearStates) n == (head . map length . generateLinearStates) n)
    it "should have one empty space in every state and produce n states" $ property $
          forAll naturals (\n -> (sum . map (sum . map (\v -> if v == Empty then 1 else 0)) . generateLinearStates) n == n )

  describe "fromZipper" $ do
    it "should convert the head to a list" $ do
          fromZipper (Zip [] 2 []) `shouldBe` [2]
    it "should convert the history to a list with elements shuffled around" $ do
          fromZipper (Zip [3,2,1] 4 []) `shouldBe` [1,2,3,4]
    it "should convert the remainder to a list" $ do
          fromZipper (Zip [] 1 [2,3,4]) `shouldBe` [1,2,3,4]
    it "should work on multiple types of Zipper I" $ do
          fromZipper (Zip "cba" 'd' "efg") `shouldBe` "abcdefg"
    it "should work on multiple types of Zipper II" $ do
          fromZipper (Zip [Peg, Empty, Peg] Empty [Peg, Empty, Peg]) `shouldBe` [Peg, Empty, Peg, Empty, Peg, Empty, Peg]

  describe "toZipper" $ do
    it "zipper of empty list is undefined" $ do
          evaluate (toZipper []) `shouldThrow` anyErrorCall 
    it "zipper of list with one element should have empty history and remainder" $ 
      do toZipper ([1]:: [Integer]) 
      `shouldBe` Zip ([]::[Integer]) (1:: Integer) ([]:: [Integer])
    it "toZipper needs to handle lists over different types I" $ 
      do toZipper ([1, 2, 3, 4]:: [Integer]) 
      `shouldBe` Zip ([]::[Integer]) (1:: Integer) ([2, 3, 4]:: [Integer])
    it "toZipper needs to handle lists over different types II" $ 
      do toZipper ("abcdefg" :: String) 
      `shouldBe` Zip ([]:: String) ('a':: Char) ("bcdefg":: String)
    it "toZipper needs to handle lists over different types III" $ 
      do toZipper ([Peg, Empty, Peg, Empty, Peg] :: Pegs) 
      `shouldBe` Zip ([]:: Pegs) (Peg:: Peg) ([Empty, Peg, Empty, Peg]:: Pegs)



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


