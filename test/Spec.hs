import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import PegSolitaire

main :: IO ()
main = hspec $ do
  describe "isWinning" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "generateStates" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "generateLinearStates" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "fromZipper" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "toZipper" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "goRight" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "goLeft" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeMoves" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "foldT" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "unfoldT" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeGameTree" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "hasSolution" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "allSolutions" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ 
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)


