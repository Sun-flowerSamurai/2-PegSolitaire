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
--import PegSolitaire (Tree(..))
import GHC.RTS.Flags (MiscFlags(generateStackTrace))
naturals :: Gen Int
naturals = choose (1, 1000)

--data Tree a = Leaf a | Node a [Tree a] deriving (Show)
testTree = makeGameTree (Zip [] Peg [Peg, Empty, Peg, Peg])
answerTree = Node 3 [Leaf 0,Node 1 [Leaf 0],Node 2 [Leaf 0,Node 1 [Leaf 0]]]

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
          generateStates 1 `shouldBe` [[Empty], [Peg]]
    it "should produce nothing for n = 0" $ do
          evaluate(generateStates 0) `shouldThrow` anyErrorCall
    it "should produce 2^n states for n = 10" $ do
          (length . generateStates) 10 `shouldBe` (2^10 ::Int)

  describe "generateLinearStates" $ do
    it "should produce just one state for n = 1" $ do
          generateLinearStates 1 `shouldBe` [[Empty]]
    it "should produce the empty list for n = 0" $ do
          generateLinearStates 0 `shouldBe` []
    it "should produce the same amount of states as the length of the first state" $ property $
          forAll naturals (\n -> (length . generateLinearStates) n == (head . map length . generateLinearStates) n)
    it "should have exactly one empty space in every state and produce n states" $ property $
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
    it "should put focus in the history and put first element of remainder in focus" $ do
          goRight (Zip [5] 1 [2,3,4]) `shouldBe` (Zip [1,5] 2 [3,4])
    it "should not go right if there is no space" $ do
          goRight (Zip [1] 2 []) `shouldBe` (Zip [1] 2 [])
    it "should do nothing if the zipper only has a focus" $ do
          goRight (Zip [] 2 []) `shouldBe` (Zip [] 2 [])


  describe "goLeft" $ do
    it "should put focus in the remainder and put first element of history in focus" $ do
          goLeft (Zip [2,1] 3 [4,5]) `shouldBe` (Zip [1] 2 [3,4,5])
    it "should not go left if there is no space" $ do
          goLeft (Zip [] 1 [2]) `shouldBe` (Zip [] 1 [2])
    it "should do nothing if the zipper only has a focus" $ do
          goLeft (Zip [] 2 []) `shouldBe` (Zip [] 2 [])


  describe "makeMoves" $ do
    it "should return the empty list if no moves can be made (middle)" $ do
          makeMoves (Zip [Peg,Peg] Peg [Peg,Peg]) `shouldBe` []
    it "should return the empty list if no moves can be made (remainder)" $ do
          makeMoves (Zip [Peg,Peg] Peg [Peg,Peg,Peg,Peg,Peg,Peg,Peg]) `shouldBe` []
    it "should return the empty list if no moves can be made (history)" $ do
          makeMoves (Zip [Peg,Peg,Peg,Peg,Peg,Peg] Peg [Peg,Peg]) `shouldBe` []
    it "should make correct moves (history)" $ do
          makeMoves (Zip [Peg, Empty, Peg, Peg, Empty] Empty []) `shouldBe` [Zip [Peg, Peg, Empty, Empty, Empty] Empty [], Zip [Peg, Empty, Empty, Empty, Peg] Empty []]
    it "should make correct moves (remainder)" $ do
          makeMoves (Zip [] Empty [Peg, Empty, Peg, Peg, Empty]) `shouldBe` [Zip [] Empty [Peg, Peg, Empty, Empty, Empty], Zip [] Empty [Peg, Empty, Empty, Empty, Peg]]
    it "should handle the focus correctly I" $ do
          makeMoves (Zip [Peg, Peg] Empty [Peg]) `shouldBe` [(Zip [Empty, Empty] Peg [Peg])]
    it "should handle the focus correctly II" $ do
          makeMoves (Zip [Peg] Empty [Peg, Peg]) `shouldBe` [(Zip [Peg] Peg [Empty, Empty])]
    it "should handle the focus correctly III" $ do
          makeMoves (Zip [Peg, Empty] Peg [Peg]) `shouldBe` [(Zip [Empty, Peg] Empty [Peg])]
    it "should handle the focus correctly IV" $ do
          makeMoves (Zip [Empty, Peg] Peg [Peg]) `shouldBe` [(Zip [Peg, Peg] Empty [Empty])]
    it "should handle the focus correctly V" $ do
          makeMoves (Zip [Empty, Peg] Peg [Peg,Peg]) `shouldBe` [(Zip [Peg, Peg] Empty [Empty,Peg])]
    it "should handle the focus correctly VI" $ do
          makeMoves (Zip [Empty, Peg] Peg [Peg,Empty]) `shouldBe` [(Zip [Empty, Peg] Empty [Empty, Peg]),(Zip [Peg, Peg] Empty [Empty,Empty])]
    it "should handle the focus correctly VII" $ do
          makeMoves (Zip [Empty] Peg [Peg]) `shouldBe` [(Zip [Peg] Empty [Empty])]
    it "should handle the focus correctly VIII" $ do
          makeMoves (Zip [Empty] Peg [Peg,Peg]) `shouldBe` [(Zip [Peg] Empty [Empty,Peg])]
    it "should handle the focus correctly IX" $ do
          makeMoves (Zip [Empty] Peg [Peg,Empty]) `shouldBe` [(Zip [Empty] Empty [Empty, Peg]), (Zip [Peg] Empty [Empty,Empty])]
    it "should handle the focus correctly X" $ do
          makeMoves (Zip [Peg] Peg [Empty, Peg]) `shouldBe` [(Zip [Empty] Empty [Peg, Peg])]
-- could add more

--testTree = makeGameTree (Zip [] Peg [Peg, Empty, Peg, Peg]) defined above
--testTree = Node " (X)  X  .  X  X " [Node " (X)  X  X  .  . " [Leaf " (X)  .  .  X  . "],Node " (.)  .  X  X  X " [Leaf " (.)  X  .  .  X "]] 
  describe "foldT" $ do
  --  it "should be able to count leaves" $ do
  --        foldT (const 1) (\v u -> v + sum u) (Node 1 [Node 2 [Leaf 3, Leaf 4], Leaf 5]) `shouldBe` 3
  -- constructors dont work? idk
    it "should be able to count leaves" $ do
          foldT (const 1) (\v u -> sum u) testTree `shouldBe` 2
    it "should be able to count nodes" $ do
          foldT (const 0) (\v u -> 1 + sum u) testTree `shouldBe` 3
    it "should be able to find maximum depth" $ do
          foldT (const 1) (\v u -> 1 + maximum u) testTree `shouldBe` 3
    it "should be able to make a list from a tree" $ do
          length ( foldT (:[]) (\v u -> v:(concat u)) testTree) `shouldBe` 5
    it "should be able to replicate the identity function" $ do
          foldT (Leaf) (Node) testTree `shouldBe` testTree
-- if constructors work, can add more


  describe "unfoldT" $ do
    it "should be able to build trees as an anamorphism" $ do
          unfoldT (\v -> if v == 0 then (0,[]) else (v, filter (\w -> w < v) [0,1,2,3])) (3 :: Int) `shouldBe` answerTree --doet het niet idk
    it "should be able to build a tree full of constants" $ do
          unfoldT (\v -> if v == 0 then ((1 :: Int),[]) else (1, [v-1])) (3::Int) `shouldBe` Node 1 [Node 1 [Node 1 [Leaf 1]]]
    it "should be polymorphic (lists)" $ do
          unfoldT (\vs -> if length vs == 1 then (head vs, []) else (sum vs, (map (:[]) vs))) [5, 3, 1] `shouldBe` Node 9 [Leaf 5, Leaf 3, Leaf 1]

  describe "makeGameTree" $ do
    it "should produce just a leaf when no move can be made (remainder)" $ do
          makeGameTree (Zip [] Peg [Peg, Peg]) `shouldBe` Leaf (Zip [] Peg [Peg, Peg])
    it "should produce just a leaf when no move can be made (history)" $ do
          makeGameTree (Zip [Empty, Empty] Peg []) `shouldBe` Leaf (Zip [Empty, Empty] Peg [])
    it "should produce just a node when one move can be made" $ do
          makeGameTree (Zip [] Peg [Peg, Empty]) `shouldBe` Node (Zip [] Peg [Peg, Empty]) [Leaf (Zip [] Empty [Empty, Peg])]
    it "should produce multiple leafs when multiple moves can be made" $ do
          makeGameTree (Zip [] Empty [Peg, Peg, Empty]) `shouldBe` Node (Zip [] Empty [Peg, Peg, Empty]) [Leaf (Zip [] Empty [Empty, Empty, Peg]), Leaf (Zip [] Peg [Empty, Empty, Empty])]
    it "should be able to handle edge case of 1 peg" $ do
          makeGameTree (Zip [] Peg []) `shouldBe` Leaf (Zip [] Peg [])

  describe "hasSolution" $ do
    it "should return false for an empty board (board with exactly one peg wins)" $ do
          hasSolution (Zip [] Empty [Empty, Empty, Empty]) `shouldBe` False
    it "should return true for a board which has one peg" $ do
          hasSolution (Zip [] Empty [Empty, Peg, Empty, Empty]) `shouldBe` True
    it "should return true for a winnable board with one move" $ do
          hasSolution (Zip [] Empty [Peg, Peg, Empty]) `shouldBe` True
    it "should return true for a winnable board with two moves, where a wrong move can be made" $ do
          hasSolution (Zip [] Empty [Peg, Peg, Empty, Peg]) `shouldBe` True
    it "should return false for an unwinnable board" $ do
          hasSolution (Zip [] Empty [Peg, Peg, Empty, Empty, Peg]) `shouldBe` False
    it "should return false for any board with an even amount of pegs" $ do
          any (hasSolution . toZipper) (generateLinearStates 9) `shouldBe` False


  describe "allSolutions" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)


