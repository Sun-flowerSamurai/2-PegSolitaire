{-|
Module      : PegSolitaire
Description : 
Copyright   : Matt Verhoeven (1728342)
              David Chen (1742477)


-}
module PegSolitaire
  (
    Peg(..),
    Pegs,
    stringToPegs,
    ----
    isWinning,
    generateStates,
    generateLinearStates,
    -- Zipper(..),
    fromZipper,
    toZipper,
    goRight,
    goLeft,
    ----
    makeMoves,
    foldT,
    unfoldT,
    makeGameTree,
    hasSolution,
    allSolutions,
    getSolution,
    trySolution,
  )
where
import Data.List (unfoldr)

data Peg = Empty | Peg deriving (Eq, Ord)

type Pegs = [Peg]

data Tree a = Leaf a | Node a [Tree a] deriving (Show)

instance Show Peg where
  show Empty = "."
  show Peg = "X"

  showList xs = \s -> foldr (\ x-> (' ':) . shows x . (' ':)) s xs

stringToPegs :: String -> Pegs
stringToPegs = map f
  where
    f '.' = Empty
    f 'X' = Peg
    f _ = error "Invalid peg string"

----------------------------------

isWinning :: Pegs -> Bool
-- ^Determines whether the Pegs are in a winning state, 
-- i.e. whether there is only one peg left on the board.
isWinning = (== 1) . countPegs
  where 
    countPegs :: Pegs -> Integer
    countPegs = sum . map (\ v -> if v == Peg then 1 else 0)

foldT :: (a->b) -> (a->[b]->b) -> Tree a -> b --leaffunc, nodefunc, tree
foldT l n = rec 
 where
  rec (Leaf x) = l x
  rec (Node x ts) = n x (map rec ts)


generateStates = error "Implement, document, and test this function"
generateLinearStates = error "Implement, document, and test this function"

-- data Zipper a = 

fromZipper = error "Implement, document, and test this function"
toZipper = error "Implement, document, and test this function"
goRight = error "Implement, document, and test this function"
goLeft = error "Implement, document, and test this function"
makeMoves = error "Implement, document, and test this function"


unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
