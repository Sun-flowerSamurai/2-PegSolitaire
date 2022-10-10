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
  rec (Node x ts) = n x (map rec ts) --hier kan je n zien als binary functie, bv cons


generateStates = error "Implement, document, and test this function"
generateLinearStates = error "Implement, document, and test this function"

data Zipper a = Zip [a] a [a] deriving Show --History, Focus, Remainder
-- Weet niet hoe ik de history om kan draaien met Show

fromZipper :: Zipper a -> [a]
fromZipper (Zip hist a rem) = reverse hist ++ [a] ++ rem

toZipper :: [a] -> Zipper a
toZipper xs = Zip [] (head xs) (tail xs)

goRight :: Zipper a -> Zipper a
goRight (Zip hist a []) = Zip hist a []
goRight (Zip hist a (x:xs)) = Zip (a:hist) x xs

goLeft :: Zipper a -> Zipper a
goLeft (Zip [] a rem) = Zip [] a rem 
goLeft (Zip (x:xs) a rem) = Zip xs x (a:rem)

makeMoves = error "Implement, document, and test this function"


unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
