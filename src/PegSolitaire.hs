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
    Zipper(..),
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


foldT :: (a -> b)    -- Leaf function
  -> (a -> [b] -> b) -- Node function
  -> Tree a          -- input tree
  -> b               -- folded result
-- ^The foldT function acts as a catamorphism factory for the Tree type.
-- The Tree type has two constructors and similarly the foldT function 
-- takes two functions. The function l is applied to the leaves and the 
-- function n is applied to the nodes.
foldT l n = rec 
 where
  rec (Leaf x) = l x
  rec (Node x ts) = n x (map rec ts) -- hier kan je n zien als binary functie, bv cons


data Zipper a = Zip [a] a [a] deriving (Eq, Ord) -- History, Focus, Remainder

instance (Show a) => Show (Zipper a) where
    show (Zip h f r) = show (show (reverse h) ++ " (" ++ show f ++ ") " ++ show r) 

fromZipper :: Zipper a -> [a]
fromZipper (Zip h f r) = reverse h ++ [f] ++ r


toZipper :: [a] -> Zipper a
toZipper []     = error "Zipper of empty list is undefined"
toZipper (x:xs) = Zip [] x xs


goRight :: Zipper a -> Zipper a
goRight (Zip h f []) = Zip h f []
goRight (Zip h f (x:xs)) = Zip (f:h) x xs


goLeft :: Zipper a -> Zipper a
goLeft (Zip [] f r) = Zip [] f r
goLeft (Zip (x:xs) f r) = Zip xs x (f:r)


listmult :: Int -> [a] -> [a] --nodig voor genlinstates
listmult 0 xs = []
listmult n xs = xs ++ listmult (n-1) xs 


to2 :: Int -> Int -> [Int] -- length, getal, base 2
-- assumption is dat n < 2^l
to2 0 n = []
to2 l n = if n >= 2^(l-1) then 1: to2 (l-1) (n - 2^(l-1)) else 0 : to2 (l-1) n


generateStates :: Int -> [Pegs]
-- kijk hier wordt ie fucked
-- werkt wel maar ziet er lelijk uit
-- wat we zouden kunnen doen is de 1en en 0en gelijk naar pegs converten in de generateBinaries
-- weet niet of het dan efficienter of duidelijker is maar je maakt er dan wel één unfold van wat ze miss willen
generateStates n = map numsToPeg (generateBinaries n)
 where
  numsToPeg = map (\v -> if v == 1 then Peg else Empty)
  generateBinaries n = unfoldr (\w -> if w == -1 then Nothing else Just(to2 n w, w-1)) (2^n - 1)
  -- ik vind zelf w == -1 ook beetje lelijk maargoed


generateLinearStates :: Int -> [Pegs]
-- vind m een beetje lelijk en jammer dat we listmult nodig hebben
-- maar ie werkt
generateLinearStates n = unfoldr rho n
 where 
  rho = (\v -> if v == 0 then Nothing else Just(listmult (v-1) [Peg] ++ [Empty] ++ listmult (n-v) [Peg], v-1))


makeMoves = error "Implement, document, and test this function"


unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
