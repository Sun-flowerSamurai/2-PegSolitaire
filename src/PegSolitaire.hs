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

-- |Shows a zipper with its history reversed, the focus parenthesized 
-- and the remainder unchanged.
instance (Show a) => Show (Zipper a) where
    show (Zip h f r) = show (show (reverse h) ++ " (" ++ show f ++ ") " ++ show r) 
    --show (Zip h f r) = show h ++ " (" ++ show f ++ ") " ++ show r


fromZipper :: Zipper a -> [a]
-- ^Turns a zipper structure back into a list 
-- with all the elements in the appropriate order.
fromZipper (Zip h f r) = reverse h ++ [f] ++ r


toZipper :: [a] -> Zipper a
-- ^Turns a list into a Zipper with the head of the list
-- at its focus, empty history and the tail of the list as its remainder.
toZipper []     = error "Zipper of empty list is undefined"
toZipper (x:xs) = Zip [] x xs


goRight :: Zipper a -> Zipper a
-- ^Moves the focus of a zipper one to the right
-- if the focus is already the at the last element,
-- the zipper remains unchanged.
goRight (Zip h f []) = Zip h f []
goRight (Zip h f (x:xs)) = Zip (f:h) x xs


goLeft :: Zipper a -> Zipper a
-- ^Moves the focus of a zipper one to the left
-- if the focus is already the at the first element,
-- the zipper remains unchanged.
goLeft (Zip [] f r) = Zip [] f r
goLeft (Zip (x:xs) f r) = Zip xs x (f:r)


listmult :: Int -> [a] -> [a]
-- ^Takes an integer n and a list and creates a new list which is
-- n repeats of that list.
-- helper function for genLinearStates
listmult 0 xs = []
listmult n xs = xs ++ listmult (n-1) xs



generateStates :: Int -> [Pegs]
-- ^ Takes a length n and returns all peg solitaire states possible of that length.
-- Works by generating all numbers in binary of a given length and
-- representing these numbers as their associated pegs. 
-- This function is exponential as there are 2^n - 1 possible gamestates
-- and it produces each one individually.
generateStates n = unfoldr (\w -> if w == 0 then Nothing else Just(to2 n (w-1), w-1)) (2^n)
 where
-- to2 :: Int -> Int -> Pegs -- length, number, binary number as pegs
-- an assumption is that n < 2^l
  to2 0 n = []
  to2 l n = if n >= 2^(l-1) then Peg: to2 (l-1) (n - 2^(l-1)) else Empty : to2 (l-1) n


generateLinearStates :: Int -> [Pegs]
-- ^ Generates all the peg solitaire states of length n. 
-- Uses the function 'listmult' in it's implementation. 
generateLinearStates n = unfoldr rho n
 where
  rho = \v
        -> if v == 0
          then
            Nothing
          else
            Just (listmult (v-1) [Peg] ++ [Empty] ++ listmult (n-v) [Peg], v-1)


second :: [a] -> a -- ik kwam er dus achter dat hier gwn functies voor zitten in standard library lmao
second = head . tail
third :: [a] -> a
third = head . tail . tail
fourthplus :: [a] -> [a]
fourthplus = tail . tail . tail -- of drop 3

makeMoves :: Zipper Peg -> [Zipper Peg]
-- assumet dat je geen zipper bestaande uit Zip [] Empty [] invult
makeMoves (Zip h f r) = filter (/= Zip [] Empty []) (unfoldr alpha h ++ unfoldr beta r ++ gamma (take 2 h) f (take 2 r)) --weet niet of take 2 h klopt eig
  where
    alpha ps = -- hier gebruik ik Zip [] Empty [] als 'empty zipper', want geloof niet dat ik een empty element toe kan voegen
      if length ps <= 2 -- niet meer mogelijk om dan te springen, er is geen ruimte meer
        then Nothing
      else
        Just(if second ps == Empty
          then (Zip [] Empty [], tail ps) --dit moet eigenlijk dus niks zijn, maar wel dat ie tail ps pakt
          else
            if (head ps == Peg && third ps == Empty)
              then (Zip (take (length h - length ps) h ++[Empty, Empty, Peg] ++ fourthplus ps) f r, tail ps)
            else
              if (head ps == Empty && third ps == Peg)
                then (Zip (take (length h - length ps) h ++[Peg, Empty, Empty] ++ fourthplus ps) f r, tail ps)
              else (Zip [] Empty [], tail ps) --dit moet eigenlijk dus niks zijn, maar wel dat ie tail ps pakt
          )
    beta qs =
      if length qs <= 2 -- niet meer mogelijk om dan te springen, er is geen ruimte meer
        then Nothing
      else
        Just(if second qs == Empty
          then (Zip [] Empty [], tail qs)
          else
            if (head qs == Peg && third qs == Empty)
              then (Zip h f (take (length r - length qs) r ++ [Empty, Empty, Peg] ++ fourthplus qs), tail qs)
            else
              if (head qs == Empty && third qs == Peg)
                then (Zip h f (take (length r - length qs) r ++ [Peg, Empty, Empty] ++ fourthplus qs), tail qs)
              else (Zip [] Empty [], tail qs)
          )
    gamma hs foc rs = --sorry voor de indentation dit is echt garbage tier
      if foc == Empty --haskell zegt dat is deze if statements kan verbeteren maar ik geloof m niet
    then if hs == [Peg, Peg]
         then if rs == [Peg, Peg]
              then [Zip ([Empty, Empty] ++ drop 2 h) Peg r, Zip h Peg ([Empty, Empty] ++ drop 2 r)]
              else [Zip ([Empty, Empty] ++ drop 2 h) Peg r]
         else if rs == [Peg, Peg]
              then [Zip h Peg ([Empty, Empty] ++ drop 2 r)]
              else []
    else if hs == [Peg, Empty]
         then if rs == [Peg, Empty]
              then [Zip ([Empty, Peg] ++ drop 2 h) Empty r, Zip h Empty ([Empty, Peg] ++ drop 2 r)]
              else [Zip ([Empty, Peg] ++ drop 2 h) Empty r]
         else if rs == [Peg, Empty]
              then [Zip h Empty ([Empty, Peg] ++ drop 2 r)]
              else []


unfoldT :: (b -> (a,[b])) -> b -> Tree a
unfoldT g x = let (c,d) = g x in rho (c,d)
  where
    rho (c, []) = Leaf c
    rho (c, d) = Node c (map (unfoldT g) d)
-- g can be of the following form:
-- if p x then (f x, []) else (h x, [some list])

makeGameTree :: Zipper Peg -> Tree (Zipper Peg)
makeGameTree = unfoldT (\v -> (v, makeMoves v))

hasSolution :: Zipper Peg -> Bool
hasSolution = foldT (isWinning . fromZipper) (\v u -> or u) . makeGameTree

allSolutions :: Zipper Peg -> [Pegs] --vragen of we de gamestates als zippers moeten opslaan of dat het ook als list mag
allSolutions = foldT (\leaf -> if (isWinning . fromZipper) leaf then [fromZipper leaf] else []) (\v u -> concat u) . makeGameTree


getSolution = error "Implement, document, and test this function"


trySolution = error "Implement, document, and test this function"


