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
    Tree(..),
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

data Tree a = Leaf a | Node a [Tree a] deriving (Eq, Show)

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
isWinning = (== 1) . length . filter (== Peg)


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


generateStates :: Int -> [Pegs]
-- ^ Takes a length n and returns all peg solitaire states possible of that length.
-- Previously worked by generating all numbers in binary of a given length and
-- representing these numbers as their associated pegs. 
-- We have adapted the code as given in the instruction, as it has better runtime complexity.
generateStates len = last $ unfoldr allStatesOfLength (len, [[]])
 where
  allStatesOfLength :: (Int, [Pegs]) -> Maybe ([Pegs], (Int, [Pegs]))
  allStatesOfLength (0, _ ) = Nothing
  allStatesOfLength (n, states) = Just (newStates, (n - 1, newStates))
    where
      newStates = [(Empty:), (Peg:)] <*> states


generateLinearStates :: Int -> [Pegs]
-- ^Generates all the peg solitaire states of length n. 
-- This is achieved using an anamorphism. 
generateLinearStates n = unfoldr rho n
 where
  rho = \v
        -> if v == 0
          then
            Nothing
          else
            Just (replicate (v - 1) Peg ++ [Empty] ++ replicate (n - v) Peg, v - 1)


makeMoves :: Zipper Peg -> [Zipper Peg]
-- ^Produces a list of all possible movs in peg solitaire,
-- when given a zipper of pegs. Uses two anamorphisms on the
-- history and remainder, and pattern-matches around the focus.
makeMoves (Zip h f r) = filter (/= Zip [] Empty []) (unfoldr alpha h ++ unfoldr beta r ++ gamma (take 2 h) f (take 2 r))
  where
    alpha ps = -- hier gebruik ik Zip [] Empty [] als 'empty zipper', want geloof niet dat ik een empty element toe kan voegen
      if length ps <= 2 -- niet meer mogelijk om dan te springen, er is geen ruimte meer
        then Nothing
      else
        Just(if ps!!1 == Empty --second element
          then (Zip [] Empty [], tail ps) --dit moet eigenlijk dus niks zijn, maar wel dat ie tail ps pakt als volgende state
          else
            if head ps == Peg && ps!!2 == Empty
              then (Zip (take (length h - length ps) h ++[Empty, Empty, Peg] ++ drop 3 ps) f r, tail ps)
            else
              if head ps == Empty && ps!!2 == Peg
                then (Zip (take (length h - length ps) h ++[Peg, Empty, Empty] ++ drop 3 ps) f r, tail ps)
              else (Zip [] Empty [], tail ps) --dit moet eigenlijk dus niks zijn
          )
    beta qs =
      if length qs <= 2 -- niet meer mogelijk om dan te springen, er is geen ruimte meer
        then Nothing
      else
        Just(if qs!!1 == Empty --second element
          then (Zip [] Empty [], tail qs)
          else
            if head qs == Peg && qs!!2 == Empty
              then (Zip h f (take (length r - length qs) r ++ [Empty, Empty, Peg] ++ drop 3 qs), tail qs)
            else
              if head qs == Empty && qs!!2 == Peg
                then (Zip h f (take (length r - length qs) r ++ [Peg, Empty, Empty] ++ drop 3 qs), tail qs)
              else (Zip [] Empty [], tail qs)
          )
    -- foc = Empty:
    gamma [Peg, Peg] Empty [Peg, Peg] = [Zip ([Empty, Empty] ++ drop 2 h) Peg r, Zip h Peg ([Empty, Empty] ++ drop 2 r)]
    gamma [Peg, Peg] Empty xs = [Zip ([Empty, Empty] ++ drop 2 h) Peg r]
    gamma xs Empty [Peg, Peg] = [Zip h Peg ([Empty, Empty] ++ drop 2 r)]
    -- foc = Peg:
    gamma [Peg, Empty] Peg [Peg, Empty] = [Zip ([Empty, Peg] ++ drop 2 h) Empty r, Zip h Empty ([Empty, Peg] ++ drop 2 r)]
    gamma [Peg, Empty] Peg (Empty:xs) = [Zip ([Empty, Peg] ++ drop 2 h) Empty r, Zip ([Empty, Empty] ++ drop 2 h) Empty (Peg: (tail r))]
    gamma [Peg, Empty] Peg [Peg, Peg] = [Zip ([Empty, Peg] ++ drop 2 h) Empty r]
    gamma [Peg, Peg] Peg (Empty:xs) = [Zip ([Empty, Peg] ++ drop 2 h) Empty (Peg: drop 1 r)]
    gamma [Peg, Peg] Peg [Peg, Empty] = [Zip h Empty ([Empty, Peg] ++ drop 2 r)]
    gamma (Empty:xs) Peg [Peg, Empty] = [Zip h Empty ([Empty, Peg] ++ drop 2 r), Zip (Peg : (tail h)) Empty ([Empty, Empty] ++ drop 2 r)]
    gamma (Empty:xs) Peg [Peg, Peg] = [Zip (Peg : (tail h)) Empty ([Empty, Peg] ++ drop 2 r)]
    -- foc = Peg maar hist of rem is niet 2 lang:
    gamma [] Peg [Peg, Empty] = [Zip [] Empty ([Empty, Peg] ++ drop 2 r)]
    gamma [Peg, Empty] Peg [] = [Zip ([Empty, Peg] ++ drop 2 h) Empty []]
    gamma (Empty:xs) Peg [Peg] = [Zip (Peg : tail h) Empty [Empty]]
    gamma [Peg] Peg (Empty:xs) = [Zip [Empty] Empty (Peg : tail r)]
    gamma [Peg, Empty] Peg [Peg] = [Zip ([Empty, Peg] ++ drop 2 h) Empty r]
    gamma [Peg] Peg [Peg, Empty] = [Zip h Empty ([Empty, Peg] ++ drop 2 r)]
    -- als geen van bovenstaande cases true is (dus geen moves mogelijk):
    gamma hs foc rs = []

  
unfoldT :: (b -> (a, [b])) -> b -> Tree a
-- ^Anamorphism factory for the tree type. 
-- Takes in a function which associates a value in b with
-- a value in a, tupled with a list in b which form the subtrees.
-- Produces a leaf if this list of subtrees is empty, and a Node otherwise.
-- Example of a function which can be passed onto unfoldT: if p x then (l x, []) else (n x, st x)
-- where st :: b -> [b].
unfoldT g x = let (c, d) = g x in rho (c, d)
  where
    rho (c, []) = Leaf c
    rho (c, d) = Node c (map (unfoldT g) d)


makeGameTree :: Zipper Peg -> Tree (Zipper Peg)
-- ^Makes a Tree of all possible moves from an initial gamestate (given as zipper)
-- for peg solitaire via unfoldT. Gamestates are saved in the Nodes, and states in which no moves
-- can be made are stored inside Leaves.
makeGameTree = unfoldT (\v -> (v, makeMoves v))

hasSolution :: Zipper Peg -> Bool
-- ^Determines whether a game of peg solitaire with given gamestate (given as zipper)
-- is winnable. This means that it is reducibleto a states satisfying the isWinning function
-- by making moves from the initial gamestate. Is defined as a hylomorphism with Trees as virtual type.
hasSolution = foldT (isWinning . fromZipper) (\v u -> or u) . makeGameTree

allSolutions :: Zipper Peg -> [Zipper Peg] 
-- ^Given an initial gamestate (as zipper), returns all winning gamestates which are possible
-- by making moves from the initial gamestate as a list of zippers. Is defined using a hylomorphism.
allSolutions = foldT (\leaf -> if (isWinning . fromZipper) leaf == True then [leaf] else []) (\v u -> concat u) . makeGameTree

allSolutions' :: Zipper Peg -> [Pegs]
-- ^ See allSolutions. Returns a list of Pegs instead of a list of Zipper Peg.
allSolutions' = foldT (\leaf -> if (isWinning . fromZipper) leaf == True then [fromZipper leaf] else []) (\v u -> concat u) . makeGameTree

getSolution :: a
getSolution = error "Implement, document, and test this function"


trySolution = error "Implement, document, and test this function"


