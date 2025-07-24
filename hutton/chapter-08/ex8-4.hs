-----------------------------------------------------------
-- Exercise 8.4

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

-- splits a list into two halves whose lengths differ
-- by at most one element.
split :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- constructs a balanced binary tree from a list
-- by recursively splitting the list and creating nodes
-- from the halves.
makeBalTree :: [a] -> Tree a
makeBalTree [] = error ("Cannot create a tree" ++
                        " from an empty list")
makeBalTree xs = case split xs of
  ([], [x]) -> Leaf x
  (l, r)    -> Node (makeBalTree l) (makeBalTree r) 

-----------------------------------------------------------

{-
  Examples:

  ghci> :l ex8-4
  ghci> makeBalTree [1..3]
  Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
  ghci> makeBalTree [1..4]
  Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-}