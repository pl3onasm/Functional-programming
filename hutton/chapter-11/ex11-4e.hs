import Data.Char
import Data.List
import System.IO
import Control.Concurrent (threadDelay)

-----------------------------------------------------------
-- Exercise 11.4.e

-- The size of the tic-tac-toe grid 
size :: Int
size = 5

-- A Grid is a 2D list of Player values
type Grid = [[Player]]

-- A Player is either O, X, or B (Blank)
-- Order is used for comparing outcomes in  
-- minimax: O < B < X
data Player = O | B | X
              deriving (Eq, Ord, Show)

-- A Tree data type to represent the game state
-- Each node contains a grid and a list of subtrees
-- representing possible future game states
data Tree a = Node a [Tree a]
              deriving Show

-- Switches to the next player (O → X, X → O)
nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = B
nextPlayer X = O

-- ┌──────────────────────────────────────────────────────┐
-- │                    Grid Utilities                    │
-- └──────────────────────────────────────────────────────┘

-- The empty grid: a square grid filled with blanks
empty :: Grid 
empty = replicate size (replicate size B)

-- Checks if the grid is full (no blanks left)
full :: Grid -> Bool
full = all (/= B) . concat

-- Determines whose turn it is based on counts of O and X
turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

-- Checks if a player has a winning line in the grid
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- Gets the diagonal of a grid 
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size - 1]]

-- Checks if the game has been won by either player
won :: Grid -> Bool
won g = wins O g || wins X g

-- ┌──────────────────────────────────────────────────────┐
-- │                   Display functions                  │
-- └──────────────────────────────────────────────────────┘

-- Prints the entire grid to the screen
putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interl bar . map showRow
    where bar = [replicate ((size*4) - 1) '-']

-- Converts a row of Player values into a printable string
showRow :: [Player] -> [String]
showRow = beside . interl bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar    = replicate 3 "|"

-- Formats a string with ANSI escape codes for bold 
-- magenta text used for highlighting the player symbol O
boldM :: String -> String
boldM s = "\ESC[1;31m" ++ s ++ "\ESC[0m"

-- Formats a string with ANSI escape codes for bold 
-- blue text used for highlighting the player symbol X
boldB :: String -> String
boldB s = "\ESC[1;34m" ++ s ++ "\ESC[0m"

-- Converts a Player value into a printable 
-- 3-line representation
showPlayer :: Player -> [String]
showPlayer O = ["   ", boldM " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", boldB " X ", "   "]

-- Inserts a separator between elements of a list
interl :: a -> [a] -> [a]
interl x []       = []
interl x [y]      = [y]
interl x (y : ys) = y : x : interl x ys

-- Clears the terminal screen (ANSI escape code)
cls :: IO ()
cls = putStr "\ESC[2J"

-- Moves the cursor to a specific position (x, y)
goto :: (Int,Int) -> IO ()
goto (x,y) = 
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- ┌──────────────────────────────────────────────────────┐
-- │                    Making a move                     │
-- └──────────────────────────────────────────────────────┘

-- Checks if a move index is valid: should be within bounds
-- and the grid cell should be blank (B). Uses a flat
-- representation of the grid for easier indexing.
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

-- Applies a move to the grid, replacing the blank cell
-- at linear index i with the player's symbol (O or X).
-- Returns a singleton list with the updated grid if 
-- move is valid, otherwise an empty list.
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs, B : ys) = splitAt i (concat g)

-- Converts a flat list into rows of n elements each
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- ┌──────────────────────────────────────────────────────┐
-- │                    Reading Input                     │
-- └──────────────────────────────────────────────────────┘

-- Prompts the user to enter a natural number
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs 
  then return (read xs)
  else do putStrLn "ERROR: Invalid number"
          getNat prompt

-- Generates prompt text for the player
prompt :: Player -> String
prompt p = "It's your turn! Enter your move (0-" 
           ++ show (size^2 - 1) ++ "): "

-- ┌──────────────────────────────────────────────────────┐
-- │                      Game Trees                      │
-- └──────────────────────────────────────────────────────┘

-- Generates the full game tree from a grid 
-- and current player
gametree :: Grid -> Player -> Tree Grid
gametree g p = 
  Node g [gametree g' (nextPlayer p) | g' <- moves g p]

-- Generate all possible valid next moves
moves :: Grid -> Player -> [Grid]
moves g p 
  | won g     = []   -- No moves if the game is over
  | full g    = []   -- No moves if the grid is full
  | otherwise = 
      concat [move g i p | i <- [0..(size^2 - 1)]]

-- Limits the depth of the game tree to a fixed value
-- Used to reduce search complexity by cutting off 
-- branches beyond depth n
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- Maximum search depth
depth :: Int
depth = 4

-- ┌──────────────────────────────────────────────────────┐
-- │            Minimax with Alpha-Beta Pruning           │
-- └──────────────────────────────────────────────────────┘

-- Implements the minimax algorithm with α-β pruning
-- It returns an annotated tree with the best score for
-- the current player at the root node
-- The parameters α and β are bounds for pruning
-- and are updated during the search; for player O
-- (minimizing player) we update β, and for player X
-- (maximizing player) we update α
abPrune :: Player -> Player -> Player -> Tree Grid -> 
           Tree (Grid, Player)
abPrune _ _ _ (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []

abPrune player α β (Node g (t : ts)) = 
  search ts α' β' bestSubtree [bestSubtree]
  where
    bestSubtree = abPrune (nextPlayer player) α β t
    bestScore = getScore bestSubtree
    (α', β') = updateAB α β bestScore

    search [] _ _ best acc = 
      Node (g, getScore best) (reverse acc)

    search (t : ts) α β best acc
      | cutoff α β = Node (g, getScore best) (reverse acc)
      | otherwise  =
          let subtree = abPrune (nextPlayer player) α β t
              score = getScore subtree
              (α', β') = updateAB α β score
              (best', acc') = 
                if better score (getScore best)
                then (subtree, subtree : acc)
                else (best, subtree : acc)
          in search ts α' β' best' acc'

    getScore (Node (_, p) _) = p
    better = if player == O then (<) else (>)
    cutoff α β = α >= β
    updateAB α β p
      | player == O = (α, min β p)  -- O's turn minimizes β
      | otherwise   = (max α p, β)  -- X's turn maximizes α


-- Finds the best move for the current player by generating
-- the pruned game tree and selecting the move with the 
-- best score
bestmove :: Grid -> Player -> Grid
bestmove g p = 
  head [g' | Node (g', p') _ <- ts, p' == best]
  where 
    tree = prune depth (gametree g p)
    Node (_, best) ts = abPrune p O X tree

-- ┌──────────────────────────────────────────────────────┐
-- │                       Game Loop                      │
-- └──────────────────────────────────────────────────────┘

-- Main entry point: starts the game with an empty 
-- grid and player O
tictactoe :: IO ()
tictactoe = do 
  hSetBuffering stdout NoBuffering
  play empty O

-- Displays the grid and continues the game loop
play :: Grid -> Player -> IO ()
play g p = do 
  cls
  goto (1,1)
  putGrid g
  play' g p

-- Main game logic for alternating turns between 
-- human (O) and computer (X)
play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "You win! 🎉 🥳\n"
  | wins X g = putStrLn "Computer wins! 😢\n"
  | full g   = putStrLn "🤝 It's a draw!\n"
  | p == O   = 
      do i <- getNat (prompt p)
         case move g i p of
            []   -> do putStrLn "ERROR: Invalid move"
                       play' g p
            [g'] -> play g' (nextPlayer p)
  | p == X   = 
      do putStr "Computer is thinking... "
         threadDelay 1000000  -- simulates thinking time
         (play $! (bestmove g p)) (nextPlayer p)