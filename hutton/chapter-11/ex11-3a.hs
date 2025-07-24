import Data.Char
import Data.List
import System.IO

-----------------------------------------------------------
-- Exercise 11.3.a
-- This version solves the exercise by labeling each node
-- with an extra value: the depth of the outcome in the 
-- tree, so that the computer player (Player X) can easily
-- choose the quickest path to a win or draw.

-- The size of the tic-tac-toe grid 
size :: Int
size = 3

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
-- │                     Grid Utilities                   │
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

-- Checks if a player has won
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

-- Converts a Player value into a printable 
-- 3-line representation
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

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
-- │                     Making a move                    │
-- └──────────────────────────────────────────────────────┘

-- Checks if a move index is valid: should be within bounds
-- and the grid cell should be Blank (B)
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

-- Applies a move to the grid 
-- (returns singleton list if valid)
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
     where (xs, B : ys) = splitAt i (concat g)

-- Converts a flat list into rows of n elements each
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- ┌──────────────────────────────────────────────────────┐
-- │                     Reading Input                    │
-- └──────────────────────────────────────────────────────┘

-- Prompts the user to enter a natural number
getNat :: String -> IO Int
getNat prompt = 
  do putStr prompt
     xs <- getLine
     if xs /= [] && all isDigit xs then
       return (read xs)
     else
       do putStrLn "ERROR: Invalid number"
          getNat prompt

-- Generates prompt text for the player
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- ┌──────────────────────────────────────────────────────┐
-- │                       Game Trees                     │
-- └──────────────────────────────────────────────────────┘

-- Generates the full game tree from a grid 
-- and current player
gametree :: Grid -> Player -> Tree Grid
gametree g p = 
  Node g [gametree g' (nextPlayer p) | g' <- moves g p]

-- Generate all possible valid next moves
moves :: Grid -> Player -> [Grid]
moves g p 
  | won g     = []  -- No moves if the game is over
  | full g    = []  -- No moves if the grid is full
  | otherwise = 
      concat [move g i p | i <- [0..(size^2 - 1)]]

-- Limits the depth of the game tree to a fixed value
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- Maximum search depth
depth :: Int
depth = 9

-- ┌──────────────────────────────────────────────────────┐
-- │                        Minimax                       │
-- └──────────────────────────────────────────────────────┘

-- Minimax with depth annotation:
-- each node is annotated with the grid data and
-- the best outcome for the current player, along with
-- the depth of that outcome in the tree
minimax :: Tree Grid -> Tree (Grid, (Player, Int))
minimax (Node g [])
  | wins O g  = Node (g, (O, 0)) []
  | wins X g  = Node (g, (X, 0)) []
  | otherwise = Node (g, (B, 0)) []  -- Draw 
minimax (Node g ts) 
  = Node (g, (best, 1 + bestDepth)) ts'
    where
      ts' = map minimax ts
      ps  = [(p, d) | Node (_, (p, d)) _ <- ts']
      best = if turn g == O 
             then minimum (map fst ps)  
             else maximum (map fst ps)  
      bestDepth = minimum [d | (p, d) <- ps, p == best]

-- Selects the best move: one that leads to the best 
-- possible outcome in the fewest number of moves
bestmove :: Grid -> Player -> Grid
bestmove g p = 
  head [g' | Node (g', (p', d)) _ <- ts, 
             p' == best, d == bestDepth]
    where 
      tree = prune depth (gametree g p)
      Node (_, (best, _)) ts = minimax tree
      bestDepth = minimum [d | Node (_, (p', d)) _ <- ts, 
                               p' == best]

-- ┌──────────────────────────────────────────────────────┐
-- │                        Game Loop                     │
-- └──────────────────────────────────────────────────────┘

-- Main entry point: starts the game with an empty 
-- grid and player O
tictactoe :: IO ()
tictactoe = do hSetBuffering stdout NoBuffering
               play empty O

-- Displays the grid and continues the game loop
play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

-- Main game logic for alternating turns between 
-- human and computer
play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = 
      do i <- getNat (prompt p)
         case move g i p of
            []   -> do putStrLn "ERROR: Invalid move"
                       play' g p
            [g'] -> play g' (nextPlayer p)
  | p == X   = 
      do putStr "Player X is thinking... "
         (play $! (bestmove g p)) (nextPlayer p)

-- Computes the maximum depth of a game tree
treeDepth :: Tree a -> Int
treeDepth (Node _ ts) = 
  case ts of
    [] -> 0
    _  -> 1 + maximum (map treeDepth ts)


-----------------------------------------------------------

{-
  We modified the minimax and bestmove functions to make 
  the computer player (Player X) choose the quickest path 
  to a win or draw, instead of simply selecting the first 
  or a random best move.

  Instead of just tagging each node with the winner 
  (Player), we also annotate it with the depth (i.e., how 
  many moves it takes to reach that outcome). So each
  node in the game tree now contains a tupple of 
  (Grid, (Player, Int)), where the Player is the winner
  and the Int is the depth of that win.

  When generating the game tree in the function minimax,
  both the Winner and the depth of the win are computed
  recursively. This is passed up the tree, so that each
  node knows the best outcome for that player and how 
  deep it is in the tree. The best outcome here is
  determined by the order of the Player values: if it's
  Player O's turn, we want to minimize the outcome, since
  we have the ordering O < B < X. For O, the best 
  nodes to choose are those labeled with O, then B, and
  in the worst case X. The same logic applies for Player 
  X, but in reverse: we want to maximize the outcome, so
  we choose nodes labeled with X first, then B, and
  in the worst case O.
  
  In the function bestmove, we then select the move that
  both leads to the best outcome (either a win or a draw)
  for the current player, and has the minimum depth.
  This way, the computer player will still always win or
  force a draw, but it will also try to do so in the
  least number of moves possible. 

-}