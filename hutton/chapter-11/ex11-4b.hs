import Data.Char
import Data.List
import System.IO
import System.Random
import Control.Concurrent (threadDelay)

-----------------------------------------------------------
-- Exercise 11.4.b

-- The size of the tic-tac-toe grid 
size :: Int
size = 4

-- The length of a winning line
winLength :: Int
winLength = 3

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

-- Checks if a player has a winning line in the grid
wins :: Player -> Grid -> Bool
wins p g = any (hasWinLine p) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = diagonals g

-- Checks if a list contains winLength consecutive
-- occurrences of a player
hasWinLine :: Player -> [Player] -> Bool
hasWinLine p xs = any (all (== p)) (windows winLength xs)

-- Returns all contiguous sublists of length n
windows :: Int -> [Player] -> [[Player]]
windows n xs
  | length xs < n = []
  | otherwise     = take n xs : windows n (tail xs)

-- Collects all diagonals and anti-diagonals of a grid
diagonals :: Grid -> [[Player]]
diagonals g = ltr ++ rtl
  where
    ltr = [[g !! (r + i) !! (c + i) | 
            i <- [0..min (size - r - 1) (size - c - 1)]]
            | r <- [0..size - 1], c <- [0..size - 1]]
    rtl = [[g !! (r + i) !! (c - i) | 
            i <- [0..min (size - r - 1) c]]
            | r <- [0..size - 1], c <- [0..size - 1]]

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
-- │                     Making a move                    │
-- └──────────────────────────────────────────────────────┘

-- Checks if a move index is valid: should be within 
-- bounds and the grid cell should be Blank (B)
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
prompt p = "Your turn! Enter your move: "

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
  | won g     = []   -- No moves if the game is over
  | full g    = []   -- No moves if the grid is full
  | otherwise = 
      concat [move g i p | i <- [0..(size^2 - 1)]]

-- Limits the depth of the game tree to a fixed value
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- Maximum search depth
depth :: Int
depth = 3

-- ┌──────────────────────────────────────────────────────┐
-- │                        Minimax                       │
-- └──────────────────────────────────────────────────────┘

-- Minimax algorithm to evaluate the game tree
minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
  | wins O g  = Node (g,O) []
  | wins X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts) 
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps  = [p | Node (_,p) _ <- ts']

-- Selects the best move for the current player
-- It sorts the subtrees by their depth and returns the
-- first one that matches the best outcome for the player
-- This ensures that the computer player (X) chooses the
-- quickest path to a win or draw.
bestmove :: Grid -> Player -> Grid
bestmove g p = 
  head [g' | Node (g', p') _ <- ts, p' == best]
    where 
      ts = sortOn treeDepth ts'
      tree = prune depth (gametree g p)
      Node (_, best) ts' = minimax tree

-- computes the maximum depth of a game tree
treeDepth :: Tree a -> Int
treeDepth (Node _ ts) = 
  case ts of
    [] -> 0
    _  -> 1 + maximum (map treeDepth ts)

-- ┌──────────────────────────────────────────────────────┐
-- │                        Game Loop                     │
-- └──────────────────────────────────────────────────────┘

-- Main entry point: ask user if they want to play
-- first or second, then start the game
tictactoe :: IO ()
tictactoe = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Tic-Tac-Toe!"
  putStrLn "Do you want to play first or second?"
  putStrLn "1. First (O)"
  putStrLn "2. Second (X)"
  choice <- getLine
  case choice of
    "1" -> play empty O True    -- human is O
    "2" -> play empty O False   -- human is X
    _   -> do putStrLn "Invalid choice, enter 1 or 2"
              tictactoe         -- restart the game

-- Displays the grid and continues the game loop
play :: Grid -> Player -> Bool -> IO ()
play g p humanIsO = 
  do cls
     goto (1,1)
     putGrid g
     play' g p humanIsO

-- Main game logic for alternating turns between 
-- human and computer
play' :: Grid -> Player -> Bool -> IO ()
play' g p humanIsO
  | wins O g  = if humanIsO
                then putStrLn "You win! 🎉 🥳\n"
                else putStrLn "Computer wins! 😢\n"
  | wins X g  = if humanIsO 
                then putStrLn "Computer wins! 😢\n"
                else putStrLn "You win! 🎉 🥳\n"
  | full g    = putStrLn "🤝 It's a draw!\n"
  | isHuman p = 
      do i <- getNat (prompt p)
         case move g i p of
            []   -> do putStrLn "ERROR: Invalid move"
                       play' g p humanIsO
            [g'] -> play g' (nextPlayer p) humanIsO
  | otherwise = 
      do putStr "Computer is thinking... "
         threadDelay 1000000  -- simulates thinking time
         g' <- if g == empty  -- make first move random
               then do i <- randomRIO (0, size^2 - 1)
                       return (head (move g i p))
               else return (bestmove g p)
         (play $! g') (nextPlayer p) humanIsO
  where
    isHuman O = humanIsO 
    isHuman X = not humanIsO   

-----------------------------------------------------------

{-
  To run the game, first make sure to make the random
  package available:

  ghci> :set -package random

  Then load the file and start the game:
  ghci> :load ex11-4b.hs
  ghci> tictactoe

  The winning lines are 3 in a row, column, or diagonal.
  The grid size is 4x4. You can change both variables
  at the top of the file to adjust the game rules.

  Mind that if you increase the grid size, you also need
  to reduce the depth. Otherwise, the game will take too
  long to compute the best move.

-}