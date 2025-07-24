import Data.Char
import Data.List
import System.IO
import Data.Ord (comparing)
import Control.Concurrent (threadDelay)

-----------------------------------------------------------
-- Exercise 11.4.c

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
nextPlayer X = O
nextPlayer B = B

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
    ps = concat g
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

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
diag g = [g !! n !! n | n <- [0..size-1]]

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
    where
      bar = [replicate ((size * 4) - 1) '-']

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
goto :: (Int, Int) -> IO ()
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
  where
    (xs, B : ys) = splitAt i (concat g)

-- Converts a flat list into rows of n elements each
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- ┌──────────────────────────────────────────────────────┐
-- │                     Reading Input                    │
-- └──────────────────────────────────────────────────────┘

-- Prompts the user to enter a natural number
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do putStrLn "ERROR getNat: Invalid number"
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
  | won g     = []
  | full g    = []
  | otherwise = 
      concat [move g i p | i <- [0 .. (size^2 - 1)]]

-- Finds a node in the game tree that matches the given 
-- grid. If the grid is not found, an error is raised.
findNode :: Grid -> Tree (Grid, Player) -> 
            Tree (Grid, Player)
findNode g node@(Node (g', _) ts)
  | g == g'   = node
  | otherwise = 
      case filter (\(Node (g'', _) _) -> g'' == g) ts of
        [t] -> t
        _   -> error "ERROR findNode: grid not found"

-- ┌──────────────────────────────────────────────────────┐
-- │                        Minimax                       │
-- └──────────────────────────────────────────────────────┘

-- Precomputes the game tree for the empty grid and 
-- player O, applying the minimax algorithm to it
-- This avoids recalculating the tree during the game
cachedTree :: Tree (Grid, Player)
cachedTree = minimax (gametree empty O)

-- Minimax algorithm to evaluate the game tree
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps  = [p | Node (_, p) _ <- ts']

-- Finds best next move using the precomputed tree
bestmove :: Tree (Grid, Player) -> Tree (Grid, Player)
bestmove (Node (g, _) ts) =
  case turn g of
    O -> minimumBy (comparing score) ts
    X -> maximumBy (comparing score) ts
  where
    score (Node (_, p) _) = p

-- ┌──────────────────────────────────────────────────────┐
-- │                        Game Loop                     │
-- └──────────────────────────────────────────────────────┘

-- Main entry point: starts the game with an empty 
-- grid and player O
tictactoe :: IO ()
tictactoe = do
  hSetBuffering stdout NoBuffering
  play cachedTree

-- Displays the grid and continues the game loop
play :: Tree (Grid, Player) -> IO ()
play tree@(Node (g, _) _) = do
  cls
  goto (1,1)
  putGrid g
  play' tree

-- Main game logic for alternating turns between 
-- human and computer
play' :: Tree (Grid, Player) -> IO ()
play' tree@(Node (g, _) ts)
  | wins O g = putStrLn "You win! 🎉 🥳\n"
  | wins X g = putStrLn "Computer wins! 😢\n"
  | full g   = putStrLn "🤝 It's a draw!\n"
  | turn g == O = do
      i <- getNat (prompt O)
      case move g i O of
        [] -> do putStrLn "ERROR: Invalid move"
                 play' tree
        [g'] -> play (findNode g' tree)
  | turn g == X = do
      putStr "Computer is thinking... "
      threadDelay 1000000  -- simulates thinking time
      play (bestmove tree)
