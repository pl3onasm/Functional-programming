import Control.Applicative
import Data.Char
import System.IO

-----------------------------------------------------------
-- Exercise 13.9

-- ┌──────────────────────────────────────────────────────┐
-- │                 Setting up the parser                │
-- └──────────────────────────────────────────────────────┘

-- | A simple backtracking parser that returns zero or more
-- (result, remaining-input) pairs
newtype Parser a = P (String -> [(a, String)])

-- | Runs a parser on a given input string by
-- stripping the constructor from the Parser type
-- and applying the function it contains
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- | Parses and returns a single character
-- Fails on empty input
item :: Parser Char
item = P (\inp -> case inp of
                    []        -> []
                    (x : xs)  -> [(x, xs)])

-- ┌──────────────────────────────────────────────────────┐
-- │                  Sequencing parsers                  │ 
-- └──────────────────────────────────────────────────────┘

-- | Functor instance lets you map a pure function over  
-- the result of a parser
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []         -> []
                          [(v, out)] -> [(g v, out)])

-- | Applicative instance allows sequencing of independent 
-- parsers
instance Applicative Parser where
  -- pure :: a -> Parser a
  -- Wraps a value without consuming any input
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- Parses a function, then a value, and applies them
  pg <*> px =
    P (\inp -> case parse pg inp of
                 []         -> []
                 [(g, out)] -> parse (fmap g px) out)

-- | Monad instance allows dependent sequencing
-- (allows later parsers to depend on earlier results)
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  -- Runs the first parser, feeds its result to the 
  -- function, and continues
  p >>= f = P (\inp -> case parse p inp of
                         []         -> []
                         [(v, out)] -> parse (f v) out)

-- ┌──────────────────────────────────────────────────────┐
-- │                    Making choices                    |
-- └──────────────────────────────────────────────────────┘

-- | Alternative instance gives us failure (empty) and 
-- choice (<|>)
instance Alternative Parser where
  -- empty :: Parser a
  -- Always fails
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- Tries the left parser; if it fails, 
  -- tries the right one on the same input
  p <|> q = P (\inp -> case parse p inp of
                         []         -> parse q inp
                         [(v, out)] -> [(v, out)])

-- ┌──────────────────────────────────────────────────────┐
-- │                    Handling space                    |
-- └──────────────────────────────────────────────────────┘

-- | Consumes zero or more whitespace characters
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- | Runs a parser, trimming leading and trailing 
-- whitespace
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- | Parses an identifier (lowercase letter followed by 
-- zero or more alphanumerics)
identifier :: Parser String
identifier = token ident

-- | Parses a natural number, discarding surrounding 
-- whitespace
natural :: Parser Int
natural = token nat

-- | Parses an integer (optional leading minus),  
-- discarding surrounding whitespace
integer :: Parser Int
integer = token int

-- | Parses a specific symbol (string), discarding 
-- surrounding whitespace
symbol :: String -> Parser String
symbol xs = token (string xs)

-- ┌──────────────────────────────────────────────────────┐
-- │                  Derived primitives                  │ 
-- └──────────────────────────────────────────────────────┘

-- | Parses a single character satisfying a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- | Parses a decimal digit
digit :: Parser Char
digit = sat isDigit

-- | Parses a lowercase letter
lower :: Parser Char
lower = sat isLower

-- | Parses an uppercase letter
upper :: Parser Char
upper = sat isUpper

-- | Parses an alphabetic character
letter :: Parser Char
letter = sat isAlpha

-- | Parses an alphanumeric character
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | Parses a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- | Parses a specific string
string :: String -> Parser String
string []     = return []
string (x : xs) = do char x
                     string xs
                     return (x : xs)

-- | Parses an identifier: one lowercase letter 
-- followed by zero or more alphanumerics
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x : xs)

-- | Parses a natural number (one or more digits)
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- | Parses an integer: a natural number optionally
-- preceded by a minus sign
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- ┌──────────────────────────────────────────────────────┐
-- │                  Parsing expressions                 │
-- └──────────────────────────────────────────────────────┘

-- | Arithmetic expressions with +, -, *, /, and integer
-- literals
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Val Int
  deriving (Eq, Show)

-- | Parses an expression:
-- a term optionally followed by + or - and another expr
-- Subtraction is implemented as left-associative
expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
           <|> do 
             ts <- many (do symbol "-"
                            term)
             return (foldl Sub t ts)
           <|> return t

-- | Parses a term:
-- a factor optionally followed by * or / and another term
-- Division is implemented as left-associative
term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
           <|> do 
             ts <- many (do symbol "/"
                            factor)
             if any (\x -> evalExpr x == 0) ts
             then empty  -- Avoid division by zero
             else return (foldl Div f ts)
           <|> return f

-- | Parses a factor:
-- either a parenthesised expression or an integer value
factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> do 
            n <- integer
            return (Val n)

-- ┌──────────────────────────────────────────────────────┐
-- │                 Evaluating expressions               │
-- └──────────────────────────────────────────────────────┘

-- | Evaluates an expression to an Int
-- Recursively evaluates the expression tree
-- Leaves are Val nodes with integer values
evalExpr :: Expr -> Int
evalExpr (Val n)   = n
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Sub x y) = evalExpr x - evalExpr y
evalExpr (Mul x y) = evalExpr x * evalExpr y
evalExpr (Div x y) = evalExpr x `div` evalExpr y

-- ┌──────────────────────────────────────────────────────┐
-- │                 Terminal I/O functions               │
-- └──────────────────────────────────────────────────────┘

-- | Reads a character from the input without echoing it
-- and returns it
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

-- | Clears the terminal screen (ANSI escape code)
cls :: IO ()
cls = putStr "\ESC[2J"

-- | Moves the cursor to a specific position (x, y)
goto :: (Int,Int) -> IO ()
goto (x,y) = 
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- | Writes a string at a specific position (x, y)
writeat :: (Int,Int) -> String -> IO ()
writeat (x,y) xs = do
  goto (x,y)
  putStr xs

-- ┌──────────────────────────────────────────────────────┐
-- │                 Calculator user interface            │
-- └──────────────────────────────────────────────────────┘

-- | ASCII art representing the calculator layout
box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

-- | A string containing all valid button characters
buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

-- | Draws the calculator box on the screen
showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | 
                     (y,b) <- zip [1..] box]

-- | Formats a string with ANSI escape codes for bold 
-- blue text used for highlighting the input area
boldB :: String -> String
boldB s = "\ESC[1;34m" ++ s ++ "\ESC[0m"

-- | Formats a string with ANSI escape code for green text
-- used for displaying status messages
grn :: String -> String
grn s = "\ESC[32m" ++ s ++ "\ESC[0m"

-- | Formats a string with ANSI escape code for red text
-- used for displaying error messages
red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- | Clears the status line at the bottom of the calculator
clearStatus :: IO ()
clearStatus = writeat (1,15) (replicate 50 ' ')

-- | Displays a string in the calculator's input area,
-- clearing any previous content first
display :: String -> IO ()
display xs = do 
  writeat (3,2) (replicate 13 ' ')
  writeat (3,2) (boldB (reverse (take 13 (reverse xs))))

-- ┌──────────────────────────────────────────────────────┐
-- │                 Calculator main loop                 │
-- └──────────────────────────────────────────────────────┘

-- | Clears the screen, shows the calculator box,
-- and waits for user input
run :: IO ()
run = do 
  cls
  showbox
  clear

-- | Resets the input area and prompts the user
-- for new input
clear :: IO ()
clear = do 
  writeat (1,15) (grn "Waiting for input...")
  calc []

-- | Main event loop: displays current input, waits for a
-- key press, validates it, and processes it accordingly
calc :: String -> IO ()
calc xs = do 
  display xs
  c <- getCh
  if elem c buttons 
  then do
    if elem c "\n=qQ\ESC\BS\DELdcDC "
    then clearStatus
    else do
      clearStatus 
      writeat (1,15) (grn ("You pressed: " ++ [c]))
    process c xs
  else
    do 
      writeat (1,15) (red ("❌ Invalid button: " ++ [c]))
      calc xs

-- ┌───────────────────────────────────────────────────────┐
-- │                 Processing user input                 │
-- └───────────────────────────────────────────────────────┘

-- | Handles a single key press by invoking the
-- appropriate action (quit, delete, evaluate, etc.)
process :: Char -> String -> IO ()
process c xs 
  | elem c "qQ" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

-- | Exits the calculator by displaying a goodbye message
quit :: IO ()
quit = do 
  goto (1,15)
  putStr (grn "Goodbye!\n\n")
  return ()

-- | Deletes the last character from the current input,
-- or resets the input if empty
delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

-- | Attempts to parse and evaluate the current input.
-- On success, displays the result. 
-- On failure, shows an error.
eval :: String -> IO ()
eval xs = 
  case parse expr xs of
    [(n,[])] -> calc (show (evalExpr n))
    [(_,out)] -> do
      writeat (1,15) (red ("Error around position "
                      ++ show (length xs - length out + 1) 
                      ++ "."))       
      calc xs
    [] -> do
      writeat (1,15) (red ("Error: Invalid input."))
      calc xs

-- | Appends a character to the current input string
-- and redisplays it
press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])


-----------------------------------------------------------

{-
  To run the calculator:

  ghci> :load ex13-9.hs
  ghci> run

  Then you can enter expressions in the display and 
  evaluate them by pressing Enter or the = key.
  Press q or Q to quit.
  Use d, D, BS, or DEL to delete the last character.
  Use c, C to clear the input.

  Some example inputs and their outputs:  
  
    41 * (2 + 3) - 5
      200
    
    (10 - 2) - 3 * 4
      -4
    
    48 / 6 +* 2
      Error around position 8.

    -2--16 /2
      6

    + + + + + 
      Invalid input.

  Note that we have also implemented the solution discussed
  in exercise 13.8, which allows for left-associative
  parsing of subtraction and division by using the foldl 
  function to combine terms. Implementations of the + and
  * operators remain right-associative, which is fine 
  because the operations are associative
  
  This change means that expressions like "1 - 2 - 3" are 
  now parsed as "(1 - 2) - 3", yielding the expected result
  of -4, rather than "1 - (2 - 3)" which would yield 2. The 
  same applies to division, where "8 / 4 / 2" is now parsed
  as "(8 / 4) / 2", yielding the expected result of 1, 
  rather than "8 / (4 / 2)" which would yield 4.

-}