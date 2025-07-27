import Control.Applicative
import Data.Char

-----------------------------------------------------------
-- Exercise 13.6

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
                    []      -> []
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
string (x:xs) = do char x
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
-- Right-recursive, so it is right-associative
expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
           <|> do 
             symbol "-"
             e <- expr
             return (Sub t e)
           <|> return t

-- | Parses a term:
-- a factor optionally followed by * or / and another term
-- Right-recursive, so it is right-associative
term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
           <|> do 
             symbol "/"
             t <- term
             return (Div f t)
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

-- | Evaluates an expression from a string input
-- Returns the result if the parse is successful and
-- there is no leftover input
eval :: String -> Int
eval xs =
  case parse expr xs of
    [(e, "")]  -> evalExpr e
    [(_, out)] -> error ("Unused input " ++ out)
    []         -> error "Invalid input"

-----------------------------------------------------------

{-
  In this version we have extended the parser to support 
  subtraction and division, and to use integer values 
  rather than natural numbers, based upon the following 
  revisions to the grammar:

        expr ::= term ( + expr | - expr | ε )
        term ::= factor ( * term | / term | ε )
        factor ::= ( expr ) | integer
        int ::= ... | -1 | 0 | 1 | ...
  
  The Expr data type now includes Sub and Div 
  constructors for subtraction and division, respectively. 
  The evalExpr function has been updated to handle these
  new operations.

  In order to also parse subtraction and division, we
  modified the expr and term parsers to handle
  subtraction and division operations. The factor parser
  was also updated to allow for integer values, including
  negative integers.
  
-}

-----------------------------------------------------------