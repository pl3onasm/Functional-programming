import Control.Applicative
import Data.Char

-----------------------------------------------------------
-- Exercise 13.5

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

-- ┌──────────────────────────────────────────────────────┐
-- │                  Parsing expressions                 │
-- └──────────────────────────────────────────────────────┘

-- | Arithmetic expressions with +, *, 
-- and integer literals
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Val Int
  deriving (Eq, Show)

-- | Parses an expression:
-- a term optionally followed by + and another expression
-- Right-recursive, so it is right-associative
expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
           <|> return t

-- | Parses a term:
-- a factor optionally followed by * and another term
-- Right-recursive, so it is right-associative
term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
           <|> return f

-- | Parses a factor:
-- either a parenthesised expression or a natural number
factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> do 
            n <- natural
            return (Val n)

-- ┌──────────────────────────────────────────────────────┐
-- │                 Evaluating expressions               │
-- └──────────────────────────────────────────────────────┘

-- | Evaluates an expression to an Int
-- Recursively evaluates Add and Mul nodes
-- Leaves are Val nodes with integer values
evalExpr :: Expr -> Int
evalExpr (Val n)   = n
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Mul x y) = evalExpr x * evalExpr y

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
  In this version we introduce a new data type Expr
  that represents arithmetic expressions as an abstract
  syntax tree (AST). It has three constructors: Add
  for addition, Mul for multiplication, and Val' for
  integer literals.

  The parsers 'expr', 'term', and 'factor' now produce
  values of type Expr instead of Int. For example,
  'expr' returns 'Add t e' rather than 't + e', building
  a tree instead of performing the calculation during
  parsing.

  A function 'evalExpr' has been addded. It takes an
  'Expr' and recursively evaluates the AST to compute
  its numeric result. The 'eval' function has been
  left unchanged, but now it calls 'evalExpr'
  to evaluate the parsed expression.

  This design separates parsing and evaluation: the
  parser builds a structured representation, and 'evalExpr'
  interprets it later. This makes it easier to extend
  or transform expressions without changing the parser.
-}