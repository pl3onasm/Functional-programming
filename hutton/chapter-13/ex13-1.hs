import Control.Applicative
import Data.Char
import Data.Functor (void)

-----------------------------------------------------------
-- Exercise 13.1

-- | A simple backtracking parser that returns zero or more
-- (result, remaining-input) pairs
newtype Parser a = P (String -> [(a, String)])

-- | Functor instance lets you map a pure function over the 
-- result of a parser
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

-- | Parses a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- | Parses a specific string
string :: String -> Parser String
string []     = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- | Parses a single character that satisfies the predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- | Succeeds only at end of input
-- Consumes nothing
eof :: Parser ()
eof = P (\inp -> case inp of
                   [] -> [((), "")]
                   _  -> [])

-- | Parses a single-line Haskell comment
-- Leading whitespace is allowed
-- Consumes trailing newline if present, or succeeds at EOF
comment :: Parser ()
comment = do
  many (sat isSpace)
  string "--"
  many (sat (/= '\n'))
  (void (char '\n')) <|> eof
  return ()

-----------------------------------------------------------

{-
  The comment parser
  - fails when the input doesn’t start with --
  - succeeds if the comment ends at EOF
  - consumes the trailing newline when it is there
  
  Examples:

  ghci> :l ex13-1.hs
  ghci> parse comment "-- hello"
  [((),"")]
  ghci> parse comment "hello"
  []
  ghci> parse comment "hello\nworld!"
  []
  ghci> parse comment "-- hello\nworld!"
  [((),"world!")]
  ghci> parse comment "   --   hello\nworld!"
  [((),"world!")]

  The first example succeeds because the parser found a
  string starting with "--" followed by EOF
  The second and third examples fail (empty list) because
  the input does not start with "--"
  The fourth example succeeds because the parser found a
  string starting with "--" followed by a newline. It
  leaves the rest of the input unconsumed.
  The fifth example shows that the parser can handle
  leading whitespace before the "--" and still succeeds.

  Note that we had to use void to ignore the result of
  the char parser, since we are only interested in the
  side effect of consuming the newline character and the
  use of <|> requires both parsers to return the same type.

  We could have defined void ourselves as:
  void :: Functor f => f a -> f ()
  void x = fmap (const ()) x
-}