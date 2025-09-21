module Chapter_08.Ex8_04 where

import Chapter_08.Ex8_01
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------
-- Exercise 8.4

-- | A strategy is a function that takes a list of the
-- opponent's previous moves and returns the next move.
type Strategy = [Move] -> Move

-- | Generates a random integer in the range 0..n-1
randomInt :: Integer -> IO Integer
randomInt n = randomRIO (0,n-1)

-- | Generates a random Move
randomMove :: () -> Move
randomMove () = 
  case unsafePerformIO (randomInt 3) of
    0 -> Rock
    1 -> Paper
    _ -> Scissors

-- | Given a Move, returns a Move that beats at least
-- one of the other two Moves.
safeMove :: Move -> Move
safeMove Rock     = Scissors
safeMove Paper    = Rock
safeMove Scissors = Paper

-- | A strategy that plays a random move except when the
-- opponent has played the same move twice in a row,
-- in which case it plays the move that beats at least  
-- one of the other two moves.
notToLose :: Strategy
notToLose []  = randomMove ()
notToLose [_] = randomMove ()
notToLose (m1 : m2 : _)
  | m1 == m2  = safeMove m1
  | otherwise = randomMove ()


-----------------------------------------------------------

{-

This implementation follows Thompson's method of generating
a random integer within the IO monad, then extracting the
random number from the IO monad using unsafePerformIO (he
does this in the code that comes with the book).
We avoid his time-based tricks and instead use randomRIO.
This is not the best way to do it, but we have not yet
covered the IO monad properly (admittedly, it is strange 
that Thomspon wants us to use unsafePerformIO before we 
have covered the IO monad at all).

The function randomMove has a dummy argument of type ()
so that it is not a top-level constant. If it were a top-
level constant, Haskell would evaluate it only once, and
the same move would be returned every time. By making it a
function, we force Haskell to re-evaluate unsafePerformIO
each time randomMove is called, generating a new random
move each time.

Testing in GHCi

ghci> :set -i..
ghci> :set -package random
ghci> :l Ex8_04
ghci> notToLose []
Paper
ghci> notToLose [Rock]
Scissors
ghci> notToLose [Rock,Rock]
Scissors
ghci> notToLose [Rock,Rock]
Rock
ghci> notToLose [Rock,Paper,Scissors]
Paper  
ghci> notToLose [Rock,Paper,Scissors,Scissors]
Scissors
ghci> notToLose [Rock,Paper,Scissors,Scissors,Scissors]
Paper
ghci> notToLose [Scissors,Scissors,Scissors]



-}