import System.IO

-----------------------------------------------------------
-- Exercise 10.6

-- reads a character from the input without echoing it
-- and returns it
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

-- wrapper for readL
-- initializes ReadL with an empty string
readLine :: IO String
readLine = do
  readL ""

-- Reads a line of input, handling backspaces 
-- returns the line as soon as the user presses Enter
readL :: String -> IO String
readL xs = do
  ch <- getCh
  case ch of
    '\n' -> do          -- newline character
      putChar '\n'
      return (reverse xs)
    '\DEL' ->           -- backspace character
      if null xs then
        readL xs
      else do
        putChar '\b'    -- move cursor back
        putChar ' '     -- overwrite character
        putChar '\b'    -- move cursor back again
        readL (tail xs)
    _ -> do             -- any other character   
      putChar ch
      readL (ch : xs)

-----------------------------------------------------------

{-
  This program implements a version of getLine that shows
  the characters as you type and supports backspace 
  editing.

  The function getCh reads a single character from the
  standard input without echoing it to the screen. It does
  this by temporarily disabling echoing and then restoring 
  it right after reading the character. This allows us to
  read newline and backspace characters without them being
  displayed immediately.

  The function readLine is the main entry point for reading
  a line of input. It is basically a wrapper around the
  recursive function readL, as it initializes the reading 
  process with an empty string.

  Reading a line of input is done by the function readL,
  which takes an accumulator string (initially empty) and
  reads characters one by one. 

  - If the character is a newline, it outputs a newline and
  returns the reversed accumulator as the final input line.

  - If the character is a backspace and there is something 
  to delete, it removes the last character from the 
  accumulator and visually erases the character from the 
  terminal by moving the cursor back, overwriting with it 
  with a space, and moving the cursor back again. If there 
  is nothing to delete, it simply continues reading without 
  modifying the accumulator.

  - Otherwise, it outputs the character and prepends it to
  the accumulator, then continues reading.

  The final reversal ensures the input is in the correct 
  order, since characters are accumulated at the front 
  during reading.

  Note that this implementation has linear time complexity
  with respect to the length of the input line, as it
  performs a linear scan of the input and constructs the
  resulting string by prepending characters, which is 
  then reversed at the end. 
  This would be different if we had used the append
  operator (++) instead of the cons operator (:), as you
  would then have to traverse the entire accumulator
  each time you append a character, leading to a
  quadratic time complexity instead!

-}
