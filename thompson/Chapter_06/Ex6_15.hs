import Test.QuickCheck

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.15

-- | Inverts the colours of a picture
invertChar :: Char -> Char
invertChar ch = if ch == '.' then '#' else '.'

invertColour :: Picture -> Picture
invertColour pic = 
  [[invertChar ch | ch <- line ] | line <- pic]

-- | Property: inverting the colours of a picture twice
-- should return the original picture.
prop_InvertColour1 :: Picture -> Bool
prop_InvertColour1 pic =
  (invertColour . invertColour) pic == pic

-- | Property: same as previous but restricted to
-- pictures containing only '.' and '#'.
prop_InvertColour2 :: Picture -> Property
prop_InvertColour2 pic =
  all (all (`elem` ".#")) pic ==>
    (invertColour . invertColour) pic == pic


-----------------------------------------------------------

{-

A property that we would expect invertColour to have is 
that inverting the colours of a picture twice should
return the original picture.
This, however, is not true for randomly generated pictures
because they may contain characters other than '.' and '#'.
If we run quickCheck on prop_InvertColour1 we get:
ghci> quickCheck prop_InvertColour1
*** Failed! Falsifiable (after 2 tests and 1 shrink): 
[["a"]]

Indeed, the inner invertColour call changes the input into
[["."]], and the outer call then changes this into [["#"]].
Since [["#"]] is not equal to [["a"]], the property fails.

To fix this, we could restrict the input pictures to only
contain '.' and '#' characters.

This is done in prop_InvertColour2 using the QuickCheck
implication operator (==>). The property is now only
tested for pictures that satisfy the precondition
(all (all (`elem` ".#")) pic), i.e. all characters should
be either '.' or '#' in each line of the picture.
All has not been covered yet, but is in the standard
prelude, and can be defined using and (see fig 6.2):

  all :: (a -> Bool) -> [a] -> Bool
  all p xs = and [p x | x <- xs]

Testing in GHCi
ghci> :l Ex6_15
ghci> quickCheck prop_InvertColour2
*** Gave up! Passed only 45 tests; 1000 discarded tests.

The message indicates that only 45 out of 1000 randomly 
generated pictures satisfied the precondition. This is 
because chances of a randomly generated character being
either '.' or '#' are very low. This is why we got so
many discarded tests and why QuickCheck gave up: it did 
not manage to find 100 valid cases in the default 1000 
tests.

-}