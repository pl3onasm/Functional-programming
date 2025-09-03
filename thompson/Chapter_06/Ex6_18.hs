type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.18

-- | Pads a picture with '.' characters so that
-- it becomes rectangular
padToRect :: Picture -> Picture
padToRect [] = []
padToRect pic =
  [ln ++ replicate (maxLen - length ln) '.' | ln <- pic]
  where 
    maxLen = maximum [length line | line <- pic]


-----------------------------------------------------------

{-

We had already defined this function in Ex6.8, in order to
test whether our two implementations of rotate90 were 
equivalent. For that purpose we needed to ensure that the 
pictures were rectangular since rotations only work 
correctly on rectangular pictures when implemented as lists 
of strings.

-}