module Chapter_07.Ex7_31 where

import Chapter_07.Ex7_29 hiding (joinLines)

-----------------------------------------------------------
-- Exercise 7.31

-- | Modified version to justify a single line of words to
-- exactly lineLen characters by adding extra spaces 
-- between words as evenly as possible
joinLine :: Line -> String
joinLine [] = ""
joinLine [w] = w
joinLine ws = justify ws gapSpaces extraSpaces
  where 
  totalChars = sum [length w | w <- ws]
  totalSpaces = lineLen - totalChars
  gaps = length ws - 1
  (gapSpaces, extraSpaces) = totalSpaces `divMod` gaps
  justify [w] _ _ = w
  justify (w : ws) base extras = w ++ replicate 
    (base + bonus) ' ' ++ justify ws base newExtras
    where
      bonus     = if extras > 0 then 1 else 0
      newExtras = extras - bonus

-- | Joins a list of lines into a single string, separating
-- lines with newline characters and justifying each line
-- to exactly lineLen characters
joinLines :: [Line] -> String
joinLines []       = ""
joinLines [l]      = joinLine l
joinLines (l : ls) = joinLine l ++ "\n" ++ joinLines ls 


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :load Ex7_31
ghci> words = splitWords text
ghci> lines = splitLines words
ghci> putStrLn (joinLines lines)
The  castle  seemed very quiet even
for a Sunday. Everybody was clearly
out  in the sunny grounds, enjoying
the  end  of  their  exams  and the
prospect of a last few days of term
unhampered by studying or homework.
Harry   walked   slowly  along  the
deserted  corridor,  peering out of
windows  as  he  went. He could see
people  messing  around  in the air
over  the  Quidditch  pitch  and  a
couple  of students swimming in the
lake,   accompanied  by  the  giant
squid.


-}