-----------------------------------------------------------
-- Exercise 3.19

-- | takes a digit (0-9) and returns its representation
-- in Roman numerals
digitToRoman :: Char -> String
digitToRoman '1' = "I"
digitToRoman '2' = "II"
digitToRoman '3' = "III"
digitToRoman '4' = "IV"
digitToRoman '5' = "V"
digitToRoman '6' = "VI"
digitToRoman '7' = "VII"
digitToRoman '8' = "VIII"
digitToRoman '9' = "IX"
digitToRoman  _  = ""


-----------------------------------------------------------

{-

You can also use guards or case expressions
to define the function.

-}