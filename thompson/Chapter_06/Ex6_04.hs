module Chapter_06.Ex6_04 where

-----------------------------------------------------------
-- Exercise 6.4

superimposeChar :: Char -> Char -> Char
superimposeChar ch1 ch2 
    | ch1 == '.' && ch2 == ch1 = '.'
    | otherwise  = '#'


-----------------------------------------------------------