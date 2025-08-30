-----------------------------------------------------------
-- Exercise 4.9

-- | Returns the largest of three integers and how many 
-- times it occurs
maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs a b c = (m,count)
  where 
    m = max a (max b c)
    count = cnt a + cnt b + cnt c
    cnt x = if x == m then 1 else 0


-----------------------------------------------------------