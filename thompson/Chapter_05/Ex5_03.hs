module Chapter_05.Ex5_03 where

-----------------------------------------------------------
-- Exercise 5.3

-- | Returns x-intercept of a line and whether it exists
-- The line is given by the equation y = mx + c
xIntercept :: Float -> Float -> (Float, Bool)
xIntercept m c
  | m == 0    = (0, False)  -- horizontal line
  | otherwise = (-c / m, True)


-----------------------------------------------------------