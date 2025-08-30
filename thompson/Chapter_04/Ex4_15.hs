-----------------------------------------------------------
-- Exercise 4.15

data Temp = Cold 
          | Hot
  deriving (Eq, Show, Ord)

data Season = Winter 
            | Spring 
            | Summer 
            | Autumn
  deriving (Eq, Show, Ord)

-- | Returns the typical temperature for a given season
-- assuming we are in the UK
seasonTemp :: Season -> Temp
seasonTemp Winter = Cold
seasonTemp Spring = Cold
seasonTemp Summer = Hot
seasonTemp Autumn = Cold


-----------------------------------------------------------