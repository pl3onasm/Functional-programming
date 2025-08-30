-----------------------------------------------------------
-- Exercise 4.16

data Season = Winter 
            | Spring 
            | Summer 
            | Autumn
  deriving (Eq, Show, Ord)

data Month = Jan 
           | Feb 
           | Mar 
           | Apr 
           | May 
           | Jun 
           | Jul 
           | Aug 
           | Sep 
           | Oct 
           | Nov 
           | Dec
  deriving (Eq, Show, Ord)

-- | Returns the season for a given month
-- assuming we are in the northern hemisphere
monthSeason :: Month -> Season
monthSeason Dec = Winter
monthSeason Jan = Winter
monthSeason Feb = Winter
monthSeason Mar = Spring
monthSeason Apr = Spring
monthSeason May = Spring
monthSeason Jun = Summer
monthSeason Jul = Summer    
monthSeason Aug = Summer
monthSeason Sep = Autumn
monthSeason Oct = Autumn
monthSeason Nov = Autumn


-----------------------------------------------------------