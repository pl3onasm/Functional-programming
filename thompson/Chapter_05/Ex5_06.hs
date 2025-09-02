-----------------------------------------------------------
-- Exercise 5.6

data ShopItem = Item Name Price
  deriving (Eq,Show)

type Name  = String
type Price = Float


-----------------------------------------------------------

{-

When we define shopItem as a data type, and not as a type
synonym, this creates a new type. This means that we cannot
use the functions defined for tuples, such as fst and snd
anymore. Instead we have to rely on pattern matching to
extract the components of a shopItem:

  getName :: ShopItem -> Name
  getName (Item name _) = name

  getPrice :: ShopItem -> Price
  getPrice (Item _ price) = price


The advantage is that we can now define functions that only
work on shopItems, and not on any tuple of the same 
structure (String, Float), which makes our code safer.
Also, we can expand the definition of shopItem later to
include more information, by adding more constructors or 
more fields to the existing constructor, without changing
the type of the functions that use shopItems. This is not
possible when using type synonyms.

-}