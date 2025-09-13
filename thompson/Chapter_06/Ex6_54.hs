-----------------------------------------------------------
-- Exercise 6.54

type Suit = String
type Value = String
type Card = (Value, Suit)
type Deck = [Card]

fullDeck :: Deck
fullDeck = 
  [(v, s) | v <- ["2", "3", "4", "5", "6", "7", "8", "9", 
                  "10", "Jack", "Queen", "King", "Ace"], 
            s <- ["Clubs", "Diamonds", "Hearts", "Spades"]]


-----------------------------------------------------------

{-

In the previous exercise we used data constructors to
represent Suit and Value. The upside of this is that we get
type safety: we cannot create a Card with an invalid Suit 
or Value. By deriving the Enum + Bounded type classes, we
can also easily enumerate all possible Suits and Values.
Pattern matching on data constructors is also very clear
and type-safe. For example:

    isRed :: Suit -> Bool
    isRed Hearts   = True
    isRed Diamonds = True
    isRed _        = False

In turn, we used type synonyms to represent Card and Deck.
A card is just a pair of a Suit and a Value, so no need for
a new data constructor. Similarly, a Deck is just a list of
cards.

The alternative presented here is to use type synonyms for
everything. The upside is that the code is simpler and more
light-weight. The downside is that we lose type safety: a 
Card could be ("Joker", "Stars"), which is not a valid 
card. We also lose the ability to easily enumerate all 
possible Suits and Values, and the ordering is not enforced 
by the type system: now "Ace" < "2" < "10" < "3" is True, 
because strings are compared lexicographically. Of course,
we could use custom comparison functions, but that is more
cumbersome than using derived Ord instances on data types,
which gives us the correct ordering out of the box. 

We also lose the benefits of pattern matching: now we have 
to use string comparison, which is more error-prone and 
less efficient. For example, to check if a suit is red, we 
have to do:

    isRed :: Suit -> Bool
    isRed s = s == "Hearts" || s == "Diamonds"

With data, pattern matching is exhaustive and the compiler
can warn us if we miss a case. With strings, we have to be
careful to cover all possible variations. A typo will
result in a bug that the compiler cannot catch (e.g.
"Diamond" instead of "Diamonds").

Testing in GHCi
ghci> :l Ex6_54
ghci> fullDeck
[("2","Clubs"),("2","Diamonds"),("2","Hearts"),
("2","Spades"),("3","Clubs"),("3","Diamonds"),
("3","Hearts"),("3","Spades"),("4","Clubs"),
("4","Diamonds"),("4","Hearts"),("4","Spades"),
("5","Clubs"),("5","Diamonds"),("5","Hearts"),
("5","Spades"),("6","Clubs"),("6","Diamonds"),
("6","Hearts"),("6","Spades"),("7","Clubs"),
("7","Diamonds"),("7","Hearts"),("7","Spades"),
("8","Clubs"),("8","Diamonds"),("8","Hearts"),
("8","Spades"),("9","Clubs"),("9","Diamonds"),
("9","Hearts"),("9","Spades"),("10","Clubs"),
("10","Diamonds"),("10","Hearts"),("10","Spades"),
("Jack","Clubs"),("Jack","Diamonds"),("Jack","Hearts"),
("Jack","Spades"),("Queen","Clubs"),("Queen","Diamonds"),
("Queen","Hearts"),("Queen","Spades"),("King","Clubs"),
("King","Diamonds"),("King","Hearts"),("King","Spades"),
("Ace","Clubs"),("Ace","Diamonds"),("Ace","Hearts"),
("Ace","Spades")]

-}