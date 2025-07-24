-----------------------------------------------------------
-- Exercise 8.7

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False


instance Eq a => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  _ == _ = False

-----------------------------------------------------------

{-
  The first instance defines equality for the Maybe type.
  The type variable a must be an instance of the Eq
  typeclass, meaning values of type a can be compared
  for equality.

  The implementation checks the two possible forms of
  Maybe values:

  - If both values are Nothing, they are equal.

  - If both are Just x and Just y, they are equal if
  x == y.

  - If one is Nothing and the other is Just, they are
  not equal.

  The second instance defines equality for lists.
  Again, the elements must belong to the Eq typeclass,
  so they can be compared pairwise.

  The implementation handles the following cases:

  - If both lists are empty, they are equal.

  - If both are non-empty, they are equal if the first
  elements are equal and the remaining elements are
  equal too.

  - In all other cases (e.g., one list is empty and the
  other is not, or unequal elements), the lists are not
  equal.

-}