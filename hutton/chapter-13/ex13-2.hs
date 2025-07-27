-----------------------------------------------------------
-- Exercise 13.2

{-
  The second grammar for arithmetic expressions is as 
  follows:

              expr ::= expr + expr | term
              term ::= term * term | factor
              factor ::= ( expr ) | nat
              nat ::= 0 | 1 | 2 | ...

  This version takes account of operator precedence, but
  it does not handle associativity of operators, leaving
  an expression such as 2 + 3 + 4 ambiguous, which can
  be interpreted as either (2 + 3) + 4 or 2 + (3 + 4).

  The corresponding parse trees are:

               expr                     expr
              /  |  \                /   |   \
          expr   +  expr         expr    +    expr
        /  |  \       |            |        /  |  \
      expr + expr   term         term   expr   +  expr
        |      |      |            |      |        |
      term   term  factor       factor   term     term
        |      |      |            |      |        |
     factor  factor  nat          nat   factor   factor
        |      |      |            |      |        |
       nat    nat     4            2     nat      nat
        |      |                          |        |
        2      3                          3        4 

  The first parse tree corresponds to the left-associative
  interpretation, while the second corresponds to the
  right-associative interpretation.

  Allowing this ambiguity in the grammar means that we
  can parse expressions like 2 + 3 + 4 or 2 * 3 * 4
  in two different ways, which is a problem for a parser 
  that expects a single, unambiguous parse tree for each 
  expression.

-}

-----------------------------------------------------------