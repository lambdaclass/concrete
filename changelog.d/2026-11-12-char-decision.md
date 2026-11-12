### Char decision recorded (2026-11-12)

Record the owner’s decision to keep `char` without a literal, construct it through explicit
checks, and expose the three approved code-point/ASCII signatures. Single quotes remain ASCII
`u8` byte literals, constants have no special rule, and SPARK-style preconditions instead of
`Option` remain open. Close waiting item 5 and mark OD42’s mixed char/byte example superseded;
revise the affected decision rows and foundation proposal. Lean is unchanged.
