### The Proof Model Computes More Operators (2026-10-24)

The extracted ProofCore model now has a value for more of the operators programs use, with the
semantics that ARITH-3 and ARITH-4 state ([PROVABLE_V1.md](docs/verification/PROVABLE_V1.md)):

- `div`, left shift and bitwise and at every integer width, and wrapping add at every width.
  Division is stuck on a zero divisor and, signed, on `MIN / -1`; a shift count that is negative
  or at least the width is stuck; these are stuck exactly where the runtime aborts;
- saturating add and subtract, clamped to the type's range;
- `&&` and `||` as conditionals, so the right operand is evaluated only when it decides the result,
  as at runtime;
- unary `-` (as `0 - x`, so at `MIN` the model has a value where the runtime aborts, which makes a
  proof about it partial-correctness) and `!`.

`check_model_diff.sh` gains leg O: one function per new operator class, each compared between the
model and the interpreter, with edge cases pinned and perturbed. Landed in train 13, where the binary-operator
preservation proof was restated to split out `&&` and `||`.
