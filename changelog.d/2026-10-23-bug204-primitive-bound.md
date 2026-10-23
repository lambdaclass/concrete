### A Bound On A Primitive Type Is Checked: Bug 204 (2026-10-23)

A type argument such as `Int` for a parameter bounded by a user trait now needs an impl of that
trait (`impl Tr for i64` serves `Int` too), or the call is refused with E0241. Before, the check
skipped primitive types: an unused bound compiled and ran, and a used one failed only in the
backend. Pinned by `error_204_primitive_bound_unchecked.con`, `error_204_primitive_bound_unused.con`
and `regress_204_primitive_bound_satisfied.con`.
