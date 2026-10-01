# Bug 073 — generic type-argument arity was not checked

**Status:** FIXED 2026-10-01 (branch `r0484-struct-caps`)
**Found:** 2026-10-01, while sizing capability arguments on structs for R-0484: kind
checking needs to know how many arguments a generic type takes, and nothing checked it.
**Severity:** wrongly accepted programs. Not an authority or memory-safety escape on its
own, but it would have let a capability argument be silently dropped once struct
capability parameters exist.

## Symptom

```con pseudocode
struct Copy Box1<T> { v: T }
let b: Box1<i32, bool> = Box1::<i32, bool> { v: 7 };   // compiled, ran, returned 7
fn first(p: Pair<i32>) -> i32                          // for Pair<A, B>
```

An extra type argument was dropped without a diagnostic. A missing one surfaced only as
an unrelated linearity error (E0208 "linear variable 'p' was never consumed"), because
the unsubstituted parameter stayed a type variable and was treated as non-`Copy`.

## Mechanism

`checkTyDeep` (`Concrete/Resolve/Resolve.lean`) verified that a generic type's name was
known and recursed into its arguments, but never compared the argument count with the
declaration. Later substitution zipped parameters with arguments, so surplus arguments
were discarded and missing ones left their parameter unsubstituted.

## Fixed

Resolve checks every explicit type-argument list against the declaration — user
structs, enums and newtypes by their `typeParams`, the builtins `Heap`, `HeapArray`,
`Vec`, `Option` (1) and `Result` (2) — in type annotations and in struct-literal
turbofish. A mismatch is E0113 ("'Box1' takes 1 type argument, but 2 were given"). The
declaration wins over the builtin table, so a user-defined `Result` is checked against its
own arity. A name whose declaration is not visible to Resolve is skipped rather than
guessed, so the check cannot reject a valid program it cannot see.

## Regression witness

`tests/programs/error_type_arg_count_extra.con` and `error_type_arg_count_missing.con`
(both must report E0113), and `type_arg_count_ok.con` as the positive control (correct
arities, including `Option` and `Result`, compile and return 7). Registered in
`run_tests.sh`.
