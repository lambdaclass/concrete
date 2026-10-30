### Language specification: ARITHMETIC_POLICY, VALUE_MODEL and PATTERN_DESTRUCTURING absorbed (2026-10-30)

The three documents are design records under `docs/archive/design-records/`, and their rules are
entries: shift fill, division rounding and the remainder's sign (ARITH-8), no ambient arithmetic
mode, `#[overflow_checked]` included (ARITH-9), the named conversions' results (ARITH-10, pending
R-0500), the primitive numeric types and their spellings (TYPE-19), `char` as one byte (TYPE-20), a
newtype's representation (TYPE-21), unspecified layout and what `repr(C)` and `repr(packed)` promise
(FFI-7, FFI-8), the `match` a destructuring `let` means (PAT-10) and `let mut` patterns (PAT-11). Six
questions the documents raised against the spec or main are open entries for the owner: profiles
that omit or demand overflow checks, pointer and reference casts with `as`, the named conversions'
spellings, comparisons on a type parameter, the trap message, and the `Float32` and `Char` spellings.
The site's old value-model page redirects to the types chapter.
