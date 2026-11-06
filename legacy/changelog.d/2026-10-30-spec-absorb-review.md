### Language specification: the review of the docs/language absorption (2026-10-30)

Codex and Kimi reviewed the absorption of the last docs/language documents. Where an entry chose
something its source left open, or claimed more than main does, it is narrowed and the choice is an
open owner decision: the result type of `try_convert` (OPEN-106), what a float-to-integer rounding
gives for NaN, infinities and out-of-range values (OPEN-107), a capability listed twice in a set
(OPEN-108), specializations that differ only in a function type's capabilities (OPEN-109), enums at
the foreign boundary (OPEN-110), misaligned fields of a packed struct (OPEN-111), mutation through a
shared reference (OPEN-112), the ownership exceptions of trusted code (OPEN-113) and how integer
division rounds (OPEN-114). ARITH-8 now states only the fill of a right shift, ARITH-10 no longer
chooses `Option` and keeps `reinterpret` to a change of signedness, ARITH-11 (pending R-0500) states
the rounding of the named float conversions, TYPE-D1 gives capability parameters to functions and
structs only, TYPE-17 compares capability sets without regard to order, TYPE-19 states each integer
type's width and signedness, MEM-D1 no longer says a Copy binding is never frozen, and TRUST-12 names
the move, cleanup and freeze rules rather than every ownership rule. OPEN-94 is withdrawn: MOD-1
already refuses a name in `with(...)` that is no capability, and main's acceptance is bug 337. PAT-11
discloses bug 336 (the binders of a destructuring `let` without `mut` are assignable). New examples
cover capability loss in argument and field position, trusted code's move, cleanup and freeze
refusals, field offsets, integer widths, every byte through `char`, newtype wrapping and the named
arithmetic operations under `#[overflow_checked]`.
