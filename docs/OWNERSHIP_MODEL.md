# Ownership Model: Copy, Linear, Destroy

Status: implemented + gated. This is the canonical description of how Concrete
decides whether a value may be duplicated, whether it may be dropped, and how it is
consumed. It also records *why* the model is what it is, so the design fork behind it
does not have to be re-derived.

## The one law

**Concrete is linear: a non-Copy value must be used exactly once, and can never
silently disappear.** "Used" means consumed, moved, returned, or handed off. There is
no implicit destructor and no catch-all discard — if a value is not `Copy`, the
checker must see where it goes.

## Three distinct concepts (do not conflate them)

The bugs this model fixed all came from treating these as one:

| concept | question it answers | who has it |
|---|---|---|
| **Copy** | may I *duplicate* it? (use it twice) | primitives, `&T`, raw pointers, `fn` pointers, and structs/enums explicitly marked `Copy` (whose fields are all Copy — a `Copy` type may never own a resource) |
| **linear** (= non-Copy) | must I *account for* it exactly once? | everything not `Copy`: `String`, `File`, `Vec<T>`, `Heap<T>`, unmarked structs, `Option`/`Result`, protocol/witness tokens |
| **Destroy** | is there an *explicit consuming cleanup* verb? | resource owners that need teardown (`String`, `File`, …); it is a *verb*, not a structural property — a linear value may have one or not |

Two things follow that are easy to get wrong:

- **`Copy` answers "duplicable", not "droppable".** `&mut T` and `Option<i32>` are not
  duplicable, yet dropping them is harmless. They are *linear* here anyway, because
  Concrete's rule is about accounting, not just aliasing. If a value genuinely owns
  nothing and is safe to copy and drop, mark it `Copy`.
- **`Destroy` is not "the reason a value is linear".** A permit / receipt / proof
  witness owns no heap and has no `Destroy`, yet must not vanish — it is simply a
  linear value you must hand off. `Destroy` is only the *name of the cleanup* for the
  linear values that need one.

## The rules, by site

All "may this disappear?" checks key on **`isCopy`** (never on "owns a resource"):

- **`_` may ignore only a `Copy` value.** A wildcard arm (`match e { _ => {} }`) or a
  `_` payload field (`E::A { _ }`) over a **non-Copy** value is rejected (**E0288**,
  `wildcardDiscardsNonCopy`). `let _ = e;` is removed entirely (**E0289**).
- **A bare statement expression** discarding a non-Copy value is rejected
  (**E0287**); a `Result`/`Option` discard gets the must-use nudge (**E0286**). A
  deferred non-Copy-returning call (`defer make();`) is **E0287**.
- **A named linear local** left unconsumed at scope exit is rejected (**E0208**) —
  including locals in `if`/`else` branches and match arms (nested-scope checking),
  and including on `return`/`break` paths. `let g = f;` over a linear `f` **moves**
  it (`f` is consumed; using `f` afterward is **E0205**).
- **Exemption:** a block whose textual end is unreachable — a non-terminating
  `while true {}` (no break) or `abort()` — need not consume its locals (a server's
  accept loop legitimately holds a resource live forever). `return`/`break`/`continue`
  do *not* exempt.

### Conservation: a linear value flows to exactly one place

Every site a value can flow *into* **moves** (consumes) a linear operand exactly once —
duplicating one would let it be freed twice. Concretely:

- **`let g = f;`**, **function argument**, **`return f;`**, **match scrutinee** — all
  consume `f`; reuse afterward is **E0205**. **Stores consume too:** `*p = v` and
  `arr[i] = v` move a linear `v` into the slot (2026-07-02).
- **Linear rebind:** `acc = f(acc, x)` is legal — a `let mut` linear variable may be
  reassigned once its OLD value has been consumed (the fold/accumulate pattern),
  including inside loops (the binding is restored within the same statement).
  Overwriting a still-LIVE linear value stays **E0219**.
- **Matching through a reference borrows payloads:** `match &e { E::A { p } => … }`
  binds a non-Copy payload `p` as `&T` (a Copy, droppable view), never moving it out
  of the borrow; `_` may ignore any payload behind a reference. An owned scrutinee
  binds payloads owned, as before.
- **Array literal** `[a, b]` moves each element in — `a`/`b` are consumed, the array
  owns them (reuse is **E0205**). *(This was a real duplication hole — `[a, b]` used to
  leave `a`/`b` live, so they could be freed while the array also owned them. Fixed and
  locked by the conservation gate.)*
- **Struct literal** `Wrap { f: x }` moves `x` in.
- **Struct destructure** `let Wrap { f } = w;` moves the source `w` out and each named
  field becomes an **owned** binding (must itself be consumed). It is checked natively
  (a `let __destr = w; let f = __destr.f` desugaring would be unsound for a linear
  struct — field access does not move — so the linear check runs on the destructure
  form and it is expanded only at Elab, past the checker).
- **`let X::V { .. } = e else { … }` (let-else)** desugars to a catch-all `_` match arm,
  which is illegal over a **non-Copy** enum (the linear `_` rule). For a non-Copy /
  resource-owning enum, **use a full explicit `match`** instead of let-else.
- **`if let` / `while let`** desugar the same way and follow the same rule (decision
  re-confirmed 2026-07-01): over a **non-Copy** enum — including `Option<i32>`, since
  the discard rule keys on the TYPE's Copy-ness, not on whether the uncovered variants
  happen to own resources — they are rejected (**E0288**); write an explicit exhaustive
  `match`. Over a `Copy` enum they work as expected. If generic instantiations ever get
  conditional Copy (`Option<i32>` Copy when the payload is Copy — a natural part of
  stabilizing `std.option`), these shapes become legal for Copy payloads without
  bending the rule. Gated by `check_pattern_ergonomics.sh` (`neg_if_let_noncopy`).
- **Assigning to a non-Copy field** (`o.f = v`) is rejected (**E0219**): overwriting
  would leak the old linear value and cannot soundly move the new one in. Destructure
  and rebuild, or make the field `Copy`.
- **Functional update `S { ..base }`** may not *copy* a non-Copy field from `base`
  (**E0220**) — the value would be owned by both `base` and the result. Set each
  non-Copy field explicitly.
- **Known gap (KNOWN_HOLES H11, open):** projecting a non-Copy value *out* of a place
  by value — `let g = w.f;` or `let g = arr[i];` — currently copies instead of moving,
  so it can be owned twice (double-free). The borrow form `&w.f` is correct. The intended
  rule is "projecting a non-Copy sub-place by value is rejected — borrow it or
  destructure the whole place"; the fix is position-sensitive and scheduled.

### Control flow: how branches, arms, and loops account for consumption

Refined 2026-07-02 (the H12 std migration forced the checker to model these
precisely; all four rules are as-built in `Concrete/Check.lean`):

- **Fall-through branches must agree.** After `if`/`else` (or a `match`), the
  branches that reach the merge point must agree on what was consumed
  (E0212/E0209) — a value cannot be "maybe consumed" afterwards.
- **A diverging branch/arm is exempt from the merge** — it never reaches the
  merge point, so `Some { v } => { use(v) }, None => { drop(x); return 1 }` is
  sound. If EVERY arm diverges, the code after the match is unreachable and
  takes the first arm's state.
- **A branch that exits the function must consume everything live.** An early
  `return` is a function exit: `if c { return 0; } … drop(s);` leaks `s` on
  the returning path (E0208). Pending `defer`s count as consumption
  (they run on the unwind); borrows (`&`/`&mut`) are aliasing-exclusive, not
  consumption obligations, so letting one lapse is fine. `break`/`continue`
  keep values live at the loop boundary; `abort()`/infinite loops are exempt
  (the process never proceeds).
- **Consuming an outer linear inside a loop** is normally rejected (E0207 —
  the next iteration would see a moved value), EXCEPT inside a branch that
  exits the function: `while … { if bad { s.drop(); return 1; } }` is sound
  because no iteration follows the return. A loop nested *inside* that branch
  resets the exemption (it iterates before the return).
- Consequence for APIs: a generic function returning one of two linear
  arguments (`max<T>(a, b)`) always leaks the other — such helpers require
  `T: Copy` (`std.math.max/min/clamp`, the `std.test` asserts).

### How to intentionally get rid of a value

- **Copy value:** just drop it (`_`, bare statement, ignore it) — free.
- **Linear value:** account for it. Consume it (pass it on / return it / call its
  `destroy()`), or destructure it exhaustively and consume/hand off the parts. Inside
  an exhaustive match, a `_` on a **Copy** payload is fine:

  ```concrete
  match vec_pop::<i32>(&mut stack) {   // pop-and-drop, on purpose
      Option::Some { _ } => {},        // i32 payload is Copy — `_` may ignore it
      Option::None       => {},
  }
  ```

  A resource-bearing payload is non-Copy, so `Some { _ }` over it is rejected — bind
  and release it: `Option::Some { f } => { f.destroy(); }`.

## Why linear, not affine (the decision, 2026-07-01)

Concrete had drifted into using **two** predicates for "may this disappear": named
bindings used `!isCopy` (linear), but the `_` wildcard used `ownsResource` (affine —
it allowed a resource-free non-Copy value to be silently dropped). The result was an
incoherent middle band: `match e { _ => {} }` would drop an `Option<i32>` or a plain
view struct, while the *same value bound to a name* required consumption. Naming a
value was stricter than `_`-ing it.

Three coherent fixes were on the table:

1. **Copy-centric** — drop iff `Copy`. Correct axis, but needs conditional `Copy` for
   generics or `Option<i32>` feels artificially linear.
2. **Resource-centric** — must-consume iff `ownsResource`. Too weak: it cannot express
   affine/protocol **tokens** that own no heap yet must not vanish — central to a
   capability-based language.
3. **Linear (chosen)** — non-Copy = used exactly once; `_` ignores only `Copy`.
   Expresses tokens natively (a token is a linear value that owns nothing), one law,
   no implicit weakening.

This is textbook substructural typing: `Copy` is **contraction** (may use more than
once), discardability is **weakening** (may use zero times); a *linear* type permits
neither. Precedents: **Austral** (explicit linear, no implicit `Drop` — the closest
sibling) and **Linear Haskell** (multiplicities). Unlike Rust — which is affine with
an *implicit* `Drop` that runs cleanup for you — Concrete makes cleanup explicit, so
it must reject silent disappearance rather than paper over it with a destructor.

## Deliberately deferred: conditional `Copy`

`Copy` is currently a fixed per-declaration flag, so a generic container cannot be
`Copy` (marking `Option` `Copy` would wrongly make `Option<File>` `Copy` too). That
means `Option<i32>` is treated as linear, and `match opt_i32 { _ => {} }` is rejected
— you write the two-arm form. The principled convenience is **conditional `Copy`**:
`Option<T>` is `Copy` iff `T` is `Copy`. Then a catch-all `_` over `Option<i32>` would
be valid *for the right reason* — the whole value is genuinely unrestricted. This is a
future convenience, **not a foundation**; the linear rule stands without it.

## Where it lives

- Predicate: `Concrete/Check.lean` — `isCopyType` (the `Copy` decision) and the
  discard/consume checks that key on it (`checkScopeExit`, the per-block
  `checkBlockLocalsConsumed`, the match-arm `_` gate, the `Stmt.expr` discard check).
- Move-through-let and the divergence helpers (`blockNonTerminating`) are in the same
  file.
- Diagnostics: `E0205` use-after-move, `E0208` never-consumed, `E0286` discarded
  must-use, `E0287` discarded non-Copy statement, `E0288` `_`-drops-non-Copy
  (`wildcardDiscardsNonCopy`), `E0289` `let _` removed.
- Gates: `scripts/tests/check_linear_discard.sh` (the `_`/discard rules),
  `scripts/tests/check_linear_nested_scope.sh` (nested-scope + move-through-let +
  divergence), `scripts/tests/check_linear_conservation.sh` (every value-flow site
  moves a linear value exactly once — array-lit/struct-lit/destructure/arg/return/
  match; the anti-duplication backstop), `scripts/tests/check_ignored_result.sh`
  (`Result`/`Option` must-use).
- Related docs: `docs/KNOWN_HOLES.md` (H6 and H9 entries), `docs/IGNORED_RESULT.md`.
