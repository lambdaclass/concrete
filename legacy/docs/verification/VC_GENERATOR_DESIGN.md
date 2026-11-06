# Design: a weakest-precondition VC generator

Status: design, not implemented. Written before code deliberately — the thing being replaced
currently works, and improvising its replacement is how you get a fifth walker instead of a
calculus.

## The problem, stated as evidence rather than preference

Obligations are generated today by **four hand-written AST walkers**, one per runtime-safety
family (`collectDivisorsE`, `collectIndexUsesE`, `collectArithE`, `collectShiftsE`), each
recursing over the whole expression grammar independently.

Every discovery defect found in 2026-08 came from that structure:

| Defect | Cause |
|---|---|
| `(a)[i]` produced no bounds obligation | the walker matched a bare `.ident`, not a `.paren` |
| `b.data[i]` produced none | same, for `.fieldAccess` |
| `let a = [0; 16]` produced none | the size map read only annotated lets |
| array in a nested block produced none | the size map scanned only top-level statements |
| shadowed array sized from the wrong binding | resolution by a flat, function-wide name map |
| shadowed variable gave `x << 40` on an `i8` width 64 | the same, for types |
| an `#[overflow_checked]` addition silently dropped | type resolution failed, obligation vanished |

Two of those were **unsound** — a false claim proved by a kernel. All of them are the same shape:
*a case some walker did not cover, in a traversal that had to be written four times.*

Patching four walkers treats symptoms. A calculus removes the category: **you cannot forget a case
when you are not enumerating cases.** This is what Why3 does, and the comparison is why this
document exists (see ROADMAP, "Design honesty: this is largely Why3's architecture").

## Core idea: one traversal, N requirement rules

Today: **N walkers × the whole grammar**. Each new constructor must be added in N places, and the
gate that would catch a miss is file-granular (see `check_constructor_coverage`, which catches "no
walker handles X" but not "walker #2 forgot X").

Proposed: **one traversal × a rule table**.

```
vcExpr : Env → Expr → List Obligation      -- ONE recursion over the grammar
requires : Env → Expr → List Obligation    -- what THIS node needs, non-recursive
```

`vcExpr` walks the grammar exactly once and calls `requires` at every node; `requires` is a flat,
per-constructor table with no recursion in it:

| node | obligations it requires |
|---|---|
| `binOp .div l r` | `r ≠ 0`; and for signed types the `MIN / -1` quotient bound |
| `binOp .mod l r` | `r ≠ 0` |
| `binOp .shl/.shr l r` | `0 ≤ r < width(l)` |
| `binOp .add/.sub/.mul` | in-range for the type, when `#[overflow_checked]` |
| `arrayIndex a i` | `0 ≤ i < len(a)` |
| `cast e t` | representable in `t` (currently no obligation kind at all) |
| `unaryOp .neg e` | `e ≠ MIN` (currently no obligation kind at all) |

Adding a family becomes **a row**, not a walker. Adding an AST constructor forces one case in one
place, and the existing per-constructor coverage gate becomes meaningful rather than approximate.

Note the last two rows: `MIN` negation and float→int casts trap at runtime today with **no
obligation kind**. Under a rule table they are two entries, not two more walkers — which is
evidence the structure is right, since the current design makes them expensive enough to have been
skipped.

## Statements: the transformer proper

```
wp : Stmt → Post → (Post × List Obligation)
```

Standard weakest-precondition, with the safety side-conditions collected alongside:

- `x = e` → `(Post[x := e], vcExpr(e))`
- `if c then A else B` → obligations of `c`, plus `wp(A)` under `c` and `wp(B)` under `¬c`
- `s₁; s₂` → `wp(s₁, fst (wp s₂ Q))`
- `while c inv I do B` → the three standard obligations: `I` holds on entry, `I ∧ c` re-establishes
  `I`, and `I ∧ ¬c` implies the postcondition

**This subsumes `scopedWalk`.** The existing walker threads enclosing guards, negated guards,
fall-through and loop invariants by hand — that is a hand-rolled approximation of exactly what the
`if` and `while` rules above do for free. One mechanism replaces two.

**It also subsumes `ScopeDecls`.** The per-scope environment threaded for array lengths and
variable types is the transformer's environment. That work is already done and transfers directly.

## The decision that matters most: surface AST, or Core?

Today obligations are computed from the **surface AST**. H19 records the consequence: nothing
proves those obligations correspond to what is actually compiled.

Running the transformer over **Core** instead — after elaboration, desugaring and monomorphisation
— shrinks H19 substantially: the VCs would then be about the representation the backend lowers,
not about source text that later passes may transform.

Costs, stated so the choice is deliberate:

- Core is desugared, so obligations lose source-level shape and **diagnostics get worse** unless
  spans are threaded carefully;
- `CoreExtract` already extracts Core to Gallina, so a Core-level generator composes with the
  existing extraction path — an argument *for*;
- the surface-level contract expressions (`#[requires]`, `#[ensures]`) are surface-shaped, so a
  Core generator needs them elaborated too.

**Recommendation: Core.** H19 is the largest unproven gap in the trust story, and this is the one
change that attacks it structurally rather than by adding another cross-check.

## Why this makes Register A statable

Register A asks: *is the obligation sufficient to rule out the bug?* Today that is argued per
family, per row, because there is no single object to state it about.

With a calculus there is one theorem:

> if every obligation produced by `wp(body, True)` holds, the body executes without a trap

That is provable by induction over the transformer's rules — one proof, replacing five
half-discharged rows. It is the difference between arguing about four walkers and proving one
function correct.

## Migration: differential, not big-bang

The existing walkers are, as of 2026-08, believed correct — seven defects were found and fixed,
and each is now gated. That makes them a **usable oracle**.

1. Implement `wp`/`vcExpr` alongside the existing generators; ship nothing.
2. Add a differential report: run both over the corpus and diff the obligation sets by
   `(function, kind, conclusion)`. This mirrors `--report core-semantics-diff`, which already does
   exactly this shape of check for extraction.
3. **Every difference is a finding either way** — a walker missed something, or the transformer
   did. Neither is a nuisance; both are the point.
4. Switch consumers over only when the diff is empty or every remaining difference is explained in
   writing.
5. Delete the walkers, and with them the four-places-to-forget problem.

Step 3 is why this is worth doing even if the transformer turns out no better: the diff is a
second opinion on a layer that has produced two unsound claims this year.

## What it does NOT solve

- **Loops still need invariants.** WP does not invent them; the `while` rule above requires an `I`.
  So this forces the loop-invariant question rather than answering it — that is a separate roadmap
  item and a prerequisite for loop-carried obligations.
- **Termination is untouched.** WP is partial correctness. Liveness needs `#[decreases]` (rung 8).
- **Print fidelity is unchanged.** Still validated, never proved; the target's surface semantics
  are not ours.
- **It is not automatic proof.** It generates better obligations; discharging them is the same
  three kernels.

## Sequencing

1. per-walker constructor coverage (small; closes the file-granular limit in the current gate)
2. `requires` rule table + `vcExpr`, expressions only, differential against the four walkers
3. `wp` over statements, subsuming `scopedWalk`
4. move to Core, with spans threaded for diagnostics
5. the Register A soundness theorem over the transformer
6. delete the walkers

Steps 1–2 are independently valuable and reversible. The commitment point is step 4.
