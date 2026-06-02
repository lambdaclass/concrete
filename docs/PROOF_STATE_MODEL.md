# ProofCore State Model

Status: design proposal (ROADMAP Phase 4 item 1).

This document fixes how `PExpr` and `eval` model state for mutation,
arrays, structs, loops, and failure.  It is the guiding contract for
all later extraction rules in Phase 4 — array index assignment,
richer while bodies, struct/field update, bounded buffers.  Without
this contract, each rule would invent its own encoding (the failure
mode `a93f68f`'s docs commit named).

## Scope

In scope:
- environment (variable) updates
- array read and array set
- struct field read and struct field update
- loop-carried state (while + for desugars)
- bounds assumptions on array indexing
- failure behavior (out-of-bounds, missing function, fuel exhaustion)

Out of scope (deliberately, named for the reader):
- references / borrows / aliasing.  `&mut` and `&` do not appear in
  PExpr.  Concrete's eligibility profile excludes references from
  proof-eligible code today; this is unchanged.
- concurrent mutation across tasks.  Phase 3's concurrency track
  owns this; ProofCore does not.
- persistent state across function calls.  Each `eval` starts in a
  fresh env; calls do not mutate caller state.
- heap allocation.  PVal arrays and structs are immutable values
  shaped like records, not pointers.

## Substrate: functional updates everywhere

PExpr is a pure value language.  Every "mutation" — assigning a
variable, writing to an array slot, updating a struct field — is
modeled by producing a *new* value and rebinding the name.  No
in-place edits.  No heap.  This is the only encoding that keeps
`eval : FnTable → Env → Nat → PExpr → Option PVal` total and pure.

Concrete's source-level mutation (`x = e`, `arr[i] = e`,
`obj.f = e`) extracts to functional-update PExpr shapes.  The
semantic gap (source has true mutation; PExpr has functional
updates) is bridged by the Phase 12 preservation obligations
named per-section below.

## 1. Environment updates

### Today

`Env = String → Option PVal`.  `Env.bind name v` returns a new env
where `name` maps to `v`.  `PExpr.letIn name val body` binds in the
body's env.  `PExpr.while_ cond assigns cont` uses `evalAssigns`,
which threads each `(name, expr) :: rest` by rebinding `name` after
evaluating `expr`.

### Decision

Keep `Env` as `String → Option PVal`.  Keep `Env.bind` as the only
update primitive.  Mutation (`x = e`) extracts to `Env.bind name v`
in the iteration env, exactly as `evalAssigns` already does.

Scoping: a `letIn` binding shadows any outer `name`.  An `assign`
inside a loop rebinds an already-bound `name` (does NOT introduce
a new scope); after the loop, the binding established by the last
iteration's last assign is what `cont` sees.

### Phase 12 obligation

`assignment_preservation`: for every source-level `x = e` in a
proof-eligible function body, the source's mutation of `x` after
`e` is equivalent (under the source operational semantics) to the
PExpr's `assigns` entry under `evalAssigns` (functional rebind).

## 2. Array read and array set

### Today

`PExpr.arrayIndex arr idx` evaluates `arr` to a `PVal.array_ elems`
and `idx` to `.int i`, returning `lookupIndex elems i.toNat` or
`none` if `i < 0` or out-of-bounds.

There is no `arraySet`.  Source-level `arr[i] = v` blocks extraction.

### Decision

Add `PExpr.arraySet (arr : PExpr) (idx : PExpr) (val : PExpr)`.
Evaluation:
- evaluate `arr` to `.array_ elems`, `idx` to `.int i`, `val` to `v`
- if `i < 0` or `i.toNat ≥ elems.length`: return `none` (out-of-bounds
  is *stuck*, not silently dropped — see § 5)
- otherwise return `.array_ (elems.set i.toNat v)`, using Lean's
  `List.set` (in-bounds replacement, out-of-bounds is identity, but
  the OOB branch is already ruled out by the prior check)

`PExpr.arraySet` returns a new array value; the original is
unchanged.  Source-level `arr[i] = v` (which mutates `arr`) extracts
to `assign "arr" (arraySet (var "arr") idx_expr val_expr)` — i.e.,
rebind `arr` to the updated array.

### Phase 12 obligation

`array_set_preservation`: for every source-level `arr[i] = v` in a
proof-eligible function body, the source's mutation produces an
array whose `j`th element is `v` if `j = i` and the original `j`th
element otherwise.  PExpr's `arraySet` produces exactly that array
by construction of `List.set`.  The obligation is mechanical.

## 3. Struct field read and struct field update

### Today

`PExpr.fieldAccess obj field` reads via `lookupField`.  There is no
`structSet`.  Source-level `obj.f = e` blocks extraction.

### Decision

Add `PExpr.structSet (obj : PExpr) (field : String) (val : PExpr)`.
Evaluation:
- evaluate `obj` to `.struct_ name fields`, `val` to `v`
- if `fields` has no entry named `field`: return `none` (Check
  forbids this; the safe-default is `stuck`)
- otherwise return `.struct_ name (fields.map (replacing the named
  pair with `(field, v)`))`

Source-level `obj.f = e` extracts to `assign "obj" (structSet
(var "obj") "f" e_expr)`.

### Phase 12 obligation

`struct_set_preservation`: analogous to array_set.

## 4. Loop-carried state

### Today

`PExpr.while_ cond assigns cont` uses `evalAssigns` to thread
updates.  Body must be flat `CStmt.assign` (no nested let/if/return
inside the loop body).  This is the ring_contains blocker.

### Decision: step-function form

Lift the body shape from "list of assigns" to a "step function" —
an inner PExpr that, given the current env, produces a new env (the
post-iteration state) or `none` (failure).  Conceptually:

    PExpr.while_step
      (cond : PExpr)
      (carried : List String)         -- which env keys are loop-carried
      (step : PExpr)                  -- evaluates to a PVal.struct_ shape
                                      -- whose fields are the new values
                                      -- for `carried` names (and only those)
      (cont : PExpr)

The `step` PExpr can contain `letIn`, `ifThenElse`, etc. — anything
that produces a `PVal.struct_` of the loop-carried names.  Returns
inside the loop body are NOT modeled here (break/early-return inside
a while is a separate extension; deferred).

Existing flat-assign `while_` becomes a special case where `step` is
a sequence of `letIn`s ending in a `structLit` of the carried names.
The extractor can keep emitting flat-assign form for simple loops
and step-function form for richer bodies; the eval rule unifies both
by reading the resulting struct's fields back into the env.

### Phase 12 obligation

`while_step_preservation`: for every source-level `while cond { body
}` in a proof-eligible function body, where `body` reads only the
loop-carried names and reassigns only those names, the source's N
iterations produce the same loop-carried name → value map as PExpr's
`while_step` with `step` extracted from `body`.

## 5. Bounds assumptions and failure

### Today

- Out-of-bounds array read: `eval` returns `none`.
- Missing function in `FnTable`: `eval` of a `call` returns `none`.
- Fuel exhaustion: `eval` returns `none`.

All three failures are indistinguishable from "this expression is
ill-typed" at the `Option PVal` level.

### Decision: keep `none` as "stuck"; no exceptions

For now, all failure modes remain `none`.  No `PVal.error` or
`Result`-like wrapper.  Theorems that need to prove "the function
succeeds" pass hypotheses that rule out the stuck cases (e.g.,
"the buffer has at least N elements," "fuel ≥ K").  This is what
`ring_new_correct` and `compute_tag_zero_correct` already do.

The alternative (introducing a typed failure value) is a bigger
refactor that would touch every existing theorem.  Defer until a
candidate actually forces it.

### Phase 12 obligation

`failure_preservation`: source-level out-of-bounds is undefined
behavior (panics in debug builds; UB in release).  PExpr's `none`
encodes "we don't claim anything about this case."  The preservation
claim is one-directional: when PExpr returns `some v`, the source
does NOT exhibit UB and produces `v`; when PExpr returns `none`, the
source MAY exhibit UB and we make no claim.  This is the only honest
match between the source's UB semantics and PExpr's `Option`
semantics.

## 6. What the state model does NOT decide

- Width / signedness of arithmetic.  That's the multi-width PBinOp
  item (ROADMAP Phase 4 item 7), independent of this doc.
- Whether `PVal` becomes typed.  PVal stays dynamically shaped; type
  discipline is enforced by Check on the source side and by the
  Phase 12 preservation obligations above on the proof side.
- Reference semantics.  References are not in PExpr.
- Atomics / concurrency.  Phase 3 owns concurrency.

## 7. Applied: ring_push and ring_contains

### ring_push

Source:

    fn ring_push(rb: RingBuf, val: i32) -> RingBuf {
        let cap: i32 = 16;
        let mut d: [i32; 16] = rb.data;
        d[rb.head % cap] = val;
        let new_head: i32 = (rb.head + 1) % cap;
        let new_count: i32 = if rb.count < cap { rb.count + 1 } else { cap };
        return RingBuf { data: d, head: new_head, count: new_count };
    }

Under the state model:

    letIn "cap" 16
    letIn "d" (fieldAccess (var "rb") "data")
    -- d[rb.head % cap] = val
    letIn "d" (arraySet (var "d")
                  (binOp .mod (fieldAccess (var "rb") "head") (var "cap"))
                  (var "val"))
    letIn "new_head" (binOp .mod
                        (binOp .add (fieldAccess (var "rb") "head") 1)
                        (var "cap"))
    letIn "new_count" (ifThenElse
                        (binOp .lt (fieldAccess (var "rb") "count")
                                   (var "cap"))
                        (binOp .add (fieldAccess (var "rb") "count") 1)
                        (var "cap"))
    structLit "RingBuf"
      [ ("data",  var "d")
      , ("head",  var "new_head")
      , ("count", var "new_count")
      ]

The `let mut d = ...; d[...] = ...` mutation becomes two sequential
`letIn`s, the second one binding `d` to `arraySet (var "d") ...`.
Functional update encoded as shadowing.

### ring_contains

Source:

    fn ring_contains(rb: RingBuf, val: i32) -> i32 {
        let cap: i32 = 16;
        let scan: i32 = if rb.count < cap { rb.count } else { cap };
        for (let mut i: i32 = 0; i < scan; i = i + 1) {
            let idx: i32 = ((rb.head - rb.count + i) + cap * 2) % cap;
            if rb.data[idx] == val { return 1; }
        }
        return 0;
    }

The loop body has a nested `let idx = ...` and an early `return 1`.
That's the shape the current `while_` flat-assign form rejects.  Under
the step-function form, the body becomes a step PExpr that:
- reads loop-carried `i` and the immutable `rb`, `val`, `scan`, `cap`
- evaluates `idx`
- if `rb.data[idx] == val`, signals "early-return with value 1"
- otherwise produces new loop-carried env `{ i = i + 1 }`

The early-return shape requires a small extension: the `step` can
either produce a `PVal.struct_` of carried names (continue) or a
sentinel `PVal.struct_ "__early_return__" [("value", v)]` (break with
value).  The `while_step` eval rule checks for the sentinel and
returns its value directly, bypassing `cont`.  Alternative: a `Sum`
typed step result with explicit Cont/Break variants.  Choose the
latter (Result-shaped); the sentinel approach is fragile.

This is the only piece of the state model that needs richer typing.
The PExpr addition is:

    PExpr.while_step
      (cond : PExpr) (carried : List String)
      (step : PExpr) (cont : PExpr)

with the convention that `step` evaluates to either:
- `.enum_ "LoopStep" "Cont"   [(name, new_value), ...]`
- `.enum_ "LoopStep" "Break"  [("value", v)]`

Reusing existing `PVal.enum_` keeps PExpr minimal.

### Phase 12 obligation

`while_step_preservation` per § 4, plus a new
`while_step_early_break`: when the source loop returns from inside
the body, PExpr's `step` produces `.enum_ "LoopStep" "Break"` with
the same returned value.

## 8. Open questions deferred to forcing examples

- Multi-level nested loops (loops inside loops): same step-function
  shape applied twice.  Defer until a candidate forces it.
- Continue (`continue` jumps to step): introduce a third variant
  `LoopStep::Skip` with the carried-name updates from step-only.
  Defer until forced.
- Break with label: defer.
- Loop invariants: a `while_step` extension that takes an inductive
  invariant PExpr would enable richer theorems.  Defer until manual
  loop reasoning becomes painful enough to force it.

## 9. Summary contract

| Source shape | PExpr shape | New eval helper | Phase 12 obligation |
|---|---|---|---|
| `x = e` (rebind) | `assign "x" e` (inside `while_`) | `evalAssigns` (exists) | `assignment_preservation` |
| `let x = e` | `letIn "x" e body` (exists) | n/a | n/a (already pure) |
| `arr[i] = v` | `assign "arr" (arraySet ...)` | `arraySet` eval rule (new) | `array_set_preservation` |
| `obj.f = v` | `assign "obj" (structSet ...)` | `structSet` eval rule (new) | `struct_set_preservation` |
| `while cond { body }` flat | `while_ cond assigns cont` (exists) | `evalAssigns` (exists) | `while_step_preservation` |
| `while cond { body }` rich | `while_step cond carried step cont` (new) | `evalWhileStep` (new, uses Cont/Break enum) | `while_step_preservation` + `while_step_early_break` |
| `arr[i]` (read) | `arrayIndex arr idx` (exists) | `lookupIndex` (exists) | already covered |
| `obj.f` (read) | `fieldAccess obj f` (exists) | `lookupField` (exists) | already covered |

## Review checklist

Before any of this lands as code:

- [ ] § 1: `Env.bind` as the only env update — agreed?
- [ ] § 2: `arraySet` returns new array, OOB is stuck — agreed?
- [ ] § 3: `structSet` returns new struct, missing field is stuck — agreed?
- [ ] § 4: step-function form for richer while bodies, with `LoopStep` enum for Cont/Break — agreed? (alternative: keep flat-assign and reject richer bodies harder)
- [ ] § 5: `none` remains the universal failure value; no typed errors yet — agreed?
- [ ] Names and signatures (`PExpr.arraySet`, `PExpr.structSet`, `PExpr.while_step`) — final?

Once the boxes are checked, the implementation order is:
1. `arraySet` + Phase 12 stub for `array_set_preservation` → unblocks `ring_push`
2. `structSet` + Phase 12 stub
3. `while_step` + `LoopStep` enum convention + Phase 12 stubs → unblocks `ring_contains`

Each step is small, additive, and self-contained.  No existing
extraction rule changes.  Existing theorems unaffected.
