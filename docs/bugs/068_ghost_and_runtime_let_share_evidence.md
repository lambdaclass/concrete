# Bug 068 — a ghost let and a runtime let produce the same body evidence

<!-- bug-ledger
summary: a ghost let and a runtime let produce the same body evidence
coverage: a ghost let and a runtime let produced IDENTICAL body evidence, so erasure was invisible to the subject digest and a proof over one stayed valid-looking after a change to the other. Found while scoping proof-context elaboration, not by a gate. FIX: EvidenceStmtV2.letBind carries isGhost, on the same principle as exprStmt's isValue. Gated by check_shadow_body_v2.sh; see docs/bugs/068 (2026-08-06)
class: 10
-->

**Status:** FIXED 2026-08-06
**Found:** 2026-08-06, while scoping the proof-context elaboration path that R-0004 step 2a
needs for `assert`/`assume`. Not found by a gate — found by asking what a ghost binding
looks like in evidence, which nothing had asserted.
**Class:** the R-0004 class — two different programs sharing one encoding.

## Witness

```concrete
mod m { pub fn f(p: Int) -> Int { ghost let g: Int = p + 1; return p; } }
mod m { pub fn f(p: Int) -> Int {       let g: Int = p + 1; return p; } }
```

| | `shadow bodyV2` |
|---|---|
| `ghost let` | `7f3048a954b6a90a0e416d7013afbb91` |
| `let` | `7f3048a954b6a90a0e416d7013afbb91` |

Identical. They are not the same program: a ghost binding is ERASED before Core, so
`p + 1` never executes — it cannot trap on overflow, and it is not a runtime value. The
runtime binding executes it.

## Cause

`Concrete/Elab/Elab.lean`, the `letDecl` case. Both paths end at the same evidence node:

```lean
if isGhost then
  addGhostVar name
  return ElaboratedStmtV2.mk [] (Proof.EvidenceStmtV2.letBind none cValEv.evidence)
addVar name finalTy
return ElaboratedStmtV2.mk [.letDecl ...] (Proof.EvidenceStmtV2.letBind none cValEv.evidence)
```

The Core lists differ (`[]` versus one statement) and the evidence does not. `isGhost` is
the whole difference between the two branches and it reaches no byte.

The binding half compounds it: a ghost let calls `addGhostVar` (a name list) and never
`addVar`, so it opens no scope slot. The evidence therefore emits a `letBind` — a node
whose name says *binding* — for something that binds no position.

## Why the digest matters here

Freshness. A proof over the runtime version stays valid-looking if the declaration is
changed to `ghost`, because the subject digest does not move. That is the precise failure
R-0004 exists to remove, arriving through a construct nobody had probed.

## Fixed

`EvidenceStmtV2.letBind` carries `isGhost : Bool`, on the same principle as `exprStmt`'s
`isValue` flag: the distinction that decides whether code runs belongs in the bytes.

Gated by `check_shadow_body_v2.sh` (`a ghost let and a runtime let do not share bytes`),
mutation-verified: hardcoding the flag to `false` fails that leg.

## Not fixed here, and tracked separately

A ghost binding still has **no scope position**, so a `ghost let` cannot be referenced by
a `binderRef` and an `assert` predicate that reads one has nothing to resolve against.
That is the ghost-binder-frame work R-0004 step 2a needs for `assert`/`assume`, and it is
a different change from making the two lets distinguishable. Recorded in `ROADMAP.md`.
