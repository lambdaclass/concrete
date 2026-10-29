# Bug 066 — the interpreter discards a match guard's effects

<!-- bug-ledger
summary: the interpreter discards a match guard's effects
coverage: the interpreter discarded a match guard's environment, so a guard's output and mutations vanished while its VALUE still selected the arm; return values agreed, so only an OUTPUT differential caught it. Gated by check_match_guard_effects.sh (12 checks: succeeding guard, failing guard's effects surviving the fall-through, two failing guards in order, a guard whose pattern never matches, outer-mutation persistence, arm-local mutation NOT leaking, binder-frame restoration with a rebinding control). Mutation-verified on the EFFECTS axis (threading the env only on success fails 3 legs). The binding-LEAK axis is prevented upstream, so its mutation legitimately survives rather than indicating missing coverage: code owner Elab.bindArmVar alpha-renames every match payload binder to a fresh Core name (x -> x.b0, verified with --emit-core), gated by regress_045_match_binder_shadow.con (run_ok 42) in run_tests.sh. If that owner or gate changes, the leak mutation becomes killable and check_match_guard_effects.sh's restoration leg starts doing real work -- a 045/066 coupling, also recorded in the bug doc (2026-08-04)
class: 7
ruling: Violates: unrecorded
-->

**Status:** FIXED 2026-08-04
**Found:** 2026-08-04, while measuring match evaluation order for R-0004's evidence
producer (guards evaluated only after the pattern matches; first successful guarded arm
wins).
**Class:** interp/compiled differential — the same class as the 2026-06-27 sweep.

## Witness

```concrete
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn guard1() with(Console) -> Bool { print("GUARD-RAN\n"); return true; }
    pub fn main() with(Console) -> Int {
        let e: E = E::A { v: 7 };
        match e {
            E::A { v } if guard1() => { print("took-guarded\n"); return 1; },
            E::A { v } => { print("took-fallback\n"); return 2; },
            E::B { v } => { return 0; }
        }
    }
}
```

| | guard effect | arm taken |
|---|---|---|
| interpreter (`--interp`) | **`GUARD-RAN` missing** | took-guarded |
| compiled | `GUARD-RAN` printed | took-guarded |

Both select the correct arm. Only the guard's OBSERVABLE EFFECT differs, which is why
a return-value-only comparison would not have caught it.

## Cause

`Concrete/Interp/Interp.lean`, in the match-arm walker:

```lean
let guardOk := fun (armEnv : Env) (guard : Option CExpr) =>
  match guard with
  | none => Except.ok true
  | some g => do
    let (_, gv) ← evalExprVal fns enums armEnv g   -- <-- env DISCARDED
```

`evalExprVal : ... → Except String (Env × IVal)` returns the updated environment, which
carries accumulated output and any state the expression touched. Every other call site
threads it forward (`let (env, v) ← evalExprVal ...`); this one alone drops it with `_`.
So the guard is evaluated, its VALUE is used to select the arm, and everything else it
did is thrown away.

## Why it matters beyond printing

`Env` is not just an output buffer. Anything a guard mutates is lost, so a guard that
increments a counter, or whose call mutates through a `&mut`, leaves no trace — while
the compiled binary performs it. The interpreter is the differential oracle, so a
divergence here weakens every differential test that involves a guarded arm.

## Fixed

`guardOk` now returns `(Env × Bool)` and all four arm forms thread it: the success path
evaluates the body in the guard's environment, and the FALL-THROUGH path carries it
forward after dropping only the arm's own bindings. Gated by
`scripts/tests/check_match_guard_effects.sh` (5/5), which is mutation-verified —
restoring the discard fails three legs with the exact divergence.

## The two axes, and why only one is mutation-killable

**Axis 1 — discarded effects.** Real, fixed, and killed behaviorally: threading the
env only on the success path fails three legs.

**Axis 2 — leaked arm bindings.** Structurally prevented UPSTREAM, not by this fix.
Bug 045 alpha-renames every match payload binder to a fresh Core name (`x` becomes
`x.b0`, verified with `--emit-core`). The owners are concrete and executable, not
prose: the code owner is `Elab.bindArmVar` (fresh name from `env.freshBinder`), and the
guarding fixture is `regress_045_match_binder_shadow.con` (run_ok 42) in
`run_tests.sh`. So a leaked arm binding cannot shadow an outer
variable of the same spelling — the names differ. Leaking the whole arm environment on
failure therefore SURVIVES mutation: it is not behaviorally observable.

The `drop`-to-outer-length restoration is kept as environment hygiene and as defence if
that upstream property ever changes, but it must not be described as gated. If match
binders ever stop being alpha-renamed, this mutation becomes killable and the leg
`a failed arm's binding does not shadow the outer variable` starts doing real work —
today it passes for the upstream reason, not because of the restoration.

This is a genuine coupling between bug 045 and bug 066, recorded so a future change to
either does not silently weaken the other.

## Fix shape (as diagnosed)

`guardOk` must return the updated environment alongside the boolean
(`Except String (Env × Bool)`), and each arm must thread it into the body's evaluation —
including the FALL-THROUGH path, since a guard that fails has still run its effects and
the next arm must start from the environment it left behind.

The fall-through case is the subtle half: it is tempting to thread the env only when the
guard succeeds, which would silently discard the effects of every failed guard.

## Gate

A differential fixture asserting interpreter and compiled output agree for:

1. a guard whose effect runs and whose arm is taken;
2. a FAILING guard whose effect runs before falling through to a later arm;
3. two failing guards, so the ORDER of discarded-then-restored environments is checked;
4. a guard on a non-selected arm that must not run at all (pattern did not match).

Case 2 is the one the current code would still fail after a naive fix.
