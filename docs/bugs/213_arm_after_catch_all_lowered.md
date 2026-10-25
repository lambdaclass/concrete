# Bug 213 — a match arm after an unguarded catch-all arm failed SSA verification

<!-- bug-ledger
summary: a match arm following an unguarded binding or `_` arm in Core order (after CoreCanonicalize moves unguarded catch-alls last) was lowered into a predecessor-less block, so its uses of earlier values failed SSA verification natively (E0703) while `--interp` ran the program. Lower stops at the first unguarded catch-all arm. Pinned by `regress_213_arm_after_binding_arm.con` (binary and --interp). Found by the fuzz farm's mutation generator
fixtures: regress_213_arm_after_binding_arm.con
class: 8 — an arm after an unguarded catch-all lowered into a block no edge reaches
silent: no
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the fuzz farm's mutation generator on `concrete-ci-2` (findings
`crash-5ff5654bc7eb` and `crash-7dec4da326b6`: a mutated literal pattern became a binding, `u32 =>`
or `v0 =>`, ahead of the `_` arm), reproducing at main `eb122a01`; reduced in the post-R-0492
differential hunt.
**Severity:** loud. The binary failed in the SSA verifier (E0703, "use of %t3 defined in
non-dominating block 'entry'") while `--interp` ran the program.

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD18/exit 2n: the SSA failure is reproducible from the diff, but whether this unreachable-arm program should execute or be refused remains undecided.

## Reproducer

`tests/programs/regress_213_arm_after_binding_arm.con` reduces to:

```con
enum Copy E { A { v: i32 }, B { w: i32 } }
fn main() -> Int {
    let e0: E = E::A { v: 7 };
    let k: i32 = 1;
    let r: i32 = match k { 0 => 1, y => 2, _ => match e0 { E::A { v } => v, E::B { w } => w } };
    return r as Int;
}
```

Observed on main: E0703 at `ssa-verify` natively; `--interp` returned 2. Expected: 2 from both.
The source admits an arm after a catch-all (PAT-3 refuses only a repeated variant), and
CoreCanonicalize keeps unguarded catch-all arms last but in their written order, so two catch-alls
in a row reach Lower.

## Cause

The `.match_` case of `Lower.lowerExpr` lowers an unguarded binding or `_` arm with an unconditional branch into the arm
and then opens the next arm's check block, which therefore has no predecessor. The following arm
was lowered into that block, so its uses of values defined earlier (the scrutinee's aggregate, a
loaded local) sat in a block the entry does not dominate: SSAVerify treats a predecessor-less
block as its own root.

## Fix

Both arm loops of that case (enum and non-enum scrutinee) stop at the first unguarded binding
or `_` arm: the arms after it can never be selected and are not lowered. The interpreter already
selected the first matching arm. Pinned by `regress_213_arm_after_binding_arm.con` (42 natively
and under `--interp`, three shapes: a value match, a statement match whose arms return, an enum
match), with the farm's two minimized programs reduced into it.

## Structural cause

Class 8 ("storage, places and control flow decided ad hoc in `Lower`"): which arms are reachable
was not decided before lowering, so Lower built blocks for arms no edge reaches, the bug 078 / 175
shape of an unreachable region holding real uses.

## Structural owner

R-0501 (S10 Place MIR: match lowering over a decision tree whose unreachable arms are pruned once),
with R-0504 (S12 validators).
