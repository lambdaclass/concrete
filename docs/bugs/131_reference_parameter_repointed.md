# Bug 131 — a reference parameter can be re-pointed at a block-local value that dies before it is read

**Status:** FIXED 2026-10-21 (batch 2, `land2-fix`)
**Found:** 2026-10-21, by the semantics probes (`param_repoint.con`, while evaluating "references
only as parameters and in borrow blocks").
**Severity:** memory safety in safe code. A reference parameter is an assignable binding on main, so
`r = &local` inside a block is accepted and the later `*r` reads a dead stack slot. It is the
parameter variant of bug 104 (a `let`-bound reference local re-pointed the same way).

## Reproducer

`tests/regressions/bug131_reference_parameter_repointed/repro.con`:

```con
fn g(r: &Int) -> Int {
    if true {
        let local: Int = 7;
        r = &local;
    }
    return *r;
}

fn main() -> Int {
    let a: Int = 1;
    return g(&a);
}
```

On main (`5b63cd87`): accepted; the binary exits 7, read from `local`'s slot after its block has
ended (the slot happens to still hold 7, so the exit looks plausible; it is a dead read). `--interp`
fails `interp: ref path base 'local' not in env`: the interpreter's environment knows `local` is gone,
the checker does not. Expected: a checker diagnostic at `r = &local`.

Re-pointing one parameter at another, `fn f(r: &Int, q: &Int) -> Int { r = q; return *r; }`, is also
accepted on main (both engines return 41 for `f(&a, &b)` with `b = 41`); it is not a dangling read,
since both sources outlive the call, but it is the same unchecked reassignment.

**On `land2-fix`** (`ce4632b5`, the branch head, built from a clean export of that revision): both
programs are refused with E0238 at the assignment ("cannot trace what reference 'r' borrows: a
reassignment may not re-point a reference at different storage ... use a new binding"), the
normalized place rule of bug 091. So batch 2 closes this bug together with 104; MEMORY_SEMANTICS §15
REF-6 already lists "a reference parameter re-pointed at a local" among its rejected shapes, with
status **land2**.

## Cause

`Concrete/Check/Check.lean`: `checkFn` adds every parameter as a mutable binding, and the `.assign`
case of `checkStmt` records no provenance for a reference assigned to an existing binding (bug 104's
cause: `borrowedFrom` is recorded only by `let x = &y`). The E0234 assignment check covers only
borrow-block references. Nothing compares the scope of `local` with the scope of the binding the
reference is stored in, and for a parameter that binding outlives every block of the body.

## Fix

Batch 2's normalized place rule (bug 091 round 3, the re-pointing rule `recordRefProvenance
(reassign := true)` that also fixes bug 104): a reassignment may not re-point a reference at
different storage, a parameter included. Both programs above are refused at the assignment
(E0238): `error_131_param_repointed_block_local.con` (the reproducer) and
`error_131_param_repointed_param.con` (`r = q`). The open-bug reproducer row is dropped.

## Structural cause

No reference-provenance model in the source checker ([structural class](STRUCTURAL_CLASSES.md) 4,
with 091, 092, 104 and 105): a reference's source is attached to the syntactic form that created
it, so an assignment moves the reference without its provenance. It is also borrow tracking by
syntax (class 5): each check recognizes the binding forms it was written for, and the parameter
form was not among the forms bug 104's record names.

## Structural owner

R-0499 (S8, queue row 4: references carry provenance through every assignment and merge, and a
loan's place must outlive every binding that can hold it), with the rule stated by exit 2i (R-0500,
REF-6: a reference binding, parameters included, is never re-pointed). Under exit 0b the repair is
batch 2's normalized place rule on `land2-fix`, which already refuses this reproducer; the bug closes
when batch 2 lands with it as a permanent rejection fixture.

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode
`interp-fails:ref path base 'local' not in env`) requires the program to compile natively and the
interpreter to fail with the recorded message. When the checker refuses it (as `land2-fix` does),
that leg fails until this document, the corpus entry and the leg are updated together.
