# Bug 127 — the binary evaluates struct-literal fields in declaration order, the interpreter in source order

**Status:** FIXED 2026-10-21 (branch `small-fixes-111-127-135`)
**Found:** 2026-10-21, by the semantics probes (finding N1, `evalorder.con`).
**Severity:** silent wrong code. The language ratified source order for struct-literal initializers
on 2026-08-08; the binary runs them in the struct's declaration order, so an accepted program's
effects (and which initializer traps first) differ between the binary and `--interp`.

## Reproducer

`tests/regressions/bug127_struct_literal_field_order/repro.con`:

```con
struct Copy P { a: Int, b: Int }

fn tick(c: &mut Int, v: Int) -> Int {
    *c = *c * 10 + v;
    return v;
}

fn main() -> Int {
    let mut c: Int = 0;
    let p: P = P { b: tick(&mut c, 5), a: tick(&mut c, 6) };
    return c + p.a * 0 + p.b * 0;
}
```

On main (`5b63cd87`): the binary exits **65** (`tick(6)` for `a` ran first), and `--interp` prints
**56** (`tick(5)` for `b` ran first, as written). Expected: 56 from both.

The probe `evalorder.con` measured the other positional constructs in both engines: call arguments,
binary operands and array literals are evaluated left to right; enum-variant literals use
declaration order in both engines (they agree, though the ratified rule names struct literals only);
`arr[i()] = j()` evaluates the right-hand side first, then the index, in both.

## Cause

`Concrete/IR/Lower.lean`, the `.structLit` case of `lowerExpr`: a "Bug #7 fix" reorders the
literal's fields into the struct definition's order (`lookupStructFields`, then `filterMap` over the
definition) and then lowers and stores each initializer in that order, so evaluation follows the
storage order. The 2026-08-08 decision changed `Elab` and removed the reordering in
`CoreCanonicalize` (see [EVIDENCE_PRODUCER_MATRIX](../verification/EVIDENCE_PRODUCER_MATRIX.md));
`Interp.evalFields` folds the source-order list, but `Lower` kept its own reordering.
`check_evidence_build.sh`'s ordering gate runs the probe with `--interp` only, so it passes while the
binary still uses declaration order.

## Fix

`Lower`'s `.structLit` case walks the literal's field list in the order Elab built it, the source
order `Interp.evalFields` also folds, and stores each value at its field's declared offset
(`fieldByteOffset`, looked up by name). Evaluation order and layout are now independent: the
reordering into declaration order (the old "Bug #7 fix", from when offsets were accumulated
positionally) is gone. The reproducer exits 56 natively, as it prints 56 under `--interp`.

Regression legs:

- `tests/programs/regress_127_struct_literal_source_order.con` (`run_tests.sh`, exit 42, plus an
  interpreter-versus-binary leg on it and the reproducer): two-field, rotated three-field, generic
  and mixed-width (`u8`/`Int`) literals, and a functional update with a variable base, each
  checking the order the initializers ran in and every field's value, so storing by position would
  fail too.
- `check_evidence_build.sh`'s struct-literal order gate gains the native leg it lacked: both
  directions (`x` written first, `y` written first) must run in source order in the binary as well
  as in the interpreter.

The open-bug reproducer row (`runs-exit:65`) in `check_example_outputs.sh` is dropped.

Checked alongside, not changed here (filed separately, both pinned as open-bug reproducers):

- **Enum-variant literals** are evaluated in the variant's declaration order in both consumers
  (Elab walks the variant's fields), against the same rule 4: [bug 137](137_enum_literal_field_order.md).
  Reordering them is not a `Lower` change: the interpreter and the proof model bind enum payloads by
  position, so source order needs a name-keyed payload in three consumers.
- **Functional update** `S { f: e, ..base }` evaluates `base` once per omitted field (Elab's
  desugaring to `base.field` per field): [bug 136](136_functional_update_base_reevaluated.md). After
  this fix both consumers agree (explicit initializers in source order, then the base reads); on
  main they also disagreed, since the binary interleaved the base reads with the explicit
  initializers in declaration order.

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): evaluation order
is decided independently by `Elab`, `CoreCanonicalize`, the interpreter and `Lower`; the decision was
applied to three of them, and the gate pinning it checks one consumer. It is also class 8: `Lower`
decides evaluation order and storage layout in one loop, so placing fields at their declared offsets
also reorders their evaluation.

## Structural owner

R-0501 (S10 Place MIR: fixed evaluation order is a property of the lowered CFG, independent of
layout) with R-0496 (S7 Core semantics as the one definition the binary is compared against); the
rule itself is enforced under exit 2j (R-0500, "evaluation order and short-circuiting"), and R-0491
(exit 2c) adds the permanent binary-vs-interpreter leg. As silent wrong code it joins exit 0b under
the defect disposition rule: the repair evaluates initializers in source order into temporaries and
stores them at their declared offsets, and the ordering gate gains a native leg.

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit:65`) requires
the binary to exit with the recorded wrong status 65. When the binary exits 56, that leg fails until
this document, the corpus entry and the leg are updated together.
