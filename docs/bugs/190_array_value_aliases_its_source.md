# Bug 190: binding or passing an array by value shared the source's storage

<!-- bug-ledger
summary: an immutable `let`, a `match` binder or a by-value argument of array type aliased the source array in the binary (an array value is its storage's address in Lower), so writing the source changed the copy (silent; `--interp` copied). `snapshotArrayValue` copies at every by-value site; pinned by `regress_190_array_value_copy.con` (`--interp == binary`, 42)
fixtures: regress_190_array_value_copy.con
class: 8 — an array bound by value aliased its source
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `soundness-holes`)
**Found:** 2026-10-23, by the Codex review of the bug 136/137 fix (an evaluation-order temporary
holding an array did not snapshot it), then generalized by probing plain bindings.
**Severity:** silent wrong code in safe code. Arrays are Copy values; the binary made a binding
or a by-value argument an alias of the source array, so a later write to the source changed the
copy. `--interp` copied, so the two consumers disagreed.

Confirmed real by Codex review, 2026-10-25: OWN-D1 / VALUE_MODEL.md require value copying; Lower shared array storage, so mutating the source changed its copy. The old overlapping-argument example is now rejected.

## Observed

```con
fn let_copy() -> Int {
    let mut xs: [Int; 1] = [5];
    let t: [Int; 1] = xs;
    xs[0] = 9;
    return t[0];      // binary 9, --interp 5
}

fn by_value(a: [Int; 1], r: &mut [Int; 1]) -> Int { r[0] = 9; return a[0]; }
fn arg_copy() -> Int {
    let mut xs: [Int; 1] = [5];
    return by_value(xs, &mut xs);   // binary 9, --interp 5
}
```

The same held for a `match` binder arm over an array scrutinee, and therefore for the
evaluation-order temporaries of bugs 136/137 (`E::V { b: xs, a: set(&mut xs[0]) }` stored the
written element). Assignment `t = xs` and struct values were not affected (assignment stores into
the target's own slot; a struct read is a loaded value). Measured on `wip/soundness-holes` before
the fix (server `fede-256gb`).

## Cause

`Lower` represents an array value by the address of its storage: an `arrayLit` lowers to its
alloca, and a variable read yields the variable's slot. An immutable `let` (`setVar name val`), a
binder arm (`setVar binding scrVal`) and a by-value call argument passed that address on, so the
"copy" was the source.

## Structural cause

Class 8 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): storage, places and control flow are
decided ad hoc in `Lower`. Whether a value is a copy or the source's storage is decided by each
binding site, and an array is the one Copy type whose SSA value is an address.

## Structural owner

R-0501 (S10 Place MIR: places and values distinct, a copy explicit). The repair under exit 0b is
one Lower helper, `snapshotArrayValue`, applied at every site that binds or passes an array BY
VALUE.

## Fix

`snapshotArrayValue value val`: when `value` has array type and is not a fresh value (an array
literal or a call result), copy it into its own entry-block alloca and use that. Applied at every
by-value site: an immutable `let` (a mutable one was already copied into its promoted alloca), a
`match` scrutinee bound by an arm, a by-value call argument, a `return` operand and a block's
trailing value. The last two matter because the value is taken before the deferred calls run: a
`defer` writing the array changed the returned value (Codex review, round 2).

## Regression

`tests/programs/regress_190_array_value_copy.con`: `let`, a `match` binder, an evaluation-order
temporary in an enum literal, assignment (a control), and a returned array and a block's array value with a `defer` that writes it; each copies, writes the source, reads the copy; exit 42 natively and under `--interp`
(`run_tests.sh`: `run_ok` and the `--interp == binary` loop). Before the fix the binary exits 1.

Since D2 (loans keyed by root) a by-value argument can no longer be aliased by a `&mut` argument of
the same call (E0293), so that shape left the fixture; `snapshotArrayValue` still copies the
argument.
