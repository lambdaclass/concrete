# Bug 144 — heap-shell interiors were not linearity-tracked: a field read duplicated a value, and `free` dropped one

<!-- bug-ledger
summary: heap-shell interiors were not linearity-tracked (KNOWN_HOLES H11's disclosed exclusion), so `let a = h.s; let b = h.s;` double-freed a String and `free(h)` dropped a live linear interior (EVALUATION2 D2). By-value non-Copy heap-field reads are E0290 and `free(h)` of a non-Copy pointee is E0287; the H11 exclusion is withdrawn. `error_144_*.con`, `regress_144_heap_node_destructure.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_144_heap_field_read_twice.con error_144_free_drops_linear.con error_144_defer_free_drops_linear.con error_144_heap_newtype_unwrap.con regress_144_heap_node_destructure.con
class: 13 — heap interiors exempted from use-once by a disclosed exclusion
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED 2026-10-21 (branch `safety-eval2`)
**Found:** 2026-10-21, by the second semantics evaluation (EVALUATION2 D2; probes `h01`, `h02`,
`h04`, `h05`), not filed then. It had been disclosed as a design exclusion (KNOWN_HOLES H11:
"`Heap<T>` interiors are not linearity-tracked", for the `h.next` + `free(h)` destructure idiom).
**Severity:** double free in safe code (`h05`), and silent skipped destruction of a linear value
(`h01`, `h02`).

## Reproducers

`tests/regressions/bug144_heap_interior_untracked/repro.con`:

```con
struct S { s: String }

fn main() with(Alloc) -> Int {
    let mut t: String = "a string long enough to live on the heap for sure";
    string_append_int(&mut t, 1234567);
    let h: Heap<S> = alloc(S { s: t });
    let a: String = h.s;
    let b: String = h.s;
    free(h);
    drop_string(a);
    drop_string(b);
    return 0;
}
```

On main (`5a064e77`): accepted; the binary double-frees the string (SIGTRAP, exit 133 on arm64
macOS). `repro_free.con` (`let h: Heap<R> = alloc(mk(1, p)); free(h);` with `R` logging its
destruction): accepted; exit 0, the destructor never ran.

Fixed: refused at the checker, natively and under `--interp`:

```
error[check]: (E0290) cannot move non-Copy value of type 'String' out of field 's' of 'S' by value — the owner would still hold the same value (duplication)
error[check]: (E0287) free() would drop the live non-Copy value of type 'R' inside the heap cell
```

## Cause

`Concrete/Check/Check.lean` `.fieldAccess` skipped the H11 projection rule (E0290) for an owned
heap shell (`isHeapShell`), and the statement-discard check exempted `free(...)` whatever its
pointee: `free(h)` hands back the pointee, and dropping that value was "the established
free-and-drop idiom". Together they let a heap cell's non-Copy field be read out any number of
times and the cell then be freed with its remaining contents unaccounted for.

## Fix

Heap interiors are linearity-tracked (user decision (c), 2026-10-21: no safe store into, or drop
of, an initialized non-Copy place without consuming it):

- A by-value non-Copy field read of a heap shell is E0290, as for any other owner. Copy fields,
  borrows (`&h.f`) and further projection stay legal.
- `h.0` on a `Heap<W>` with `W` a newtype over a non-Copy type is E0290 too (Codex review, round 4:
  the newtype unwrap treated the shell as the newtype's owner, consumed it and never freed the
  cell); `error_144_heap_newtype_unwrap.con`.
- `free(h)` as a statement, or deferred, is E0287 (`freeDropsLinearInterior`) when the pointee is
  not Copy. `free(h)` of a Copy pointee is unchanged.
- The idiom becomes: move the node out of its cell (`let n = *h;`, which frees the cell, or
  `let n = free(h);`) and destructure it (`let Node { value, next } = n;`).

Fixtures (`run_tests.sh`): `error_144_heap_field_read_twice.con` (E0290),
`error_144_free_drops_linear.con` and `error_144_defer_free_drops_linear.con` (E0287), and the
positive control `regress_144_heap_node_destructure.con` (`*h` and bound `free(h)` destructures,
both destructors run, `free` of a Copy pointee; prints `12 7`). An interpreter leg asserts both
reproducers are refused by both engines.

Migration: `std/` 0, `examples/` 0, `check_linear_discard.sh` 3 cases (its `free(box)` and value-pattern legs now consume the pointee, `let Token { x } = free(b);`, and a new leg pins `free(b);` of a linear pointee as E0287), `tests/programs/` 23 files (`regress_126_heap_ref_one_hop.con`,
which landed with bug 126 after this branch began, takes its `Cell<Int>` back from `free` and
destructures it): 19 heap tests whose pointee was a
plain-data struct (only scalar fields, no `Destroy`) now declare it `struct Copy` (their subject is
heap ownership, not the struct's linearity), and three heap-node tests
(`complex_recursive_list`, `complex_recursive_tree`, `complex_recursive_mutual`) replaced
`h.next` + `free(h)` with `let Node { .. } = *h;`. (Until [bug 146](146_heap_deref_does_not_free.md), found reviewing this migration, `*h` did not actually free the cell.)

## Structural cause

A documented exclusion that contradicted the linear model ([structural class](STRUCTURAL_CLASSES.md)
13: the rule "every non-Copy value is consumed exactly once" had no enforcing site for heap
interiors), and ownership state not modeled for the place a heap shell denotes (class 15).

## Structural owner

Containment: ROADMAP exit 0b. The decision that heap interiors are tracked: R-0500 (exit 2i, an
explicit addition to its acceptance), normative text in OWNERSHIP_MODEL and MEMORY_SEMANTICS §8;
KNOWN_HOLES H11's exclusion is withdrawn. The one Core checker (R-0499/R-0514) and Place MIR's
per-place initialization state (R-0501) consume it.
