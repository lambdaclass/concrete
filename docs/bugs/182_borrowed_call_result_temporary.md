# Bug 182 — a borrowed call result is accepted: a `Heap` temporary is miscompiled and a linear one leaks

<!-- bug-ledger
summary: a borrowed call result was accepted (`&alloc(..)` SIGSEGV, `&int_to_string(..)` leaked). Only a variable or a string literal is a reference's root (E0238 REF-5@1), and loans are keyed by root (E0293 REF-8@1); std migrated at two sites. Pinned by the two reproducers, seven `error_d2_*.con` and `regress_d2_roots_and_projections.con` ((D2, the Austral model))
fixtures: error_d2_borrow_literal.con error_d2_borrow_field_of_temporary.con error_d2_temporary_receiver.con error_d2_mut_borrow_string_literal.con error_d2_call_disjoint_fields.con error_d2_call_field_read.con error_d2_receiver_projection_read.con error_d2_branch_receiver.con error_d2_call_field_read_in_branch.con error_d2_explicit_mut_receiver_read.con regress_d2_roots_and_projections.con
class: 8 — a borrowed temporary has no slot
class: 13 — REF-5: a borrowed call result is accepted
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `soundness-holes`, decision D2)
**Found:** 2026-10-23, by the language-tightening probes (witness programs for
[TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md), the REF-5 / G12 row).
**Severity:** memory-unsafe wrong code in safe code, and a silent leak of a linear value. REF-5@1
([MEMORY_SEMANTICS §15.3](../language/MEMORY_SEMANTICS.md)) refuses a borrow of anything but a
canonical place; gap G12 records that a borrow of a literal, call result or aggregate literal is
accepted. These are the two consequences of that gap for a call result of linear or heap type.

## Reproducer

`tests/regressions/bug182_borrowed_call_result_temporary/repro.con`:

```con
struct Copy B { v: Int }
fn peek(b: &Heap<B>) -> Int { return b.v; }
fn main() with(Alloc) -> Int { return peek(&alloc(B { v: 42 })); }
```

Expected: refused by REF-5 (E0238), or exit 42. Observed on main `185d998c` and `wip/train3`
`949d79e9`: the binary is killed by SIGSEGV (valgrind: invalid read at address `0x2a`, the payload's
value); `--interp` prints 42.

`repro_linear_leak.con` in the same directory: `n(&int_to_string(12345))` with
`fn n(s: &String) -> Int`. Both consumers return 5, and valgrind reports 32 bytes definitely lost:
the borrowed `String` temporary is never destroyed, although the checker requires every linear
value to be consumed and nothing in Concrete is destroyed implicitly.

## Cause

`Lower` passes a borrowed call result as the value it produced: for `&alloc(..)` that is the heap
pointer itself (`call @_C.main.peek(ptr %t4)`, `%t4` the `malloc` result), while `&Heap<B>` is the
address of a slot that holds the heap pointer (the one-hop rule of MEMORY_SEMANTICS §5, bugs 102 and
126), so `peek` loads the payload as a pointer. Check (`checkExpr`, the borrow case) accepts the
operand because it is not a place it refuses (only `&(40 + 2)`, constants and temporary `&mut self`
receivers are refused), and no pass gives the temporary a slot or a destruction point, so a linear
temporary is dropped on the floor. A string literal temporary (`&"text"`, 77 corpus sites) is
harmless only because a literal `String` owns no heap memory (capacity 0).

## Structural cause

Two classes. A documented rule with no enforcing site ([structural class](STRUCTURAL_CLASSES.md)
13): REF-5@1 says a borrow names a canonical place, and the checker refuses only some non-places.
And storage decided ad hoc in `Lower` (class 8): a temporary has no place, so the representation of
a borrow of it depends on the operand's type (a heap pointer, an aggregate's address or a scalar's
spill), the shape of bugs 102 and 126.

## Structural owner

The rule decided on 2026-10-23
([TIGHTENING_DECISIONS D2](../language/TIGHTENING_DECISIONS.md#d2-the-austral-model-borrow-a-variable-then-project-q2-ref-5-g12-revised-2026-10-23)):
the Austral model: only a variable (or a string literal, a static root) is borrowed, a reference
to a part is a projection through a reference, `&s.f` is front-end sugar, and loans are keyed by
root; every other operand is E0238 (REF-5), enforced under exit 2i (R-0500). A temporary is never a
root, so the semantics needs no temporary materialization. Measured migration for the operand rule:
one corpus site (`std/src/map.con`, `m.remove(&2)`); both reproducers here become rejection
fixtures.

## Fix

Decision D2, the Austral model (TIGHTENING_DECISIONS; normative in MEMORY_SEMANTICS §15.3 REF-5 and
REF-8, now version 2): a reference is formed only by borrowing a variable (a string literal counts
as a static, immutable root) and by projecting through a reference. A temporary is never a root, so
`&alloc(..)` and `&int_to_string(..)` are refused (E0238 REF-5@1), and neither the miscompile nor
the leak can be written. The check is one test on the normalized place rule's existing verdict
(`Norm.temp`, `refuseTemporaryBorrow` in `Check.lean`) at every reference-forming site: an explicit
`&e` / `&mut e` (a projection of a temporary, `&mk().f`, included) and a method's implicit `&self`
receiver (`mk().get()`). `&mut "text"` is refused (the static root is immutable). `&s.f` stays legal
as sugar: the normalized place is already "root `s`, projection `f`".

D2's second half lands with it: loans are keyed by the root variable. Within one call, borrows of
one root overlap whatever their fields (`pathsOverlap` compares roots), and a call that makes a new
mutable loan on a root (an owned variable, or a projection such as a `self.offs` receiver) may not
mention that root in another argument (`checkCallRootReads`, E0293 REF-8@1). A plain reborrow of a
`&mut` parameter as a receiver beside a read through it (`self.sift_up(self.len - 1)`) makes no new
loan and stays legal (D1).

Codex review, round 1, four holes closed: an `if` / `match` result as a `&self` receiver is a
temporary too (the receiver's own verdict is used); a read of the borrowed root inside a branch,
guard or arm body of another argument counts (the mention walk enters nested blocks); the D1
exemption is decided from the operand (a plain reborrow of ANY reference binding: a parameter, a
borrow-block binder, an arm binder), not from the root it reaches; and a string literal stays a
static root through a reborrow or projection (`&*(&"ab")`). Fixtures `error_d2_branch_receiver.con`,
`error_d2_call_field_read_in_branch.con`, and two run cases added to `regress_d2_roots_and_projections.con`.

Migration (`rule_impact.sh`, 2198 units against `wip/train3` 1df4cf48): `std/src/map.con`
(`m.remove(&2)` becomes `m.remove(&key2)`), `std/src/cli.con` (`self.offs.push(self.names.len())`
reads `self.names.len()` into a local first), `tests/programs/regress_047_048_hashmap_probe`
(`m.insert(..).is_none()` binds the result first), `regress_2i_ref8_call_disjoint.con` (two fields
of one struct in one call become two calls), and `regress_190_array_value_copy.con` (its by-value
argument aliased by a `&mut` argument is now refused); both reproducers become rejection fixtures.

Regression (`run_tests.sh` and `tests/rules/exit_2i.tsv`): E0238 for both reproducers,
`error_d2_borrow_literal.con`, `error_d2_borrow_field_of_temporary.con`,
`error_d2_temporary_receiver.con`, `error_d2_mut_borrow_string_literal.con`; E0293 for
`error_d2_call_disjoint_fields.con`, `error_d2_call_field_read.con`,
`error_d2_receiver_projection_read.con`; run control `regress_d2_roots_and_projections.con` (a
variable, a string literal, sugar `&s.f`, a projection through a reference parameter, a receiver
reborrow beside a read through it; exit 42, `--interp == binary`).

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`) requires
`repro.con`'s binary to be killed by a signal; when it exits 42 or is refused, that leg fails until
this document, the corpus entry and the leg are updated together. The leak form is not pinned (an
exit status cannot show it); its fixture lands with the rule.
