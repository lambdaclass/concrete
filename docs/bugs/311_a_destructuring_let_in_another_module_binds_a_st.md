# Bug 311 — a destructuring let in another module binds a struct's private fields (TYPE-11 enforced only for literals, reads and writes)

**Status:** FIXED (branch `wip/bug-311`)
**Found:** 2026-10-28, by the class-13 spec audit (`wip/class13-audit`, `research/process/class13-audit-2026-10/`): one program per clause of each normative rule, native against `--interp`.
**Severity:** silent: an unsound acceptance. Another module reads a private field, so a module cannot rely on privacy to keep an invariant (a `NonZero` built only by its smart constructor is read and, for a Copy struct, its fields copied out freely).

<!-- bug-ledger
summary: a destructuring let in another module binds a struct's private fields (TYPE-11 enforced only for literals, reads and writes)
coverage: reproducer tests/regressions/bug311_a_destructuring_let_in_another_module_binds_a_st/repro.con, pinned by run_tests.sh (`run_err` legs for the reproducer, `error_311_destructure_private_linear.con` and `error_311_destructure_private_copy_partial.con`; `error_311_parent_constructs_child_newtype.con`, `error_311_parent_reads_child_private_field.con` and `error_311_parent_destructures_child_private_field.con`; `run_ok` for `regress_311_destructure_pub_fields.con`) and check_spec.sh S4 (`TYPE-11/destructure-private-field`, `TYPE-11/parent-constructs-child-newtype`, `TYPE-11/parent-reads-child-private-field`)
class: 13
silent: yes
ruling: Violates: TYPE-11@1 — a destructuring let outside the declaring module binds a private field; a parent module has no representation rights over its child (OD21)
exit: 0b
-->

## Reproducer

`tests/regressions/bug311_a_destructuring_let_in_another_module_binds_a_st/repro.con`: module `nz` declares `pub struct Copy NonZero { v: Int }` with a private field and a smart constructor; module `m` writes `let NonZero { v } = value;`.

## Observed

On main ca49fada both consumers accept it and print or exit 42, while `value.v` in the same position is refused (E0298). A non-Copy struct with a private field is destructured the same way (`error_311_destructure_private_linear.con`), and so is a partial pattern over a Copy struct.

## Expected

[TYPE-11@1](../spec/types.md#TYPE-11): outside the module that declares it, a struct field that is not marked `pub` is not named in a struct literal, read or written. A destructuring `let` names and reads each field it binds, so it is refused at check with E0298, as TYPE-11's visibility table records ("only if every bound field is `pub`").

## Cause

`Concrete/Check/Check.lean`, `checkStmt`'s `.letStructDestructure` case: it looked each binder's field up by name (`lookupStructField`) and never consulted the field's visibility, which only the field-access (`.fieldAccess`), field-assignment and struct-literal sites check.

## Structural cause

Class 13: a documented rule with no enforcing site for one of its forms. TYPE-11 is enforced at
each syntactic form that names a field, one site per form, and the destructuring form (added by
pattern ergonomics) never got one; TYPE-11's status `partial R-0500` and the construction-rights
plan's "record update/destructure ... active roadmap work" hid the gap from every gate, since no
pending example recorded it. Same audit: bugs 310 (STMT-7), and the SYN-3 and duplicate-member gaps.

## Structural owner

R-0499 (the Core checker as the one judgment): privacy belongs to field projection in Core, which
every surface form that names a field elaborates to. Until then, check_spec.sh S12
(`wip/spec-code-coverage`) asks for a negative witness per rule code, and this bug adds the
destructuring witness for E0298.

## Containment while open

Fixed with the filing. A parent module's rights over its child were OD21's question, which the
owner decided on 2026-10-29: only the declaring module constructs a newtype or names a private
field, and a parent gets no rights over its child. The fix applies that answer to every form at
once, since they share one test: a parent that imports its child's newtype or struct may no longer
construct the newtype (E0296), read or write a private field (E0298) or destructure one (E0298).
`tests/programs/module_qualified_impl`, which bug 262's record named as the program the strict
rule would refuse, makes its child struct's fields `pub`.

## Fix

The `.letStructDestructure` case refuses, outside the declaring module, a binder whose field is not
`pub` (E0298, `rule: TYPE-11@1`), before the binders are added, with the field-read site's test
(`structIsLocal`). Under OD21, `checkModule`'s local newtype and struct sets (and their identities)
hold this module's own declarations only, no longer its submodules', so the same test refuses a
parent. TYPE-11 gains the reject example `destructure-private-field`, and its `parent-*` examples,
pending until now, are enforced.

Codex confirmation 2026-10-28: CONFIRMED — struct destructuring reads private fields across sibling modules without checking TYPE-11; OD21's parent-child question does not apply (silent; OD: none).
