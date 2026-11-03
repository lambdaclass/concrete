# Bug 312 — a destructuring let binds a name with the compiler's __destr_ prefix (SYN-3 checked only on a plain let)

**Status:** OPEN
**Found:** 2026-10-28, by the class-13 spec audit (`wip/class13-audit`, `research/process/class13-audit-2026-10/`): one program per clause of each normative rule, native against `--interp`.
**Severity:** silent: an unsound acceptance. A program SYN-3 forbids compiles, and its binder carries the spelling Verify's post-mono REF-6 check exempts. No wrong result was observed (Elab renames binders apart), so the harm is the hole in REF-6's backstop, not a miscompile today.

<!-- bug-ledger
compiler: legacy
summary: a destructuring let binds a name with the compiler's __destr_ prefix (SYN-3 checked only on a plain let)
coverage: archived reproducer(s) for bug 312; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 13
silent: yes
exit: 0b
area: Check — the fix is in Concrete/Check (Check.lean, CheckError.lean); the fix commit's line weight is mostly run_tests.sh fixtures
ruling: Violates: SYN-3@1
-->

## Reproducer

`tests/regressions/bug312_a_destructuring_let_binds_a_name_with_the_compil/repro.con`: `let S { a: __destr_S } = s;`.

## Observed

On main ca49fada both consumers accept the reproducer and return 42; an enum destructuring `let E::A { v: __destr_v } = e else { .. };` is accepted too, while `let __destr_x: Int = 42;` is refused (E0283).

## Expected

[SYN-3@1](../spec/syntax.md#SYN-3): a `let` does not bind a name that begins with `__destr_`. A destructuring `let` is a `let`, so each of its binders is refused that prefix at check (E0283).

## Cause

`Concrete/Check/Check.lean`, `checkStmt`: only the `.letDecl` case tested the prefix. The `.letStructDestructure` case and the arm that an enum destructuring `let` desugars to (binders marked `letBound`) did not, and `Concrete/Check/Verify.lean`'s post-mono REF-6 check exempts every `let` named `__destr_*`.

## Structural cause

Class 13: a documented rule enforced at one of the forms it covers. The prefix test was written
inline at the plain `let`, and each `let` form adds its binders on its own path, so a new form
(struct destructuring, then the enum destructuring desugared into an arm) skipped it. Same audit:
bugs 310 and 311 and the duplicate-member gap.

## Structural owner

R-0499 (S8: the Core checker as the one judgment, where every binder a `let` form introduces is
one binder node), with R-0492 (one binder identity, so compiler temporaries are not recognized by
spelling and Verify needs no spelling exemption) removing the reason the prefix is reserved at all.
Until then one function, `refuseReservedLetBinder`, is the rule's enforcing declaration and every
`let` form calls it on each binder; check_spec.sh S12
(`wip/spec-code-coverage`) asks for a negative witness per rule code.

## Historical branch containment (unlanded)

The filing branch contained an unlanded repair attempt; it is archived, not part of the frozen compiler.

## Archived fix attempt

`refuseReservedLetBinder` (CheckError.lean, `spec: SYN-3`) holds the prefix test; the plain `let`,
each struct-destructuring binder and each `letBound` arm binder call it. SYN-3 names it in
`Enforced by:` and gains the reject example `destructuring-binder`. Match-arm and parameter
binders are outside SYN-3's text and stay accepted.

Codex confirmation 2026-10-28: CONFIRMED — destructuring binders bypass SYN-3's reserved-prefix check and inherit Verify's blanket `__destr_` exemption, although the probes demonstrate no wrong result (silent; OD: none).

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-312`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug312_a_destructuring_let_binds_a_name_with_the_compil/error_312_enum_destructure_reserved_binder.con`
- `tests/regressions/bug312_a_destructuring_let_binds_a_name_with_the_compil/repro.con`

The original unlanded fix attempt is preserved at `refs/archive/branches/wip/syn3-destructure`.
