# Bug 336 — a destructuring let without mut binds assignable names

**Status:** OPEN
**Found:** 2026-10-30, by absorbing PATTERN_DESTRUCTURING into the specification (wip/spec-absorb-2): a probe of `let mut` over a struct pattern showed that `mut` changes nothing, native against `--interp`, on main cc876322.
**Severity:** silent: a program OWN-10 refuses is accepted, and both consumers agree on its result.

<!-- bug-ledger
compiler: legacy
summary: a destructuring let without mut binds assignable names
coverage: archived reproducer(s) for bug 336; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 12
silent: yes
ruling: Violates: OWN-10@2
-->

## Reproducer

`tests/regressions/bug336_a_destructuring_let_without_mut_binds_assignable/repro.con`: a destructuring `let` without `mut`, then an assignment to one of its binders.

Codex confirmation 2026-10-30: CONFIRMED ("Destructuring `let` without `mut` incorrectly creates mutable checker
bindings, allowing the assignment that OWN-10 requires rejecting with E0217"), by source inspection of the sites
below.

## Observed

On main cc876322 (built on OVH): `let P { a, b } = p; a = a + 1;` is accepted; the binary exits 42 and `--interp`
prints 42. The control `let a: i64 = 40; a = a + 1;` is refused by both: `error[check]: (E0217) cannot assign to
immutable variable 'a'`. `let mut P { a, b } = p` behaves the same: the `mut` is read and dropped.

## Expected

[OWN-10](../spec/ownership.md#OWN-10): only a `mut` binding is assigned or mutably borrowed, so `a = a + 1` is
refused with E0217 at check. [PAT-5](../spec/patterns.md#PAT-5) makes no mutability exception for a destructuring
`let`.

## Cause

`Concrete/Frontend/Parser.lean`, `parseLet`: `mut` is read, but the two destructuring forms return without it
(only an ordinary `letDecl` keeps it), and neither destructuring AST constructor nor `PatBind` stores mutability.
`Concrete/Check/Check.lean` registers every struct-pattern binder with `addVar ... true` (mutable), so the
assignment check (E0217 when `!info.mutable`) never fires. Elab later expands the pattern into `letDecl ... false`,
too late to refuse anything.

## Structural cause

Class 12, one fact kept in two places: a binder's mutability is lost in the parser and the AST, then invented
again by each later pass, `true` in Check and `false` in Elab, so the two disagree and the checker's guess is the
permissive one (also class 11: an absent flag read as "mutable").

## Structural owner

R-0499 (one checker on Core, by stable rule ID): the binder's mutability is carried in the AST pattern from the
parser on and enforced once, so no pass invents it.

## Historical branch containment (unlanded)

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg (runs-exit 42). Meanwhile a user
declares a destructured binder they mean to change with a plain `let mut` copy of it (`let mut a2: i64 = a;`); a
program that assigns a binder of a non-`mut` destructuring `let` is accepted today and refused once this is fixed.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-destructure-mut`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug336_a_destructuring_let_without_mut_binds_assignable/repro.con`
