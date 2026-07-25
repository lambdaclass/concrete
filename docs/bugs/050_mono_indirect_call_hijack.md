# Bug 050: Mono rewrites indirect fn-pointer calls to direct calls when the local's name matches a generic fn

**Status:** FIXED (R-0002, 2026-07-25) — a call's callee is now `Callee.direct` or
`Callee.indirect`, decided in Elab where the scope is known, and no later pass
re-decides it. Gate: `scripts/tests/check_indirect_call_identity.sh` (9 checks).
Fixtures: `tests/programs/regress_050_indirect_call_shadow.con` (42, was 21) and
the project `tests/programs/regress_050_generic_f_std_io/` (builds + exits 0).
Mutation `test_mutation.sh` #21 re-emits the indirect call as direct and is
KILLED.
**Discovered:** 2026-07-18, middle-end audit (two reproduced variants).

## Symptom

A fn-pointer LOCAL whose name collides with any generic fn in the program
has its indirect call silently rewritten to a direct call of the generic:

- Silent wrong code (`tests/programs`-shape repro `.audit_me/tI_shadow.con`):
  `let pick: fn(i64)->i64 = double; pick(21)` next to
  `fn pick<T: Copy>(x: T) -> T` — compiled prints **21** (called the
  identity generic), interp prints **42**.
- Build-breaking: a project defining `fn f<T: Copy>(x: T) -> T` becomes
  unbuildable — std's `io_Writer_write_raw`/`io_Reader_read` call local
  fn-pointers named `f` (std/src/io.con:95,227), Mono rewrites them to
  `@f_for_PtrMut_T_u8_E`, SSAVerify rejects E0711. A one-letter generic
  named `f`/`g` breaks every project using std io.

## Root cause

Elab emits an indirect call through a fn-typed local as `.call varName [] args`
(`Concrete/Elab/Elab.lean:1021-1027`), indistinguishable from a direct call.
Mono's empty-typeArgs branch (`Concrete/IR/Mono.lean:391-431`) resolves the
name against the GLOBAL fnMap + linker-alias pool; a match with any generic
fn specializes it and rewrites the call. Mono has no scope information to
distinguish "local fn-pointer named pick" from "direct call of pick". The
bug-044 alias-orientation fix widened the surface: names not in fnMap now
also resolve via aliases (`Mono.lean:356-368`), and `injectTypeArgsStmts`
has the same hazard inside generic bodies.

## The fix

`CExpr.call`'s callee became a `Callee` sum (`direct name` / `indirect binding`),
set in Elab's fn-typed-local branch — the one place that still knows the callee is
a value in scope. A `Coe String Callee` kept existing construction sites meaning
`direct` (which every one of them is), so the migration's compile errors landed
exactly where code CONSUMES a callee as a name: 23 sites. A new constructor was
rejected for the opposite reason — the form would have fallen silently into 160+
catch-all match arms in Check's linearity walker, Interp, and Lower.

Lower and Interp got SMALLER: both deleted their own scope-probing re-derivation
and read the form instead. Three passes were quietly wrong in the same way and
had not been filed:

- Elab's submodule prefixing renamed any matching callee, so a local fn-pointer
  colliding with a submodule function was rewritten to it.
- Mono's `injectTypeArgsExpr` matched callee names against generic-fn names,
  which would hand a local fn-pointer a generic's type arguments (predicted by
  the root-cause note above).
- CoreCheck picked capability and arity checks by whichever lookup succeeded, so
  an indirect call whose binding matched a global was checked against that
  global's capabilities and arity instead of its own pointer type.

ProofCore treats an indirect callee as having no statically known definition:
extraction returns `none` (blocked), dependency collection contributes no edge,
and the fingerprint uses a `callptr` prefix so a direct call and an indirect call
through a same-named local cannot share a proof subject.

## Remaining gap in the same family

`.ident name (.fn_ ..)` in the same rename pass has the identical hazard — it
cannot tell a global function used as a value from a LOCAL of fn type, so a local
whose name collides with a submodule function is renamed to that function. Unlike
the call case, Core carries no marker for it (`.ident` has only a name and a
type), so closing it needs the same treatment: record it at elaboration, where the
scope is still known. A comment marks the site.

Separately, building this fix's gate surfaced **bug 056**: a fn pointer is
represented in Lower as a register NAME (`@fnref.<fn>`), which is the same
identity-in-a-string anti-pattern one layer down, and it makes reassigning a
fn-pointer across a branch fail SSAVerify.
