# Bug 050: Mono rewrites indirect fn-pointer calls to direct calls when the local's name matches a generic fn

**Status:** Open
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

## Candidate fix

Elab must mark indirect calls distinctly (an `.indirectCall`/callee-is-value
form) so Mono/Lower/EmitSSA never resolve a callee VALUE by global name
lookup. Same durable shape as the intrinsic-identity fix (audit 2/3):
identity by resolution, not by name. Regression: both repros above —
compiled == interp (42), and a project defining `fn f<T:Copy>` builds and
runs std io correctly.
