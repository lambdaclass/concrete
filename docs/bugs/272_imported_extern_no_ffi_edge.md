# Bug 272 — a call of an imported extern has no calls-ffi trust edge

<!-- bug-ledger
summary: CoreCheck recorded the `calls-ffi` trust edge only when the callee's spelling was one of its own module tree's `extern` names, so a call of an extern imported from another module (`import c.{abs}`) had none, plainly, trusted or under an alias: `--report trust-edges` under-reported where a program leaves the language. The edge now comes from the call's binding to a foreign declaration (the function's `foreignCalls`, own or imported), the binding its trust and capabilities were already decided by. Reporting only: no capability, trust or refusal changes. Found by the name-capture work on bugs 238-247; confirmed real by Codex review 2026-10-25. Fixture `regress_272_imported_extern_ffi_edge.con`. Class 1.
fixtures: regress_272_imported_extern_ffi_edge.con
class: 1 — the FFI edge asked whether an extern of the call's spelling is declared in this module tree, not whether the call is bound to a foreign declaration; fixed 2026-10-25: the resolver's binding
silent: yes
-->

**Status:** FIXED (2026-10-25, branch `wip/tooling-batch`)
**Found:** 2026-10-25, noted by the name-capture work on bugs 238-247 (CoreCheck knows only its own
module's externs); confirmed real by Codex review (read-only, origin/main `9c61ff27`): "VERDICT: REAL".
**Severity:** silent (evidence). The program compiles and runs correctly; the trust-edge report,
whose job is to say where a program leaves the language, omitted every call of an imported extern.

## Reproducer

`tests/programs/regress_272_imported_extern_ffi_edge.con`: `mod c` declares `extern fn abs` and a
`trusted extern fn tabs`; `mod u` imports both and calls them, `mod v` imports `abs as cabs`. On
origin/main `9c61ff27`, `--report trust-edges` gave `c own_call calls-ffi abs` and no calls-ffi
edge for `u imported_call`, `u trusted_call` or `v aliased_call` (totals `calls-ffi=1`), while the
binary called the C function from each (42). Expected: a calls-ffi edge for all four
(`calls-ffi=4 calls-trusted=1`).

## Rule

`Concrete/Semantics/TrustEdges.lean`: `callsFFI Y` — "this function calls an `extern`
declaration", with no restriction to the module declaring it. An edge is a record of where rawness
is, never a license; this fix changes the record only.

## Cause

`Check/CoreCheck.lean`: `externHere := envE.externNames.contains fn`, and the edge was
`if space == .free && externHere`. `externNames` is `collectTrustNames m`, the externs of the module
being checked and its submodules. An imported extern is never in it. The same call's trust
(`freeTrusted`) and capabilities were already decided from `foreignDecl?`, the resolver's binding of
the call (Elab fills `foreignCalls` for own and imported externs), so one call was judged foreign
for one fact and not for the other.

## Fix

The edge is `if foreignDecl?.isSome`: a free call bound to a foreign declaration (runtime `_C$rt.`
spellings excluded, as before). `externHere`'s other readers are collision guards for METHOD calls
beside an extern of the same spelling (bugs 238-239) and are unchanged.

Regression legs in `run_tests.sh`: `run_ok` of the fixture (42), its `--interp` (42), and the
trust-edge leg asserting the five edges and the totals.

## Structural cause

Class 1 (declaration identity by spelled name): "is this call foreign?" was answered by "is an
extern of this spelling declared here?", which differ as soon as a spelling denotes an imported
declaration.

## Structural owner

R-0492 / R-0514: every fact about a call is read from its resolved callee identity; CoreCheck's
trust edges now are, and the spelling list remains only as a method-call collision guard.
