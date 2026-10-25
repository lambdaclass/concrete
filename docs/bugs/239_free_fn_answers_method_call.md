# Bug 239 — a free function named like a method's definition name answers the method call

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, by the Codex review of bug 227's fix (R-0492 slice 6), reproduced on main
`70bcdeb6` and on the train-17 candidate `593dc100`.
**Severity:** silent wrong code. `p.get()` was checked, compiled and interpreted as a call of the
module's free function `P_get`, by value, without the receiver borrow.

## Reproducer

- `tests/programs/error_239_method_call_answered_by_free_fn.con`: `defs` declares `P`, a third
  module `ext` declares `impl P { fn get }`, and `user` imports `P`, defines `fn P_get(p: P) -> i32
  { return 99; }` and returns `p.get()`. Before the fix the binary and `--interp` returned 99. The
  same program without `user`'s free function is refused E0264 ("no method 'get' on type 'P'": the
  impl in `ext` does not reach `user`), and so is this one now.
- `tests/programs/regress_239_free_fn_beside_imported_method.con`: `P`'s method declared by its own
  module, so it reaches `user`; `user` returns `p.get() + P_get(p) - 99`. Before: 99 (both calls ran
  the free function); now 42, natively and under `--interp`.
- `tests/programs/error_239_free_fn_named_like_own_method.con`: `fn P_get` beside `impl P { fn get
  }` in one module. Before: the native build failed on a redefined LLVM symbol and `--interp`
  refused the call; now E0299 at Check (MOD-3).

## Cause

A method's definition name is `Type_method`, and Check's function table (`fnNames`), Elab's
signature table (`fnSigs`) and the shared argument-type peek kept free functions and impl methods
in one list under those spellings, free functions first. A receiver call `x.m()` and a `T::m()`
call looked the method up by `T_m` in that list, so a free function spelled `T_m` answered them.
Elab then emitted a direct call spelled `P_get`, and the renaming that respells a top-level free
function colliding with another module's method (`qualifyCollidingTopLevelFns`, bug 097:
`user_P_get`) renamed every call of that spelling, the method call included. In one module the
two definitions kept one Core name. Bug 082 had already put a module's method definition names in
its callable namespace (an import aliased to `S_value` beside `impl S { fn value }` is E0299), but
the declaration-collision check compared only free functions, externs and constants.

## Fix

- Check and Elab look a method up in a method table alone (`TypeEnv.methodNames` /
  `lookupMethod`; `ElabEnv.methodSigs` / `lookupMethodSig`; `PeekCtx.methodSig`); the method call's
  callable identity comes from the method resolver (`resolveMethodCallee`). A `T::m()` call tries
  the method table first and keeps the free lookup only for a head that has no such method (an
  upper-case module's function). A free-spelled name keeps today's resolution: free functions
  first, then a method by its definition name (`P_get(&p)`, or a method used as a value,
  `regress_096_method_value_same_named`); when it resolves to a method it is in the method
  namespace (`ElabEnv.freeFnNames`, `freeSpelledSpace`).
- A Core reference carries its namespace (`CallSpace`: on `Callee.direct` for a call, on
  `CLocal.space` for a function value), and every Elab renaming renames each namespace by its own
  table: submodule prefixing (`FnRenames`), the parent and sibling
  cross-module renames, type respelling (`CoreRename.methodName`) and the top-level collision
  respelling, which now renames free functions only, so a method call keeps its spelling there.
  `CFnDef.isMethod` marks a method's definition.
- CoreCheck decides a method call's capabilities against the program's method table
  (`CoreCheckEnv.methodCaps`, then the imported requirements), never against a free function or
  extern listed under the same spelling first, and records no FFI trust edge for it (Codex review
  r4: an untrusted extern `P_get` beside `P::get(..)` made the method call require `Unsafe`,
  E0520; `regress_239_method_call_beside_untrusted_extern.con`).
- The reverse direction, a free call answered by a method (Codex review r5): Elab's callee
  resolver met an imported type's method `P_get` before the module's own `extern fn P_get`, so the
  binding was missing from `foreignCalls` and Mono specialized the free call to a generic method of
  that name (42 for `abs(-99)` = 99, natively and `--interp`), and the post-mono capability
  verifier charged a free call bound to an extern for the method's capabilities (E0601). The
  resolver now takes the module's foreign binding before a method; Mono (`MonoState.currentForeign`)
  and the post-mono verifier bind a free call to the function's foreign binding and decide other
  calls against definitions of their namespace (`regress_239_extern_call_beside_generic_method.con`,
  `regress_239_extern_call_beside_capable_method.con`).
- The proof call graph (`ProofCore`, same-module preference by Core name, not by source name) and
  the assumption summary (method calls resolved to methods only) no longer read a method call as a
  call of the module's free function of that spelling (a false "effects unknown" in
  `--report caps`; `regress_239_static_method_beside_free_fn_report.con`, whose `P::get()` was
  E0262 before the fix).
- CoreCheck checks a method call's arguments against the method's signature and records a
  trusted-call edge only for a trusted method; the SSA verifier checks a call against a signature
  of its namespace; the proof call graph and the assumption summary read a free call bound to a
  foreign declaration as that declaration (Codex review r6:
  `regress_239_method_call_beside_extern_of_other_arity.con`, E0262/E0522 before;
  `regress_239_extern_call_beside_method_with_indirect_call.con`, a false "effects unknown" and
  `--interp` running the method before; `regress_239_method_call_beside_trusted_extern.con`, the
  native binary calling `abs` (99) while `--interp` ran the method (42), and false
  calls-ffi/calls-trusted edges, before).
- Codex review r7: a method call spelled by its caller (`P_get` for a nested module's
  `inner_P_get`) is found in CoreCheck's method table through the linker aliases, so it is not
  checked against an extern of the spelling (E0522; `regress_239_nested_method_beside_extern.con`,
  E0262 before the fix); the type-agreement backstop compares a method call with the method
  (`regress_239_respelled_method_beside_extern_types.con`, E0299 before); and the proof-eligibility,
  exclusion and trust-boundary reports read only free calls as extern calls
  (`regress_239_method_call_not_an_extern_call_report.con`: `--report proof` excluded `run` as
  "calls extern: P_get"); Codex review r8: the predictable profile, the effects report and the
  unsafe report do the same (before: "calls extern (P_get)", "ffi: yes", and a method caller
  listed as relying on an extern), and the assumption summary follows a method value to the
  method alone (`regress_239_method_value_no_foreign_assumption.con`). Codex review r9: those
  reports still count FFI reached THROUGH a method call, from the namespace-aware call graph's
  closure (`regress_239_method_call_reaches_ffi_report.con`; the r8 version had lost it), and Lower
  never takes a method call for a compiler intrinsic of its spelling
  (`regress_239_method_spelled_like_intrinsic.con`: a method spelled `read_owned` defined in another
  module was lowered as the intrinsic load, 7 natively for 42 under `--interp`, before the fix).
- Codex review r10: native emission never intercepts a method call as a runtime `Vec` operation
  (`regress_239_method_spelled_like_vec_op.con`: a method spelled `vec_get` failed to build), the
  allocation classification of the reports and the predictable profile reads free calls only
  (`regress_239_method_spelled_like_alloc_op.con`), the proof call graph never takes a method call
  for an intrinsic (`regress_239_alias_spelled_method_reaches_ffi.con`: 7 natively for 42 before,
  and the FFI behind the call was lost), capability explanations resolve a free call bound to an
  extern to that extern and a method call to the method (namespaced keys in `CapLookup`;
  `regress_239_extern_call_beside_capable_method_explained.con`), and the assumption summary does
  not redirect a method edge through a free import alias of its spelling
  (`regress_239_method_edge_beside_free_alias.con`).
- Codex review r11: native emission no longer turns a direct method call into a call through a
  function-typed parameter of its spelling (`regress_239_fn_parameter_spelled_like_method.con`:
  99 natively, 42 under `--interp`, before).
- Codex review r12: CoreCheck decides a free call bound to a foreign declaration (`currentForeign`)
  against that declaration, never a method sharing its spelling
  (`regress_239_imported_extern_alias_beside_capable_method.con`: E0520 before).
- Codex review r13: the proof call graph follows a method call's aliases to methods only
  (`regress_239_method_ffi_beside_free_alias.con`: the FFI behind the call was lost before), and
  cleanup in the allocation reports is read from every call, since a destructor is a method call
  (`regress_239_destructor_call_is_cleanup.con`, a regression in an earlier version of this fix):
  a free call of a freeing intrinsic or a destructor call (`cleanupCallsStmts`), never an ordinary
  method spelled like a freeing intrinsic (`regress_239_method_spelled_like_free_op.con`, Codex
  review r14).
- Codex review r14: the type-agreement backstop compares a method VALUE with the method
  (`regress_239_method_value_type_agreement.con`, E0299 before), and capability explanations
  resolve a method call to the method of its spelling or of an alias target, else a dependency,
  never a free function an alias of the spelling names
  (`regress_239_method_cap_explained_past_free_alias.con`: 99 for 42 natively before the fix, and
  the explanation borrowed the free function's `Console`).
- Codex review r16: a method value never inlines a module constant of its spelling, in Lower or
  the interpreter (`regress_239_method_value_beside_constant.con`: E0602 natively, a refusal under
  `--interp`, before), and a free call is not made a trusted-call edge by a trusted method's Core
  name (`regress_239_extern_call_not_trusted_by_method.con`); a free call's trust is its own
  declaration's, so a trusted extern keeps its trusted-call edge beside a method of its spelling
  (`regress_239_trusted_extern_beside_method_edge.con`, Codex review r17), own or imported
  (`regress_239_imported_trusted_extern_beside_trusted_method.con`, Codex review r18), and never
  by another module's trusted extern of its name (`regress_239_untrusted_extern_beside_trusted_extern.con`).
- Codex review r19: an imported foreign binding's call is not checked against a method of the
  declared name (`regress_239_imported_extern_alias_beside_method_arity.con`), and the assumption
  summary matches a function's foreign spellings by their normalization too, so an extern import
  alias does not also reach a trusted method of the declared name
  (`regress_239_extern_alias_assumption_not_method.con`).
- Codex review r21: an extern call, for the effects, predictable, proof-eligibility, exclusion,
  trust-boundary and unsafe reports, is a free call its function's resolver bound to a foreign
  declaration (`foreignBoundCalls`), not any call spelled like some extern of the program
  (`regress_239_free_call_not_extern_report.con`; pre-existing).
- Codex review r22: the predictable profile cites FFI reached through any call when the
  call-graph closure holds the function, a free call as well as a method call
  (`regress_239_free_call_reaches_ffi_predictable.con`; pre-existing: `main` passed while `run`
  failed).
- `declarationCollisions` counts a module's impl method definition names, so a free function or
  extern spelled like one is E0299 (MOD-3); two methods of one name on one type stay TRAIT-5's.

Regression legs in `run_tests.sh`: the E0264 and E0299 refusals, the positive control, and
`regress_239_parent_method_beside_child_free_fn.con` (a parent naming its own method by its
definition name, as a call and a value, beside a child's free function of that name: the parent's
cross-module rename must leave it alone; a regression Codex found in the first version of this
fix), each `run_ok … 42` with `--interp` = 42.

Merged with R-0492 step 3 (bug 223, main `1b69ed0e`): Check and Elab find a method by its type's
declaration identity (`methodByIdentity`) and otherwise by its spelling in the method table alone.
Step 3 had contained the capture by refusing a method whose Core name a free function takes
(E0299, `error_223_method_core_name_taken.con`); with the method namespace in Core that refusal
is lifted and the fixture is `regress_223_method_core_name_taken.con` (42).

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): free functions and impl
methods shared one spelled namespace in Check, Elab and Core, and a method was found by the string
`Type_method`, so whichever definition of that string a lookup met first answered.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (method tables keyed by the typed `MethodKey`). This fix separates
the method namespace in the tables and in Core's callee; the tables are still keyed by the
`Type_method` spelling until R-0548.
