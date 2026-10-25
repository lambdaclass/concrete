# Bug 229 — Resolve's module scope refuses names Check admits

<!-- bug-ledger
summary: Resolve's module scope refused programs Check and Elab accept: a function registered before a same-named enum made `E::A` "not an enum" (E0104), a user `enum Option` answered `Option<i32>` (E0113) though the builtin is never withheld, and a child module's newtype was an unknown type (E0108). Type positions now search the type namespace only, an enum named `Option` is always the builtin (as in Check's enum table), and children's newtypes are known types. Found by the Codex reviews of bug 217's fix, 2026-10-24. Fixtures `regress_229_*`. Class 12.
fixtures: regress_229_value_does_not_hide_type.con regress_229_user_option_does_not_hide_builtin.con regress_229_child_newtype_known.con regress_229_option_named_alias_or_struct.con regress_229_alias_and_enum_literal_precedence.con
class: 12 — Resolve's module scope: one first-wins map for types and values, its own builtin-`Option` rule, child newtypes missing, against Check's environment
silent: no
-->

**Status:** FIXED (branch `wip/bughunt-216-217`)
**Found:** 2026-10-24, by the Codex review (round 3) of bug 217's fix: the new walk over nested
submodules carried three Resolve scope rules into them, and each also refuses top-level programs
on main.
**Severity:** valid programs refused at Resolve; no wrong code.

Confirmed real by Codex review, 2026-10-25: docs/spec/scope.md SCOPE-2 distinguishes type and value namespaces; Resolve let a function hide an enum. The bundled child-newtype visibility change remains contingent on OD21.

## Reproducer

Each fixture holds a top-level and a nested shape; on main `408880cb` each is refused at Resolve,
and Check, Elab and the interpreter accept the nested shape (main does not resolve it):

- `tests/programs/regress_229_value_does_not_hide_type.con`: `enum Copy E { A }` and `fn E()` in one
  module; `E::A` is `(E0104) 'E' is not an enum`.
- `tests/programs/regress_229_user_option_does_not_hide_builtin.con`: a module declaring
  `enum Copy Option { A }` and using the builtin `Option<i32>`; `(E0113) 'Option' takes 0 type
  arguments, but 1 was given`.
- `tests/programs/regress_229_child_newtype_known.con`: `mod leaf { pub newtype N = i32; }` and a
  parent function `fn top(x: N) -> N`; `(E0108) unknown type 'N'`.

Expected and now observed: each exits 42 natively and under `--interp`.

## Cause

- `Resolve.lookupSymKind`, used for type positions (`declaredTypeArity`, `declaredCapArity`, an enum
  literal's enum), read `Scope.symbolMap`, one first-wins map over values and types, so a function
  registered before a same-named enum answered. MOD-3 keeps types and values in separate namespaces
  (bug 218's `regress_218_type_and_value_namespaces.con`), and Check and Elab look types up apart.
- Resolve let a user `enum Option` answer `Option` (the declaration won over the builtin table),
  while `BuiltinEnums.builtinEnums`, the rule Check and Elab share (bug 065), puts the builtin
  `Option` first and never withholds it.
- `buildGlobalScopeFromSummary` registered a child module's structs, enums, aliases and traits but
  not its newtypes, which Check's environment includes (`allNewtypes`).

## Fix

`Scope` carries `typeKinds` (every type declaration of a name, in scope order), and each type
position consults it the way Check and Elab consult their tables. A written type
(`lookupWrittenType`: annotations, signatures, type arguments, arity checks) takes an alias first,
since Check expands aliases before any struct or enum lookup, then the first struct, enum or
newtype, an enum named `Option` being the builtin. An enum literal or pattern head
(`lookupEnumForLiteral`) reads the enum table: the builtin `Option`, the builtin `Result` unless a
user enum `Result` is in scope (`builtinEnums`), else the scope's enum. `lookupSymKind` is gone.
The reviews narrowed the first versions: answering every `Option` with the builtin refused
`type Option = i32` and a `struct Option<A, B>` (r4); one first-wins type map let an imported
generic struct `N<T>` beat a local `type N = i32` and a `type Option` hide `Option::Some` (r5).
`buildGlobalScopeFromSummary` registers children's newtypes (symbols and known types), and bug 217's
submodule scope includes them. Regression legs in `run_tests.sh`: the five `regress_229_*.con`
fixtures (`regress_229_option_named_alias_or_struct.con` and
`regress_229_alias_and_enum_literal_precedence.con` the r4 and r5 witnesses), `run_ok ... 42` and
`--interp` = 42.

## Structural cause

Class 12 ("one fact kept in two places"): the environment a module's names resolve in is assembled
separately by Resolve and by Check/Elab, and Resolve's copy had a single namespace, its own builtin
rule and a shorter list of child declarations.

## Structural owner

R-0492/R-0514 (one resolver: one scope per module, consumed by Check and Elab).
