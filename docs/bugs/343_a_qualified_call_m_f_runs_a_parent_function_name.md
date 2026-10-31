# Bug 343 — A qualified call m::f() runs a parent function named m_f, and an undeclared m_f() reaches the submodule function m.f

**Status:** FIXED 2026-10-31 (contained, branch `wip/fix-343-qualified-call-identity`): a qualified path resolves among qualified paths only, so an undeclared `m_f()` is E0101; a qualified path whose flat spelling also names anything else is refused (E0299) until qualified paths are resolved by identity (R-0492)
**Found:** 2026-10-31, by probes of the qualification rule of SYNTAX_FREEZE_REVIEW §1.1 while absorbing it into SYN-10 (spec audit, wip/spec-audit-3); confirmed by Codex and Grok (both: real bug, one bug with two directions, not covered by bug 151's E0299 containment)
**Severity:** silent wrong code in safe code: a qualified call `m::f()` runs a different function, the parent's own `m_f`, with no diagnostic on either consumer; and a name the program never declares (`m_f`) is accepted and calls `m.f`

<!-- bug-ledger
summary: A qualified call m::f() runs a parent function named m_f, and an undeclared m_f() reaches the submodule function m.f
fixtures: error_343_parent_fn_captures_qualified_call.con error_343_local_captures_qualified_call.con error_343_other_module_fn_captures_qualified_call.con error_343_nested_module_captures_qualified_call.con error_343_nested_parent_fn_captures_qualified_call.con error_343_builtin_captures_qualified_call.con error_343_flat_spelling_reaches_module_fn.con error_343_nested_flat_spelling_reaches_child_fn.con regress_343_qualified_paths.con
class: 1
silent: yes
ruling: Violates: MOD-1@1
-->

## Reproducer

`tests/regressions/bug343_a_qualified_call_m_f_runs_a_parent_function_name/repro.con`: a submodule `m` with `pub fn f() -> u8 { return 42; }`, a top-level `fn m_f() -> u8 { return 7; }`, and `main` returning `m::f()`.

## Observed

On `fb8946c6c` (main `dee500ee` plus documentation-only commits), the reproducer compiles with no diagnostic, the binary exits **7** and `--interp` prints **7**: `m::f()` ran the top-level `m_f`. The other direction, with no top-level `m_f`: `mod m { pub fn f() -> u8 { return 42; } }` and `fn main() -> u8 { return m_f(); }` compiles, exits **42** and `--interp` prints **42**: the undeclared name `m_f` reaches `m.f`. (A two-level path `a::b::f()` is a parse error, E0001 `expected <, got ident(f)`, on both consumers; that is the parser's current one-level qualification, recorded under SYN-10, not this bug.)

## Expected

[MOD-1@1](../spec/modules.md#MOD-1): a name resolves to the declaration it names, and a name that is not declared or imported is refused (E0100, E0101). `m::f()` names `m.f`, so it must call `m.f` (exit 42) or, if OPEN-139 decides that a qualified call needs an import, be refused; it may never run `m_f`. `m_f()` names nothing the program declares or imports, so it is E0101.

## Cause

The parser turns a lowercase qualified call into a call of one flat string: `parsePrimary` in `Concrete/Frontend/Parser.lean` builds `qualName := name ++ "_" ++ nextName` and returns `.call sp qualName ...`. The resolver registers every public submodule function under the same flat string (`siblingQualified` in `Concrete/Resolve/Resolve.lean`, `s.name ++ "_" ++ name`), and Check does the same for submodule signatures (`submoduleNames` in `Concrete/Check/Check.lean`, `subName ++ "_" ++ name`); the interpreter flattens function names the same way (`Concrete/Interp/Interp.lean`). The flat string is both the qualified function's identity and an ordinary identifier, so a user declaration of that spelling captures the qualified call (q3), and the identifier reaches the submodule function unqualified (q4). The E0299 containment of bug 151 compares two flattened submodule definitions only, so a parent declaration on one side, or a single definition, escapes it.

## Structural cause

Class 1 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): declaration identity by spelled name, not a resolved `DefId`. A qualified path is lowered to a spelling instead of being resolved to the declaration it names; bug 151 (two nested modules' functions flattened to one name), bug 186 and bug 239 (`Type_method` spellings) are the nearest instances.

## Structural owner

R-0492 (S5 `DefId`), applied to the shared front end by R-0514: a qualified path resolves to a `DefId` and is never respelled, so no user spelling can capture or reach it.

## Fix (containment)

The parser spells a qualified path `m::f` as `m::f` (`qualifiedPathName`, `Concrete/Frontend/AST.lean`),
a spelling no identifier can have, instead of `m_f`, and Resolve registers every qualified-path entry
under that spelling (the submodule, child and sibling-module keys of `Concrete/Resolve/Resolve.lean`).
So the second direction is repaired: `m_f()` names nothing the program declares and is E0101, and
`fmt` now prints `m::f` back instead of rewriting it to `m_f`.

Check, Elab, the interpreter and the backend still name a submodule function by the flat spelling,
so once Resolve accepts the program, `Module.flattenQualifiedPaths` (`Concrete/Frontend/TypeMap.lean`,
run first in `Pipeline.finishResolve`) respells each resolved path `m_f`. Before that, Resolve refuses
(E0299, `checkQualifiedPath`) every qualified path whose flat spelling would denote anything besides the
path's own definition: a declaration, import or constant in scope spelled `m_f`, a local or parameter
of that spelling, a builtin of that spelling (`string::length` against the builtin `string_length`,
which the interpreter called), or a second definition program-wide whose Core name or
`<module>_<name>` spelling it is (a private `m_f` of another module, or a nested `z.m.f`, whose Core
name is `m_f`). Probes of the last three shapes on the filing's main all ran the wrong function
(exit 7), outside the probes the filing recorded; they are the same defect.

Regression fixtures (`run_tests.sh`): the reproducer and six `error_343_*` captures (parent, local,
another module, a nested module, a nested parent function, a builtin) are E0299; the two directions
of an unqualified flat spelling reaching a function, top level and nested, are E0101; and
`regress_343_qualified_paths.con` (one-level paths to a top-level module and a nested child, a path
inside a nested module, a path as a function value, a parent function whose name merely starts with a
module name) exits 42 natively and under `--interp` (the r343 block checks they agree). The
reproducer left the open-bug leg of `check_example_outputs.sh`.

The refusal is containment, not the semantics MOD-1 gives these programs (`m::f()` should call `m.f`);
the repair is the structural owner's.
