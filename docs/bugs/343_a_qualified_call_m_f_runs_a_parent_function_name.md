# Bug 343 — A qualified call m::f() runs a parent function named m_f, and an undeclared m_f() reaches the submodule function m.f

**Status:** OPEN
**Found:** 2026-10-31, by probes of the qualification rule of SYNTAX_FREEZE_REVIEW §1.1 while absorbing it into SYN-10 (spec audit, wip/spec-audit-3); confirmed by Codex and Grok (both: real bug, one bug with two directions, not covered by bug 151's E0299 containment)
**Severity:** silent wrong code in safe code: a qualified call `m::f()` runs a different function, the parent's own `m_f`, with no diagnostic on either consumer; and a name the program never declares (`m_f`) is accepted and calls `m.f`

<!-- bug-ledger
summary: A qualified call m::f() runs a parent function named m_f, and an undeclared m_f() reaches the submodule function m.f
coverage: reproducer tests/regressions/bug343_a_qualified_call_m_f_runs_a_parent_function_name/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg: the binary builds and exits 7, the wrong status)
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

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the wrong exit status 7. This defect is silent, so the disposition rule asks for repair or fail-closed containment at once; neither is in this filing. A candidate containment, for the owner of the repair to choose: extend E0299 to refuse a declaration whose spelling equals any flattened `<module>_<name>` of a function the program can reach, and refuse an unqualified call that resolves only through a flattened name. Meanwhile a user avoids top-level names of the form `<submodule>_<name>`, and calls submodule functions only through `m::f`.
