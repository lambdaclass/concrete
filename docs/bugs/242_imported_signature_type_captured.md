# Bug 242 — a type in an imported signature is captured by the importer's own type of that spelling

<!-- bug-ledger
summary: a type in an imported signature (`fn make() -> P`) is read in the importer's environment by its definition spelling, and the import closure's copy of `P` comes first, so an importer's own `struct P` is unreachable (E0252 on its literal). Owned by R-0492/R-0514 (signature types carried by identity); reproducer pinned as an open-bug reproducer (probes of bug 223's fix, 2026-10-25). Class 1.
coverage: a type in an imported signature is read in the importer's environment by its definition spelling, so the import closure's copy of it hides the importer's own type of that spelling (E0252 on a valid literal); owner R-0492/R-0514 signature types by identity; reproducer tests/regressions/bug242_imported_signature_type_captured/repro.con, found by the probes of bug 223's fix 2026-10-25; pinned as an open-bug reproducer (compile-fails)
class: 1 — a type in an imported signature is a spelling the importer re-reads, so the importer's own type of that spelling is captured
silent: no
-->

**Status:** OPEN
**Found:** 2026-10-25, by the probes of bug 223's fix (R-0492 step 3): an alias of an imported
type beside a local type of the definition's spelling.
**Severity:** loud. A valid program is refused; the probes found no wrong code.

## Reproducer

`tests/regressions/bug242_imported_signature_type_captured/repro.con`: `m.shapes` declares
`pub struct P { pub v: i64 }` and `pub fn make(n: i64) -> P`; `m.user` imports only `make` and
declares its own `struct P { w: i64 }`. `user`'s literal `P { w: 2 }` is refused:
`(E0252) missing field 'v' in struct literal 'P'`. The same with top-level modules. Expected: 42.
Observed on `wip/r0492-steps34` and on main `593dc100` (where an alias `import m.shapes.{P as Q}`
beside the local `P` fails the same way).

## Cause

An imported signature carries its types as the defining module writes them, `.named "P"`. The
import closure (`FileSummary.resolveImports`, bug 036) adds every public type such a signature
names to the importer's imports when no IMPORTED declaration of that spelling is present; it does
not consult the importer's own declarations. Check's and Elab's environments list imports first
(`allStructs := imports.structs ++ summary.structs`), so the imported `P` answers the importer's
spelling `P` everywhere, and the importer's `struct P` is unreachable.

## Structural cause

Class 1 ("declaration identity by spelled name"): a type in a signature is a spelling, interpreted
in whichever environment reads it, so a signature crossing a module boundary changes meaning.

## Structural owner

R-0492 (S5 `DefId`), applied to the front end by R-0514: an imported signature's types carry the
declarations they resolved to in the defining module (`TypeId`), so the importer never re-reads a
spelling. Steps 3 – 4 of [DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md) compare by
identity where both sides are bound but do not change how signatures are carried.
