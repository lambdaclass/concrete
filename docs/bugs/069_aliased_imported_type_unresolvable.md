# Bug 069 — an imported type behind an alias does not resolve as that type

<!-- bug-ledger
summary: an aliased imported struct/enum was registered under its definition-site spelling, so the importer's local alias did not resolve (fixed 2026-08-02; **renumbered from 064**, which a second, unrelated bug already held)
coverage: aliased imported structs/enums are registered under the importer's local spelling; positive alias fixture compiles and the unaliased E0298 privacy control proves resolution was preserved; gated by check_type_identity.sh via tests/programs/bug_064_aliased_imported_type.con (2026-08-02 (renumbered from 064))
class: 1
area: Resolve/FileSummary — resolveImports; the bug was renumbered from 064, so commits naming 064 are another bug's
-->

> **Renumbered 064 -> 069 on 2026-09-20.** A second, unrelated bug was filed under
> 064 and the bug ledger keys by number, so one of the pair was silently discarded:
> the corpus is a bash associative array, where a repeated key overwrites rather than
> collides, and this entry lost. It was absent from the README index as well. Found by
> the status-agreement check added to `scripts/tests/audit_bug_corpus.sh`.

**Status:** FIXED 2026-08-02
**Found:** 2026-08-02, by running the falsifying fixture specified for the R-0004
V2 provenance spike. Code inspection alone had missed it twice.

## Symptom

An imported struct or enum bound to a local ALIAS is not usable as that type: a
field access on it is rejected as though the value were not a struct at all.

```concrete
mod lib { pub struct Copy Point { x: Int, y: Int } }
mod app {
    import lib.{ Point as Coord };
    fn f(c: Coord) -> Int { return c.x; }   // E0254 field access on non-struct type
}
```

## Why this is a real defect and not a privacy rule

The CONTROL is what settles it. The same import WITHOUT the alias produces a
DIFFERENT error:

```concrete
mod app { import lib.{ Point };
          fn f(c: Point) -> Int { return c.x; } }   // E0298 field 'x' is private
```

`E0298` is a sensible, type-aware answer: the type resolved to a struct and the
field was then judged private. `E0254` says no struct was found at all. So the
type resolves correctly when imported plainly and fails to resolve when aliased —
the alias alone is the cause.

## Mechanism

`Resolve` registers the alias under `localName`, but `resolveImports` appends the
original declaration unchanged (`acc.structs ++ [sd]`), so `StructDef.name`
remains `origName`. `lookupStruct alias` therefore finds nothing, and the field
access falls through to the non-struct path.

The same shape applies to `EnumDef` and, by inspection, to variant construction.

## Blast radius

This is an ORDINARY ELABORATION failure, not an evidence-path one — it stops a
valid program from compiling. Distinct from bug 055 (`sibling import alias emits
undefined callee`), which concerns FUNCTION aliases and the linker symbol;
this concerns TYPE aliases and name resolution.

## Why it blocks R-0004

Imported field/variant identity cannot be measured until an aliased imported type
elaborates at all. The V2 provenance work is behind this.

## Fixed

`resolveImports` now registers imported structs and enums under the importer's
local spelling, so `Coord` resolves as the imported `Point`. The regression
keeps the unaliased privacy control: it must still reach E0298, proving the fix
preserves type resolution rather than merely suppressing E0254.

R-0004's follow-on TypeId work keeps that local lookup spelling separate from
the definition-site module and declaration name, so aliasing does not mint a
new semantic type identity.

## Regression witness

`tests/programs/bug_064_aliased_imported_type.con` — must compile once fixed, and
its unaliased control must continue to report the privacy error rather than a
resolution error, so a fix that merely suppresses `E0254` does not pass.
