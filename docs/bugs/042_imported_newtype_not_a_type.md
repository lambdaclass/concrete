# Bug 042: Imported newtypes/type-aliases silently dropped by Resolve (E0108 at first use)

**Status:** Fixed (2026-07-17)
**Fixed in:** Resolve.lean import validation — the symbol-kind fall-through
(function → extern → struct → enum) now continues to newtypes and public
type aliases, registering the local name in scope AND in `knownTypes`.
**Regression test:** std compiled-coverage gate `numeric` leg
(`import std.numeric.{NonZeroU32}` + try_new both ways) +
`tests/programs/regress_042_import_newtype.con` (project test, exit 0).
**Discovered:** 2026-07-17, std compiled-coverage gate (numeric fixture).

## Repro

```text
import std.numeric.{NonZeroU32};
let z: Option<NonZeroU32> = NonZeroU32::try_new(0);
// error[resolve]: (E0108) unknown type 'NonZeroU32'
```

## Root cause

Resolve's import loop classifies each imported symbol to bring it into
scope; the chain checked functions, extern fns, structs, and enums, then
silently fell through to "contributes nothing" — no scope entry, no
knownTypes entry, and no error either (the name IS public, so the
not-public diagnostic didn't fire). Elab's separate `resolveImports`
already handled newtypes and aliases (bug 036 work), so the pipeline
DISAGREED: Elab would have accepted what Resolve rejected.

Never seen before because every existing importer of std.numeric pulls its
structs/enums (ByteView, ByteCursor, CursorError) — the newtypes
(NonZeroU32/NonZeroU64/Port) had zero cross-module consumers: another
"public surface with no compiled consumer" dark path, exactly the class
the coverage gate exists to burn down.
