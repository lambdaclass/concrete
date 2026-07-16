# Bug 036: Copy-ness and methods of a cross-module type are import-list-dependent

**Status:** Fixed (2026-07-16)
**Fixed in:** FileSummary.lean `resolveImports` — after named-symbol
resolution, a bounded closure pass pulls in PUBLIC types of the imported
modules that are REACHABLE through what was imported (signature param/return
types, struct field types, enum variant payload types, newtype inners),
with their impls and method sigs. Same-module closure only; a public export
referencing a PRIVATE type keeps today's behavior.
**Regression test:** `tests/programs/import_closure_metadata/` (project
test, exit 0).
**Discovered:** 2026-07-16, switching hexdump to std.cli.

## Repro

```text
import std.cli.{Cli, CliResult};        // CliError, CliFlags NOT imported
...
match cli.parse(1, 1) {
    CliResult::Err { error } => {
        discard(error);                  // E0295: 'CliError' is a non-Copy resource
    },
    CliResult::Ok { flags, .. } => {
        let n: u64 = flags.get_u64(0);   // E0264: no method 'get_u64' on 'CliFlags'
    },
}
```

Adding `CliError, CliFlags` to the import list makes both accepted. The
values arrive via `CliResult`'s payloads regardless of the import list, so
the CHECKER's knowledge of a type (its `Copy` marker, its impl methods)
must travel with the type itself, not with whether the user named it in an
import. Both failures are in the safe direction (rejection), but they are
false positives a user cannot decode — nothing suggests "import the type".

## Note

The consumers' explicit payload-type imports (hexdump/base64_cli/cli_tool)
are now stylistic, not required. The #13b one-source-of-typing-truth work
remains the place where this whole class (metadata keyed by reachability,
not import spelling) gets its systematic treatment.
