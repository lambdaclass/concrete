# Bug 036: Copy-ness and methods of a cross-module type are import-list-dependent

**Status:** OPEN (conservative false rejections, not a soundness hole)
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

## Direction

Resolve/Check should register type metadata (Copy marker, impls) for every
type REACHABLE through imported signatures, not only for types imported by
name. Until then, the workaround is importing payload types explicitly
(hexdump/base64_cli/cli_tool do). Pull the fix when the next workload
trips it — or fold it into the #13b one-source-of-typing-truth work.
