# Bug 035: Layout.fieldOffset PANIC — user enum with generic-container struct payload

**Status:** OPEN (loud crash, not silent wrong code)
**Discovered:** 2026-07-16, while probing bug 034 (std.cli work).

## Repro

A USER-module enum whose variant payload is a user struct containing
`Vec<...>` fields, constructed directly in user code:

```text
mod p {
    import std.cli.{CliError, CliResult, CliFlags};
    fn main() with(Alloc) -> Int {
        let r: CliResult = CliResult::Err { error: CliError::UnknownFlag };
        match r { ... }
    }
}
```

(project mode, importing std.cli) panics during lowering:

```text
PANIC at Concrete.Layout.fieldOffset Concrete.Check.Layout:311:4:
Layout.fieldOffset: struct 'Vec' not found
```

The construction site is `CliResult::Err` — the enum's OTHER variant
(`Ok { flags: CliFlags, positionals: Vec<String> }`) forces layout of the
generic `Vec` payload, and the layout ctx at that point resolves the struct
by its UNMANGLED source name (`Vec`), which post-mono is not present.

## Why it does not block std.cli

The stdlib's own consumers never construct `CliResult` in user code — only
`std.cli.parse` builds it (std-internal, where layout resolution works) and
user code only MATCHES on it, which is fine. examples/cli_tool, hexdump,
and base64_cli all pass their gates.

## Direction

`Layout.fieldOffset`'s `none => panic!` arm is reachable whenever a
generic struct's mono-instance name and the source name disagree at a
cross-module construction site. The fix belongs with the mono/layout name
resolution (look up the instantiated name, or thread typeArgs like the
`fieldByteOffset` callers do at the OTHER sites); the panic should also
become a diagnostic. File under the Lower structured-builder cleanup or
pull it forward when a workload constructs such an enum from user code.
