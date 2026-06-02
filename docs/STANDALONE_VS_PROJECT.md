# Standalone File vs Project Mode

Status: stable reference

Concrete has two compilation modes. This document defines when to use each, what each provides, and how to migrate between them.

## Two Modes

| | Standalone file | Project |
|---|---|---|
| Invocation | `concrete myfile.con -o mybin` | `concrete build` in project directory |
| Manifest | None | `Concrete.toml` with `[package]` |
| Source layout | Single `.con` file | `src/main.con` + optional sub-modules |
| Stdlib access | No | Yes (auto-imported) |
| Policy enforcement | No (`predictable`, `deny`, `require-proofs` unavailable) | Yes (via `[policy]` in `Concrete.toml`) |
| Module system | Single-file only | Full (`mod X;` resolves sub-files) |
| Proof workflow | Per-function reports work | Full: registry, bundle, `--check`, `require-proofs` |

## When to Use Each

**Standalone file** — quick experiments, algorithmic helpers, test programs, anything that needs only builtins (integer arithmetic, arrays, control flow, `print_int`, `print_string`, `print_char`, `clock_monotonic_ns`). No setup, no manifest, no directory structure. Compilation is fast and self-contained.

**Project** — anything that uses stdlib types (`String`, `Vec`, `HashMap`, `Bytes`, file I/O, networking, formatting), needs policy enforcement, uses the module system, or will grow beyond a single file.

## Stdlib Access

Projects get stdlib automatically. The compiler resolves `import std.*` when a `Concrete.toml` exists.

Standalone files do not import stdlib. This is by design: it keeps single-file compilation fast, self-contained, and free of implicit dependencies. A standalone file that compiles today will compile the same way tomorrow regardless of stdlib changes.

Builtins available in both modes: `print_int`, `print_string`, `print_char`, `clock_monotonic_ns`, integer arithmetic, arrays, structs, enums, match, `if`/`for`/`while`, `defer`.

### Proposed bridge: `--stdlib` flag

For single files that need stdlib without full project setup:

```
concrete myfile.con --stdlib -o mybin
```

This would resolve `import std.*` for a standalone file without requiring `Concrete.toml`. Not yet implemented.

## Example: Standalone `clamp_value`

A single-file helper that works without stdlib. No manifest, no directory structure.

```
// clamp_value.con
fn clamp(x: i32, lo: i32, hi: i32) -> i32 {
    if x < lo { return lo; }
    if x > hi { return hi; }
    return x;
}

fn main() -> i32 {
    if clamp(5, 0, 10) != 5 { return 1; }
    if clamp(0 - 3, 0, 10) != 0 { return 2; }
    if clamp(15, 0, 10) != 10 { return 3; }
    return 0;
}
```

Build and run:

```
concrete clamp_value.con -o clamp_value
./clamp_value   # exit code 0 = all checks pass
```

No `Concrete.toml`, no `src/` directory, no stdlib dependency. Pure computation using only builtins.

## Example: Project `parse_validate`

A project-form example with policy enforcement and the module system.

Directory layout:

```
examples/parse_validate/
  Concrete.toml
  src/
    main.con
```

`Concrete.toml`:

```toml
[package]
name = "parse_validate"

[policy]
predictable = true
```

The manifest enables:
- **Stdlib access**: `import std.*` resolves (though this example uses only builtins for its pure-computation claim)
- **Policy enforcement**: `predictable = true` makes violations into hard errors (E0610) — recursion, unbounded allocation, FFI, and blocking I/O are rejected at compile time
- **Module system**: `mod X;` can pull in sub-files from `src/`
- **Proof workflow**: `concrete check` reports proof obligations, `require-proofs` can enforce them

Build and run:

```
cd examples/parse_validate
concrete build
./parse_validate   # exit code 0 = all 8 tests pass
```

## Migration: Standalone to Project

When a standalone file outgrows single-file mode (needs stdlib, policy, or modules):

1. Create a directory for the project
2. Move the `.con` file to `src/main.con`
3. Add `Concrete.toml`:
   ```toml
   [package]
   name = "my_tool"
   ```
4. Add `[policy]` if needed:
   ```toml
   [policy]
   predictable = true
   ```
5. Switch from `concrete file.con -o bin` to `concrete build`

Common triggers: needing `String`/`Vec`/`HashMap` or any `std.*` module, wanting `predictable = true` or `require-proofs` enforcement, growing beyond one file with `mod` sub-modules, or using `concrete check` for proof obligations.

## Policy Enforcement

Policy is project-only. The `[policy]` section in `Concrete.toml` controls:

| Key | Effect |
|---|---|
| `predictable = true` | Rejects recursion, unbounded alloc, FFI, blocking I/O (E0610) |
| `deny = [...]` | Rejects specific capabilities or patterns |
| `require-proofs = true` | Fails build if any eligible function lacks a proof |

Standalone files have no manifest and therefore no policy enforcement. If you need compile-time policy guarantees, use project mode.

## Related

- [STDLIB.md](STDLIB.md) — stdlib module inventory and design rules
- [PROFILES.md](PROFILES.md) — profile definitions
- [EXECUTION_MODEL.md](EXECUTION_MODEL.md) — core/alloc/hosted stdlib layers
- [bugs/007_no_print_string_builtin.md](bugs/007_no_print_string_builtin.md) — standalone print gap history (fixed via builtins)
- [bugs/012_no_standalone_timing_path.md](bugs/012_no_standalone_timing_path.md) — standalone timing gap history (fixed via builtin)
