# Project Bootstrap UX

Status: design document (Phase 1, item 36)

Defines `concrete new`, standard project layout, starter templates, and the first outsider workflow.

## Standard layout

```
my_project/
  Concrete.toml        # project manifest (required)
  src/
    main.con           # entry point for executables
    lib.con            # entry point for libraries (future)
  tests/               # test files (future)
```

- `Concrete.toml` at the root marks a Concrete project.
- `src/main.con` must define `pub fn main()` for executables.
- `src/lib.con` (future) exports public functions, no `main`.
- Standalone `.con` files work without a project — `concrete new` is for multi-file projects and policy enforcement.

## Concrete.toml fields

```toml
[package]
name = "my_project"           # required — package name, used in module paths
version = "0.1.0"             # optional — semver, informational today

[policy]
predictable = true             # enforce predictable subset (no recursion,
                               #   bounded loops, no alloc, no FFI)
deny = ["Unsafe"]              # deny specific capabilities:
                               #   Unsafe, Alloc, File, Network, Process
require-proofs = true          # require Lean proofs for all eligible functions;
                               #   stale/missing/blocked = compile error

[profile.release]              # optional build profiles
release = true
opt_level = 3
debug_info = false
```

Only `[package] name` is required. Everything else is optional.

## `concrete new` command

```
concrete new <name> [--template <template>]
```

| Argument | Default | Description |
|----------|---------|-------------|
| `<name>` | (required) | Project directory and package name |
| `--template` | `predictable` | One of: `predictable`, `library`, `ffi` |

1. Creates `<name>/` directory (error if it exists).
2. Writes `Concrete.toml` and `src/main.con` (or `src/lib.con` for library).
3. Prints next-steps message.

## Templates

### `predictable` (default)

Pure functions, error enum, bounded loop, `predictable = true`. Based on `parse_validate`.

**`Concrete.toml`:** `[package] name = "$NAME"` + `[policy] predictable = true`

**`src/main.con`:**
```
mod $NAME {
    enum Copy ParseError { TooShort, OutOfRange }
    enum Copy Result { Ok { value: i32 }, Err { error: ParseError } }

    fn validate(input: [i32; 4], len: i32) -> Result {
        if len < 1 { return Result::Err { error: ParseError::TooShort }; }
        let mut sum: i32 = 0;
        for (let mut i: i32 = 0; i < len; i = i + 1) {
            sum = sum + input[i];
        }
        if sum < 0 { return Result::Err { error: ParseError::OutOfRange }; }
        return Result::Ok { value: sum };
    }

    pub fn main() -> Int {
        let data: [i32; 4] = [10, 20, 30, 40];
        match validate(data, 4) {
            Result::Ok { value } => { if value != 100 { return 1; } },
            Result::Err { error } => { return 2; },
        }
        return 0;
    }
}
```

### `library`

Exports public functions, no entry point, `predictable = true`.

**`Concrete.toml`:** `[package] name = "$NAME"` + `[policy] predictable = true`

**`src/lib.con`:**
```
mod $NAME {
    pub fn clamp(value: i32, lo: i32, hi: i32) -> i32 {
        if value < lo { return lo; }
        if value > hi { return hi; }
        return value;
    }

    pub fn abs_val(x: i32) -> i32 {
        if x < 0 { return 0 - x; }
        return x;
    }
}
```

### `ffi`

Trusted-boundary starter: one `trusted extern fn`, one trusted wrapper, one pure validator. Based on `packet`.

**`Concrete.toml`:** `[package] name = "$NAME"` (no predictable — FFI is incompatible)

**`src/main.con`:**
```
mod $NAME {
    // Pure validator — no capabilities, proof-eligible
    fn validate_range(value: i32, lo: i32, hi: i32) -> i32 {
        if value < lo { return 1; }
        if value > hi { return 1; }
        return 0;
    }

    // Raw FFI: opaque foreign call. Auditor reviews this.
    trusted extern fn get_raw_value() -> i32;

    // Trusted wrapper: calls FFI, validates, returns safe default on failure.
    trusted fn safe_read(lo: i32, hi: i32) -> i32 {
        let raw: i32 = get_raw_value();
        if validate_range(raw, lo, hi) != 0 { return lo; }
        return raw;
    }

    pub fn main() -> Int {
        // Test the pure validator directly (no FFI provider needed)
        if validate_range(50, 0, 100) != 0 { return 1; }
        if validate_range(-1, 0, 100) != 1 { return 2; }
        if validate_range(101, 0, 100) != 1 { return 3; }
        return 0;
    }
}
```

## First outsider workflow

Step-by-step for someone trying Concrete for the first time:

```
concrete new my_tool          # creates my_tool/ with predictable template
cd my_tool
concrete build                # compiles src/main.con, shows proof summary
concrete run                  # runs the binary, exit code 0 = success
concrete check                # shows proof status and next steps
```

Expected `concrete build` output:
```
Building my_tool...
proof: 1 eligible, 0 proved, 0 stale
Build succeeded.
```

Expected `concrete check` output:
```
my_tool proof status:
  my_tool.validate  eligible  (not yet proved)
Next steps:
  1. concrete build --report lean-stubs   (generate theorem stubs)
  2. Write Lean proofs for eligible functions
  3. concrete build --report check-proofs (verify proofs)
```

## Standalone mode vs projects

| | Standalone file | Project |
|---|---|---|
| Manifest | none | `Concrete.toml` |
| Command | `concrete build file.con` | `concrete build` (from project dir) |
| Policy | compiler flags only | `[policy]` in manifest |
| Multi-file | no | `mod other;` imports |
| When to use | quick scripts, examples | real tools, libraries, policy enforcement |

`concrete new` creates projects. Standalone files do not need it. Both use the same compiler passes.
