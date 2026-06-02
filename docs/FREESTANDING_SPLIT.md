# Freestanding / Hosted Split

Status: reference (Phase 1, item 34)

This document defines the two execution targets for Concrete programs: **freestanding** (no-std, no hosted runtime) and **hosted** (stdlib available, OS-backed entry point). It specifies what each target provides, excludes, and how they relate to the predictable profile.

For profile definitions and gates, see [PROFILES.md](PROFILES.md).
For the execution model and stdlib layers, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For the predictable runtime boundary, see [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md).
For the stdlib module inventory, see [STDLIB.md](STDLIB.md).

---

## Two Targets

### Freestanding

Pure computation. No OS, no libc, no stdlib, no entry point convention. The compiler emits code that depends on nothing beyond the target instruction set.

Freestanding code:

- imports no stdlib modules
- uses no capabilities (`Console`, `File`, `Network`, `Alloc`, `Process`)
- requires no `main` entry point — exports functions by signature
- calls no extern functions
- allocates nothing on the heap
- performs no I/O

This is the target for embedded firmware, kernel modules, WASM libraries, and any context where there is no OS to lean on.

### Hosted

Full Concrete. The stdlib is available, `pub fn main() -> Int` is the entry point, and the compiler links against libc. Capabilities like `Console`, `File`, `Network`, and `Alloc` are available when declared.

Hosted code:

- may import any stdlib module (core, alloc, or hosted layer)
- may declare capabilities in function signatures
- uses `pub fn main() -> Int` as the program entry point
- links against libc (malloc, free, write, read, etc.)
- assumes a POSIX-like OS with process model and file descriptors

This is the default target today. All existing examples compile in hosted mode.

---

## What Each Target Provides and Excludes

| Feature | Freestanding | Hosted |
|---------|:----------:|:-----:|
| Ownership / borrow checking | Yes | Yes |
| Linearity enforcement | Yes | Yes |
| Copy structs and enums | Yes | Yes |
| Fixed-size arrays | Yes | Yes |
| Stack-only data | Yes | Yes |
| Bounded loops | Yes | Yes |
| `match` / `if` / `for` | Yes | Yes |
| `defer` (stack cleanup) | Yes | Yes |
| `trusted fn` | Yes | Yes |
| Stdlib core layer (`math`, `mem`, `slice`) | No | Yes |
| Stdlib alloc layer (`vec`, `string`, `map`) | No | Yes |
| Stdlib hosted layer (`fs`, `net`, `process`) | No | Yes |
| `with(Console)` / `print` / `println` | No | Yes |
| `with(File)` / `with(Network)` | No | Yes |
| `with(Alloc)` / heap allocation | No | Yes |
| `with(Process)` / `abort` | No | Yes |
| `with(Unsafe)` / FFI | No | Yes |
| `pub fn main() -> Int` entry point | No | Yes |
| libc linkage | No | Yes |

---

## Concrete Examples

### `fixed_capacity` — freestanding-ready

The `examples/fixed_capacity/` message validator is already freestanding in its core. The validation functions, ring buffer, and byte readers use no capabilities, no allocation, no FFI, and no stdlib:

```concrete
fn validate_version(v: i32) -> i32 {
    if v == 1 { return 0; }
    return 2;
}

fn ring_contains(rb: RingBuf, val: i32) -> i32 {
    let cap: i32 = 16;
    let scan: i32 = if rb.count < cap { rb.count } else { cap };
    for (let mut i: i32 = 0; i < scan; i = i + 1) {
        let idx: i32 = ((rb.head - rb.count + i) + cap * 2) % cap;
        if rb.data[idx] == val { return 1; }
    }
    return 0;
}
```

Only the test harness (`main`, `report`) uses `with(Console)`. The computation core would compile unchanged in freestanding mode. A freestanding `Concrete.toml`:

```toml
[package]
name = "fixed_capacity"

[profile]
target = "freestanding"

[policy]
predictable = true
```

The compiler would reject `main` and `report` (they use `Console`). The pure validation core compiles as-is.

### `parse_validate` — hosted

The `examples/parse_validate/` error-flow example uses `pub fn main() -> Int` as its entry point. Its validation core is pure, but the program structure assumes a hosted environment — `main` returns an exit code and the compiler generates the libc-linked wrapper.

Its `Concrete.toml` stays as-is (default target is hosted):

```toml
[package]
name = "parse_validate"

[policy]
predictable = true
```

---

## Concrete.toml Configuration

```toml
[profile]
target = "freestanding"   # or "hosted" (default)
```

When `target = "freestanding"`:

1. The compiler rejects any `import std.*` statement
2. The compiler rejects any capability declaration (`with(Console)`, `with(File)`, `with(Network)`, `with(Alloc)`, `with(Process)`, `with(Unsafe)`)
3. The compiler does not generate a `main` wrapper or link libc
4. The compiler does not emit `printf`, `malloc`, `free`, or any libc call
5. The package exports functions by their public signatures — the caller (firmware, kernel, WASM host) is responsible for calling them

When `target = "hosted"` (default):

1. Full stdlib is available
2. `pub fn main() -> Int` is the entry point
3. The compiler generates the libc-linked `main` wrapper
4. All capabilities are available when declared

---

## Relationship to Predictable

Freestanding is a **strict subset** of predictable.

```
┌────────────────────────────────────────────┐
│  Safe (all compiled code)                  │
│                                            │
│  ┌──────────────────────────────────────┐  │
│  │  Predictable                         │  │
│  │  (no recursion, bounded loops,       │  │
│  │   no alloc, no FFI, no blocking)     │  │
│  │                                      │  │
│  │  ┌────────────────────────────────┐  │  │
│  │  │  Freestanding                  │  │  │
│  │  │  (no stdlib, no capabilities,  │  │  │
│  │  │   no entry point, no libc)     │  │  │
│  │  └────────────────────────────────┘  │  │
│  │                                      │  │
│  └──────────────────────────────────────┘  │
│                                            │
└────────────────────────────────────────────┘
```

- All freestanding code passes `--check predictable` (it satisfies all five gates by construction)
- Not all predictable code is freestanding (predictable code may use `with(Console)` for output)
- Freestanding adds one restriction beyond predictable: no capabilities at all, not even `Console`

---

## Use Cases

| Use case | Target | Why |
|----------|--------|-----|
| Embedded firmware | Freestanding | No OS, no libc, no heap |
| Kernel module | Freestanding | Cannot depend on userspace libc |
| WASM library | Freestanding | WASM host provides its own I/O |
| Cryptographic primitive | Freestanding | Pure computation, must be portable |
| Library-only package | Freestanding | No entry point, exports functions only |
| CLI tool | Hosted | Needs `main`, file I/O, console output |
| Network service | Hosted | Needs sockets, process control |
| Test harness | Hosted | Needs `main`, console output for results |

---

## Verification

| Question | Command |
|----------|---------|
| Does this code use any capabilities? | `concrete file.con --report caps` |
| Does this code allocate? | `concrete file.con --report alloc` |
| Does this code pass predictable? | `concrete file.con --check predictable` |
| What host calls does this code make? | `concrete file.con --report effects` |

A freestanding package must show `caps: (pure)` for every exported function and pass all five predictable gates. If any function shows a capability or fails a gate, the freestanding target rejects it.

---

## Implementation Status

The freestanding/hosted split is a **design specification**. The compiler today always operates in hosted mode. The gates and reports needed to enforce freestanding already exist (`--check predictable`, `--report caps`, `--report effects`). What remains:

1. `[profile] target = "freestanding"` in Concrete.toml parsing
2. Compiler enforcement: reject stdlib imports and capabilities when freestanding
3. Linker changes: omit libc linkage and `main` wrapper for freestanding output
4. A freestanding-mode test using `fixed_capacity` core functions
