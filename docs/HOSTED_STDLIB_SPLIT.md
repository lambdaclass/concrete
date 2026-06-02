# Hosted Stdlib Split

Status: design reference (Phase 3, item 56)

This document defines the boundary between the bounded/provable-friendly core stdlib and the OS/runtime-dependent hosted stdlib. It specifies what belongs in each layer, why, and how the capability/trust model makes the boundary auditable.

For the existing stdlib direction, see [STDLIB.md](STDLIB.md).
For the three-layer classification, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For the freestanding/hosted execution split, see [FREESTANDING_SPLIT.md](FREESTANDING_SPLIT.md).
For the trust model, see [SAFETY.md](SAFETY.md) and [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md).

---

## Principle

The stdlib has three layers because code has three levels of host dependency. The boundary is not arbitrary — it follows directly from what the code needs to run:

1. **Core** needs nothing. Pure computation. No OS, no allocator, no libc.
2. **Alloc** needs a heap allocator (malloc/realloc/free) and abort-on-OOM.
3. **Hosted** needs a POSIX-like OS with file descriptors, sockets, processes, and a clock.

A function that calls `malloc` cannot run on bare metal without an allocator. A function that calls `socket()` cannot run without an OS. The layers make this dependency visible at the module level, not just the function level.

---

## Layer 1: Core Stdlib

### What belongs here

| Module | Purpose | Why core |
|--------|---------|----------|
| `option` | `Option<T>` with `Some`/`None` | Pure enum, no allocation |
| `result` | `Result<T, E>` with `Ok`/`Err`, `?` support | Pure enum, no allocation |
| `math` | `abs`, `min`, `max`, `clamp`, numeric helpers | Pure arithmetic |
| `mem` | `sizeof`, `alignof` | Compile-time queries, no runtime dependency |
| `slice` | Borrowed contiguous views, checked indexing | Pointer + length, no allocation |
| `fmt` | `format_int`, `format_hex`, `format_bin`, `format_bool`, padding | Pure string construction (requires alloc for owned output, but formatting logic is pure) |
| `hash` | FNV-1a, multiplicative hash helpers | Pure computation |
| `parse` | `parse_int`, `parse_hex`, `parse_bool`, `Cursor` | Pure string-to-value conversion |
| `ascii` | `is_digit`, `is_alpha`, `is_whitespace`, char classification | Pure byte classification |
| `test` | `assert_eq`, `assert_true`, comparison assertions | Pure logic (output uses compiler-inserted printf) |

### Properties of core modules

- **No capabilities required.** Every function has `caps: (pure)` or at most borrows from caller context.
- **No libc dependency.** No `malloc`, `free`, `printf`, `strlen`, or any extern call.
- **Freestanding-compatible.** Core modules compile and run without an OS, without libc, without a heap.
- **Proof-friendly.** Functions in core modules are candidates for `--check predictable` and many are proof-eligible (no loops, no mutation, no capabilities).
- **Deterministic.** No I/O, no randomness, no clock, no environment reads. Same inputs always produce same outputs.

### What core is for

- Embedded firmware that cannot call libc
- Kernel modules that run before the allocator is initialized
- WASM libraries where the host provides its own I/O
- Cryptographic primitives that must be portable and auditable
- The "predictable inner core" pattern: pure validation logic wrapped by effectful I/O at the edges

### Boundary note: `fmt` and `parse`

`fmt` and `parse` are logically pure — their algorithms are pure computation over bytes and integers. However, `fmt` functions that produce owned `String` output currently require allocation. The design intent is:

- Formatting into a caller-provided buffer (slice-based) belongs in core
- Formatting that allocates a new `String` belongs at the alloc boundary
- Parsing from borrowed input (`&String`, `Cursor`) is fully core

This split may require `fmt` to expose both a core buffer-writing API and an alloc-layer convenience API.

---

## Layer 2: Alloc Stdlib

### What belongs here

| Module | Purpose | Why alloc |
|--------|---------|-----------|
| `alloc` | `heap_new<T>`, `grow<T>`, `dealloc<T>` | Direct malloc/realloc/free wrappers |
| `vec` | `Vec<T>` — growable array | Heap-backed, realloc on growth |
| `string` | `String` — owned mutable UTF-8 | Heap-backed buffer |
| `bytes` | `Bytes` — owned byte buffer | Heap-backed buffer |
| `text` | Borrowed text views over `String` | Borrows from alloc-layer types |
| `path` | Path manipulation, owned path buffer | String-backed, no filesystem access |
| `deque` | Double-ended queue | Heap-backed ring buffer |
| `heap` | Priority queue (binary heap) | Heap-backed array |
| `map` | `HashMap<K, V>` | Heap-backed open-addressing table |
| `set` | `HashSet<K>` | Thin wrapper over HashMap |
| `ordered_map` | Tree-based ordered map | Heap-backed nodes |
| `ordered_set` | Tree-based ordered set | Heap-backed nodes |
| `bitset` | Bit array / bitfield | Heap-backed for large sizes |

### Properties of alloc modules

- **Require `Alloc` capability.** Every allocating function declares `with(Alloc)`. Visible in `--report caps`.
- **Abort-on-OOM.** All allocation goes through `std.alloc` wrappers that null-check and abort. No silent null propagation.
- **No OS dependency beyond malloc/free/realloc/abort.** No file descriptors, no sockets, no processes, no clock.
- **Deterministic in the absence of OOM.** Same inputs produce same outputs. Allocation failure is the only non-deterministic event, and it terminates the process.
- **Linear ownership.** All alloc-layer types are linear by default. `Vec`, `String`, `Bytes`, `HashMap` must be explicitly freed or dropped. The compiler enforces this.

### What alloc is for

- General-purpose programs that need dynamic data structures
- Parsers that build owned ASTs
- Services that accumulate results
- Any program running in a hosted environment where heap is available

### Why alloc is separate from core

A bare-metal target may have no heap. An embedded system may provide a fixed-size arena instead of malloc. A WASM module may use its own linear memory allocator. By keeping alloc separate from core, programs that do not need heap allocation do not depend on it, and the allocator can be swapped or removed at the platform level.

### Why alloc is separate from hosted

Allocation needs only malloc/realloc/free — three symbols with well-defined behavior. It does not need file descriptors, sockets, process control, or any OS service beyond raw memory. Many constrained environments provide a heap allocator but not a full OS (RTOS, WASM, unikernels).

---

## Layer 3: Hosted Stdlib

### What belongs here

| Module | Purpose | Why hosted |
|--------|---------|------------|
| `io` | `print`, `println`, `TextFile`, `File::create/open` | POSIX write/fopen/fclose |
| `fs` | `read_file`, `write_file`, `append_file`, `file_exists`, `read_to_string` | POSIX file I/O |
| `net` | `TcpListener`, `TcpStream`, `bind`, `connect`, `accept` | POSIX sockets |
| `process` | `fork`, `spawn`, `kill`, `wait`, `getpid`, `exit` | POSIX process control |
| `env` | `get`, `set`, `unset` environment variables | POSIX getenv/setenv |
| `time` | `Instant`, `Duration`, `sleep`, `unix_timestamp` | POSIX clock_gettime/nanosleep |
| `rand` | `seed`, `random_int`, `random_range` | libc rand/srand |
| `args` | Command-line argument access | libc argc/argv |

### Properties of hosted modules

- **Require OS-specific capabilities.** `Network`, `Process`, `Console`, `Random`, `File`, `Clock`, `Env` — each maps to a real OS facility. Visible in `--report caps`.
- **Require `Unsafe` internally.** Hosted modules call libc functions through `trusted extern fn` declarations. The `Unsafe` is contained inside trusted wrappers — callers see domain capabilities, not raw `Unsafe`.
- **Not freestanding-compatible.** These modules assume POSIX libc is linked. They cannot run on bare metal, in a kernel, or in a minimal WASM environment.
- **Non-deterministic.** File contents change. Network connections fail. Clocks advance. PIDs vary. Environment variables are mutable global state.

### What hosted is for

- CLI tools that read files and print output
- Network services that accept connections
- Build tools that spawn subprocesses
- Any program that interacts with the operating system

---

## Layer 4: FFI Support

### What belongs here

FFI support is not a stdlib layer in the same sense as core/alloc/hosted. It is a cross-cutting concern that provides the machinery for crossing the language boundary.

| Component | Purpose | Trust level |
|-----------|---------|-------------|
| `trusted extern fn` declarations | Audited foreign bindings (e.g., `sqrt`, `abs`) | Trusted — programmer asserts safety |
| `extern fn` declarations | Raw foreign bindings | Unsafe — caller must have `with(Unsafe)` |
| Raw pointer types (`*const T`, `*mut T`) | C interop data | Copy, untracked by ownership |
| `#[repr(C)]` structs | C-ABI-compatible layout | Checked by `Layout.isFFISafe` |
| Null-terminated string conventions | C string interop | Manual — no stdlib helper yet |
| Layout guarantees | Field offsets, sizes, alignments for FFI structs | Documented in [ABI.md](ABI.md) |

### Current FFI gaps

These gaps were exposed by the Phase 2 pressure tests (`pressure_ffi_libc_wrapper.con`, `pressure_ffi_cabi.con`, `pressure_ffi_os_facade.con`):

- **No string passing to C.** Null-terminated string creation requires manual byte arrays. A future `CStr` or `CString` type would bridge this.
- **C struct interop requires manual layout.** Users must manually ensure field order and padding match C. A future `bindgen`-equivalent or layout verification tool would help.
- **No verified FFI envelopes.** Extern function contracts are trust-based. The compiler trusts the declared signature but cannot verify it matches the C header. Future work may add envelope verification.
- **No cross-language ownership protocol.** No way to express "C takes ownership" vs "C borrows" in an extern function signature.

### FFI and the trust boundary

FFI code sits outside all compiler verification. The trust model for FFI is:

1. `trusted extern fn` — programmer asserts the foreign function is safe to call with any valid arguments of the declared types. No `with(Unsafe)` required at call sites.
2. `extern fn` — raw foreign binding. Caller must have `with(Unsafe)`.
3. `trusted fn` wrapper — contains pointer arithmetic and extern calls behind a safe API. Declares its capabilities honestly.

The recommended pattern from [TRUSTED_BOUNDARY_GUIDE.md](TRUSTED_BOUNDARY_GUIDE.md) is: tiny trusted wrappers that do one unsafe operation each, with pure validation logic in separate safe functions.

---

## Capability / Trust Story

### How the split maps to capabilities

| Layer | Required capabilities | Capability visible in signatures? |
|-------|----------------------|-----------------------------------|
| Core | None (`caps: (pure)`) | No capabilities needed |
| Alloc | `Alloc` | Yes — `with(Alloc)` |
| Hosted | Domain caps (`File`, `Network`, `Process`, `Console`, `Clock`, `Env`, `Random`) + internal `Unsafe` | Yes — domain caps visible; `Unsafe` contained in trusted wrappers |
| FFI | `Unsafe` (for raw extern calls) | Yes — `with(Unsafe)` or absorbed by `trusted extern fn` |

### What "capability-declared" means

Hosted modules should declare what capabilities they need, not accumulate ambient authority. This is already enforced by Concrete's capability system:

- A function can only call functions whose capabilities are a subset of its own
- `--report caps` shows the full authority chain for any function
- `--report authority` shows transitive capability propagation
- Policy enforcement via `Concrete.toml` can restrict which capabilities a project uses

A hosted module that needs `File` and `Alloc` declares both. A caller that only has `Console` cannot call it. The boundary is compiler-enforced, not documentary.

### How trust flows across layers

```
Core (pure)
  │
  │  no trust boundary — all code is safe, enforced
  │
Alloc (Alloc capability)
  │
  │  trust boundary: std.alloc wraps malloc/free in trusted fn
  │  callers see with(Alloc), not with(Unsafe)
  │
Hosted (domain capabilities)
  │
  │  trust boundary: each hosted module wraps libc in trusted fn
  │  callers see with(File), with(Network), etc.
  │  Unsafe is contained inside trusted wrappers
  │
FFI (Unsafe capability)
  │
  │  trust boundary: extern fn declarations
  │  callers either have with(Unsafe) or use trusted extern fn
```

### Audit commands for the split

| Question | Command |
|----------|---------|
| Does this function allocate? | `--report alloc` or `--report caps` (look for `Alloc`) |
| Does this function use the OS? | `--report caps` (look for `File`, `Network`, `Process`, etc.) |
| Does this function cross the FFI boundary? | `--report unsafe` (shows trusted extern and extern fn calls) |
| Is this function freestanding-compatible? | `--report caps` — must show `caps: (pure)` |
| What is the full authority chain? | `--report authority` |

---

## Comparison With Other Languages

### Rust: `core` vs `alloc` vs `std`

Rust has a three-layer split that closely parallels Concrete's:

| Rust | Concrete | Boundary |
|------|----------|----------|
| `core` | Core stdlib | No OS, no allocator |
| `alloc` | Alloc stdlib | Needs allocator, no OS |
| `std` | Hosted stdlib | Full OS |

**Similarity:** Both separate "needs allocator" from "needs OS." Both allow `#![no_std]` programs that use only core.

**Difference:** Rust's `alloc` layer is mature and widely used in embedded. Concrete's alloc layer exists but the freestanding mode is not yet implemented — the split is currently documentary, enforced by the capability system but not by a separate compilation target. Rust uses `#![no_std]` as a crate-level attribute; Concrete intends `[profile] target = "freestanding"` in `Concrete.toml`.

### Zig: unified std with comptime OS detection

Zig has a single `std` namespace. OS-dependent code uses `@import("std").os` and comptime `@hasDecl` / target detection to conditionally include platform-specific implementations.

| Zig | Concrete |
|-----|----------|
| Single `std` namespace | Three explicit layers |
| Comptime target detection | Capability declarations in signatures |
| OS code included but may not compile for all targets | OS code in separate hosted layer, excluded from freestanding |
| Allocator parameter on collections | `with(Alloc)` capability on allocating functions |

**Zig's advantage:** One namespace, simpler mental model. Allocator-as-parameter gives fine-grained control.

**Concrete's advantage:** The split is visible in function signatures, not just in whether the code happens to compile. `--report caps` answers "does this code use the OS?" without reading the implementation. The capability system makes the boundary auditable, not just compilable.

### Go: monolithic stdlib

Go has a single, large stdlib. There is no core/alloc/hosted distinction. All packages may use the garbage collector, goroutine scheduler, and OS facilities.

| Go | Concrete |
|----|----------|
| Monolithic stdlib | Three layers |
| GC always available | Allocation is a declared capability |
| Goroutines always available | Concurrency is a future declared capability |
| No freestanding story | Freestanding is a design target |

**Go's advantage:** Simplicity — everything is always available.

**Concrete's disadvantage:** More ceremony. Functions must declare capabilities.

**Concrete's advantage:** The capability system makes the boundary between "pure computation" and "needs the OS" visible and enforceable. A Go function that happens not to use the OS still has access to it. A Concrete core function provably does not.

---

## Implementation Status

The three-layer classification exists in documentation ([EXECUTION_MODEL.md](EXECUTION_MODEL.md), [STDLIB.md](STDLIB.md)) and is enforced indirectly by the capability system. What remains:

| Item | Status |
|------|--------|
| Layer classification documented | Done |
| Capability system enforces layer boundaries indirectly | Done — core functions have no capabilities; hosted functions declare them |
| `[profile] target = "freestanding"` in Concrete.toml | Not yet implemented |
| Compiler rejects stdlib imports in freestanding mode | Not yet implemented |
| Separate compilation of core-only vs full stdlib | Not yet implemented |
| `fmt` split into core (buffer-writing) and alloc (String-producing) APIs | Not yet implemented |
| `CStr` / `CString` types for FFI string interop | Not yet implemented |
| Module-level layer annotations (e.g., `#[layer(core)]`) | Not yet designed |

The capability system already provides the enforcement mechanism. The remaining work is making the layer boundary a first-class compilation target, not just a documentary classification.

---

## Summary

| Layer | Host dependency | Capabilities | Freestanding? | Proof-friendly? |
|-------|----------------|-------------|---------------|-----------------|
| Core | None | `(pure)` | Yes | Yes — many functions are proof-eligible |
| Alloc | malloc/free/abort | `Alloc` | No (needs allocator) | Partially — allocation blocks proof eligibility |
| Hosted | Full POSIX libc | Domain caps + internal `Unsafe` | No (needs OS) | No — I/O blocks proof eligibility |
| FFI | Foreign code | `Unsafe` or `trusted extern fn` | Depends on target | No — FFI blocks proof eligibility |

The split follows one rule: a module belongs in the lowest layer that provides everything it needs. Pure computation is core. Heap-backed data structures are alloc. OS interaction is hosted. Foreign code is FFI. The capability system makes this visible and enforceable in every function signature.
