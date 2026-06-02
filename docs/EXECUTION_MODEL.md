# Execution Model

Status: reference

This document defines Concrete's execution model: how programs start, allocate, fail, and interact with the host environment. It covers the hosted/freestanding distinction, the runtime boundary, the memory/allocation strategy, target support policy, stdlib alignment, execution profiles, performance validation, verified FFI envelopes, and the concurrency direction.

For the value and ownership model, see [VALUE_MODEL.md](VALUE_MODEL.md).
For the safety model (capabilities, trusted, Unsafe), see [SAFETY.md](SAFETY.md).
For ABI and layout details, see [ABI.md](ABI.md).
For stdlib module inventory, see [STDLIB.md](STDLIB.md).

---

## Hosted vs Freestanding

Concrete currently targets a **hosted** environment only. All programs assume:

- a POSIX-like OS with process model, file descriptors, and virtual memory
- libc is available and linked (malloc, free, printf, read, write, etc.)
- 64-bit address space (pointer size = 8 bytes, hardcoded in Layout)
- `main` is the entry point, called by the OS/libc startup code

### What "hosted" means concretely

The compiler generates a `main` function that calls the user's `main` (renamed to `user_main` internally). The generated `main`:

1. Calls `user_main`
2. Optionally prints the return value (for scalar types: i32/i64 → `printf "%lld"`, bool → `"true"`/`"false"`)
3. Returns `0` to the OS

There is no Concrete runtime initialization. No global constructors, no GC setup, no thread-local storage initialization, no allocator setup. The program starts in `main`, calls libc functions directly, and exits when `main` returns.

### What "freestanding" would mean (future)

A future freestanding mode would remove the assumption of libc and OS facilities. This is not implemented but the direction is documented here so the hosted boundary remains explicit.

Freestanding would mean:

- no libc-backed assumptions by default
- no ambient runtime (no implicit malloc/free)
- allocation only if explicitly provided
- explicit target contract for startup, failure, and memory
- stdlib limited to a core subset (Option, Result, math, ptr, slice, mem)

The hosted/freestanding split is a later milestone (see `research/stdlib-runtime/no-std-freestanding.md`). The current priority is making the hosted boundary explicit so the split is straightforward when needed.

### Stdlib layer classification

The stdlib naturally divides into layers by host dependency:

| Layer | Modules | Host assumption |
|-------|---------|-----------------|
| **Core** | `option`, `result`, `mem`, `slice`, `math`, `fmt`, `hash`, `parse` | None — pure computation, no libc |
| **Alloc** | `alloc`, `vec`, `string`, `bytes`, `text`, `deque`, `heap`, `ordered_map`, `ordered_set`, `bitset`, `map`, `set` | malloc/realloc/free only |
| **Hosted** | `io`, `fs`, `env`, `process`, `net`, `time`, `rand` | Full POSIX libc |

Today these are all compiled together. The classification exists to guide future separation and to make host dependencies auditable now.

---

## Runtime Boundary

### What constitutes the "runtime"

Concrete does not have a runtime in the traditional sense (no GC, no green threads, no event loop). The runtime boundary is the set of external symbols and conventions that a compiled Concrete program depends on at link time.

### Startup

1. OS/libc calls `main(argc, argv)` (Concrete ignores argc/argv today)
2. `main` calls `user_main` (the user's `fn main()`)
3. No global initialization, no module init functions, no static constructors

### Shutdown

1. `user_main` returns
2. `main` optionally prints the return value
3. `main` returns `0`
4. OS reclaims process resources

There is no cleanup hook, no `atexit` registration, no destructor ordering. Linear ownership and `defer` handle resource cleanup within function scope. When the process exits, the OS reclaims everything.

### Failure

Concrete has no panic/unwind mechanism. Failure modes:

| Failure | What happens | Who handles it |
|---------|-------------|----------------|
| Explicit error return | `Result<T, E>` propagated via `?` | User code |
| `exit(code)` | libc `exit()` terminates process | OS |
| Out-of-memory | `malloc` returns null → `abort()` (see allocation section) | Process terminates |
| Null pointer dereference | Hardware trap (SIGSEGV) | OS kills process |
| Integer overflow | Wraps (LLVM default for `add`/`sub`/`mul`) | Silent — no trap |
| Array out-of-bounds | Checked accessors return `Option`; unchecked is UB | User code / UB |
| Stack overflow | OS stack guard page, SIGSEGV | OS kills process |

There is no structured panic. No stack unwinding. No catch mechanism. This is intentional — it keeps the execution model simple and compatible with freestanding targets in the future. Error handling is explicit through return types.

### External symbol dependencies

Every compiled Concrete program links against these categories of external symbols:

**Always required** (emitted by the compiler):

| Symbol | Source | Used by |
|--------|--------|---------|
| `malloc` | libc | String builtins, Vec builtins, user `Alloc` code |
| `free` | libc | String/Vec deallocation, `drop_string` |
| `realloc` | libc | Vec growth |
| `memcpy` / `llvm.memcpy.p0.p0.i64` | libc / LLVM intrinsic | Vec operations, string operations |
| `memset` | libc | Vec pop (Option zero-init) |
| `memcmp` | libc | `string_eq`, `string_contains` |
| `printf` | libc | Main wrapper (result printing) |
| `snprintf` | libc | `int_to_string`, `float_to_string` |
| `strtol` | libc | `string_to_int` |
| `llvm.smax.i64` / `llvm.smin.i64` | LLVM intrinsics | `string_slice` |
| `abort` | libc | OOM check (`__concrete_check_oom`), `std.alloc` wrappers |

**Conditionally required** (from user stdlib imports):

| Symbol | Source | Used by stdlib module |
|--------|--------|-----------------------|
| `fopen`, `fclose`, `fread`, `fwrite`, `fseek`, `ftell` | libc | `std.io`, `std.fs` |
| `read`, `write` | libc | `std.io` |
| `getenv`, `setenv`, `unsetenv` | libc | `std.env` |
| `fork`, `execvp`, `waitpid`, `kill`, `getpid` | libc | `std.process` |
| `socket`, `bind`, `listen`, `accept`, `connect`, `send`, `recv`, `close` | libc | `std.net` |
| `htons`, `htonl`, `inet_pton`, `setsockopt` | libc | `std.net` |
| `clock_gettime`, `nanosleep`, `time` | libc | `std.time` |
| `rand`, `srand` | libc | `std.rand` |
| `exit`, `raise` | libc | `std.libc` |

### Test mode

In test mode (`concrete file.con --test`), the compiler generates a test runner `main` instead of the normal main wrapper. The test runner:

1. Calls each `#[test]` function in order
2. Prints `PASS: <name>` or `FAIL: <name>` for each
3. Returns `0` if all pass, `1` if any fail

---

## Memory and Allocation Strategy

### Current model: libc malloc

All heap allocation goes through libc `malloc`/`realloc`/`free`. There is no custom allocator, no arena, no bump allocator, no GC. The compiler emits direct calls to these functions in:

- **String builtins** (`EmitBuiltins.lean`): `string_concat`, `string_slice`, `string_trim`, `int_to_string`, `float_to_string`, `bool_to_string` all call `malloc` for new buffers. `drop_string` and `string_concat` call `free`.
- **Vec builtins** (`EmitBuiltins.lean`): `vec_new_{size}` calls `malloc` with initial capacity × element size. `vec_push_{size}` calls `realloc` when full (2× growth). `vec_free` calls `free`.
- **User code** via `std.alloc`: `heap_new<T>()`, `grow<T>()`, `dealloc<T>()` wrap malloc/realloc/free with `trusted` + `Alloc` capability.

### Allocation is capability-tracked

The `Alloc` capability marks functions that allocate. This is enforced by `CoreCheck` — a function calling `heap_new`, `Vec::new`, or `String::concat` must declare `with(Alloc)` or be called by a function that does. This makes allocation visible in:

- `--report caps`: shows which functions require `Alloc` and why
- `--report alloc`: shows allocation/cleanup patterns and warns about functions that allocate without cleanup

### Allocation failure

**Abort on OOM.** All allocation paths check for null returns from `malloc`/`realloc` and abort the process immediately if allocation fails. This matches Rust's default allocator behavior and is appropriate for hosted programs.

The abort-on-OOM guarantee covers two layers:

1. **Compiler builtins** (`EmitBuiltins.lean`): All 11 `malloc`/`realloc` call sites in string and vec builtins pipe through `__concrete_check_oom`, a compiler-emitted helper that null-checks and calls `abort()`.
2. **Stdlib wrappers** (`std/src/alloc.con`): `heap_new<T>()` and `grow<T>()` null-check their `malloc`/`realloc` results and call `abort()` on failure.

This means any allocation reachable through the standard API is OOM-safe. Direct `extern fn malloc` calls from user `trusted` code are not checked — the user is responsible for null-checking in that case.

Future directions beyond abort-on-OOM:

1. **Propagate OOM as error**: builtins return `Result<T, AllocError>`. Correct but invasive — changes the signature of every allocating operation.
2. **Allocator trait**: user-provided allocator with configurable failure behavior. Most flexible but highest complexity.

### Deallocation model

Concrete uses linear ownership + `defer` for deterministic deallocation:

- **Linear types**: structs and enums must be consumed exactly once. The compiler enforces this at check time.
- **`defer`**: schedules cleanup at scope exit. The standard pattern is `defer vec.free();` or `defer string.drop();`.
- **`Destroy` trait**: types implementing `Destroy` get their `destroy()` method called when they go out of scope (not yet automatic — users must call it or use `defer`).

There is no automatic destructor insertion (RAII). Resource cleanup is explicit. This is intentional — it keeps the execution model transparent and avoids hidden control flow.

### Stack allocation

Local variables and temporaries are stack-allocated via LLVM `alloca`. The compiler uses entry-block allocas for:

- aggregate variables that are modified across control flow (promoted from phi nodes to stable storage)
- temporary structs for pass-by-pointer ABI
- local variables that need an address (e.g., `&mut` borrows)

Stack size is not bounded or checked by the compiler. Stack overflow is caught by the OS guard page.

### Memory layout

All memory layout decisions are centralized in `Concrete/Layout.lean`:

- Type sizes and alignments follow platform conventions (i8=1, i16=2, i32=4, i64=8, f64=8, ptr=8, bool=1)
- Structs use C-like field layout with natural alignment padding (unless `#[repr(packed)]`)
- Enums use tag + padded payload (i32 tag, payload aligned to max variant alignment)
- `#[repr(C)]` provides C-compatible layout for FFI structs
- `#[repr(align(N))]` sets minimum alignment

See [ABI.md](ABI.md) for full details.

### Future directions

| Direction | Description | Phase |
|-----------|-------------|-------|
| Abort on OOM | Check malloc returns in builtins, abort on null | E (done) |
| Bounded allocation profile | Compile-time cap on allocation count/size for high-integrity use | E/F |
| Allocator parameter | User-provided allocator for collections (Zig-style) | G+ |
| No-alloc mode | Compile without malloc/free for freestanding targets | G+ |
| Arena/bump allocators | Stdlib allocator implementations beyond libc malloc | G+ |

---

## FFI and Runtime Ownership Boundary

This section documents how ownership, capabilities, and resource tracking interact with the FFI boundary. For FFI type rules and safety checks, see [FFI.md](FFI.md). For ABI and calling convention details, see [ABI.md](ABI.md).

### Capability model at the FFI boundary

Extern functions participate in the capability system:

| Declaration | Capability requirement | Use case |
|------------|----------------------|----------|
| `extern fn foo(...)` | Caller must have `Unsafe` | Raw foreign calls |
| `trusted extern fn bar(...)` | No capability required | Audited pure functions (math, abs) |
| `trusted fn wrap(...) with(Alloc, Unsafe)` calling extern fn | Caller must have `Alloc` and `Unsafe` | Wrappers that audit raw pointer use but still expose `Unsafe` |

The standard pattern is a three-layer stack:

1. **libc declaration** (`std.libc`): raw `extern fn malloc(size: u64) -> *mut u8`
2. **trusted wrapper** (`std.alloc`): `trusted fn heap_new<T>() with(Alloc, Unsafe) -> *mut T` — calls malloc, null-checks, casts the pointer. The `trusted` marker means raw pointer operations inside are audited, but `Unsafe` is still visible to callers.
3. **user code**: calls `heap_new<T>()` with both `Alloc` and `Unsafe` capabilities

Today `trusted` allows raw pointer operations without additional checks inside the function body, but it does **not** hide capabilities from callers. The declared `with(...)` set is the caller-visible contract. A future capability-hiding mechanism (where a trusted wrapper could absorb `Unsafe` and expose only `Alloc`) is not yet implemented.

### Ownership across FFI calls

The linearity checker tracks ownership at the Concrete level:

- **By-value arguments to extern fn consume the variable.** If you pass a linear type by value, it is marked consumed. The compiler assumes the foreign function took ownership.
- **By-reference arguments (`&T`, `&mut T`) borrow without consuming.** The original variable remains usable after the call.
- **Raw pointers (`*mut T`, `*const T`) are Copy.** No ownership tracking. The compiler cannot see what C code does with a pointer.

### What the compiler cannot track

Once data crosses the FFI boundary, the compiler has no visibility:

| Scenario | Compiler behavior | Risk |
|----------|------------------|------|
| Passing `*mut T` to extern fn | No tracking (Copy type) | C code may free, leak, or corrupt |
| Extern fn returns `*mut T` | No obligation to free | Caller may leak if they drop the pointer |
| Trusted code extracts `.ptr` from `Vec`/`String` | `Vec`/`String` is consumed, but buffer is now a raw pointer | Double-free if both Concrete and C free it |
| Extern fn writes beyond buffer bounds | No defense | Buffer overflow / UB |

These gaps are intentional — they match the cost model of C FFI in Rust, Zig, and other systems languages. The mitigation is:

1. **`trusted fn` wrappers** that present a safe interface
2. **Capability tracking** that makes FFI usage visible in reports
3. **`--report unsafe`** that shows which trusted functions wrap which extern calls

### What is NOT allowed at the FFI boundary

The compiler enforces that only FFI-safe types appear in `extern fn` signatures:

- **Allowed**: integers, floats, bool, char, `()`, raw pointers (`*mut T`, `*const T`), `#[repr(C)]` structs
- **Rejected at compile time**: `String`, `Vec<T>`, `HashMap<K,V>`, non-repr(C) structs, enums, arrays, references

This prevents accidentally passing a managed Concrete type to C code that doesn't understand its layout.

### Calling convention

`#[repr(C)]` structs in `extern fn` signatures are passed by value following the platform C ABI. LLVM handles the register/stack lowering for the target architecture. Internal Concrete function calls use pointer-based passing for all aggregates.

For full calling convention details, see [ABI.md](ABI.md).

### Known gaps and future directions

| Gap | Description | Planned mitigation |
|-----|-------------|-------------------|
| Raw pointer leaks | `*mut T` from extern fn can be dropped without freeing | Linear pointer wrappers or `must_use` annotation |
| No verified FFI envelopes | Extern fn contracts are trust-based, not mechanically checked | Verified FFI envelopes (Phase E item 9) |
| No cross-language ownership protocol | No way to express "C takes ownership" vs "C borrows" | Ownership annotations on extern fn parameters |

---

## Target and Platform Support Policy

Concrete currently supports a single target profile:

| Property | Value |
|----------|-------|
| Architecture | 64-bit only (pointer = 8 bytes, hardcoded in `Layout.lean`) |
| OS | POSIX-like (Linux, macOS) |
| Linker | System linker via `clang` |
| LLVM backend | Host-native target triple (whatever `clang` defaults to) |
| Endianness | Little-endian assumed (not verified) |

### Support tiers

| Tier | Definition | Current targets |
|------|-----------|-----------------|
| **Tier 1** | CI-tested, ABI documented, release-blocking | `x86_64-linux` (CI runs on `ubuntu-latest`) |
| **Tier 2** | Developer-tested, builds and tests pass locally, no CI | `aarch64-darwin` (macOS Apple Silicon — primary development machine) |
| **Experimental** | May compile, no compatibility promise | `x86_64-darwin`, everything else LLVM can target |

A target is not "supported" just because LLVM can emit code for it. Supported means:

- layout assumptions are documented and tested
- ABI behavior matches documented conventions
- the runtime boundary (startup, allocation, failure) works as specified
- core stdlib modules compile and pass tests
- reports remain truthful

### What is target-dependent

| Component | Target-dependent? | Notes |
|-----------|-------------------|-------|
| Type layout (sizes, alignment) | Partially — pointer size hardcoded to 8 | 32-bit would need `Layout.lean` changes |
| Struct layout | No — follows C conventions, LLVM handles padding | `#[repr(C)]` verified against C compiler on Tier 1 |
| Calling convention | Partially — LLVM handles register/stack lowering | By-value repr(C) struct passing relies on LLVM's target ABI |
| Allocation | No — always libc malloc/realloc/free | |
| External symbols | Partially — POSIX assumed | Freestanding would need different symbol set |
| Test suite | Partially — some tests use network/process | Skippable via `--fast` mode |

### What is not yet validated empirically

- Cross-compilation (building for a different target triple than the host)
- 32-bit targets (pointer size is hardcoded to 8)
- Non-POSIX hosted environments (Windows, WASI)
- Struct layout agreement between Concrete and C compilers on Tier 2/Experimental targets

### Future directions

| Direction | Description | Phase |
|-----------|-------------|-------|
| Cross-target CI | Compile + link + run tests on both x86_64 and aarch64 | F |
| Empirical layout validation | Compare Concrete struct layout against C compiler output | F |
| 32-bit support | Make pointer size configurable in Layout | G+ |
| Freestanding targets | Remove POSIX/libc assumptions for bare-metal | G+ |
| Windows hosted | POSIX compatibility layer or native Win32 | G+ |

---

## Stdlib and the Execution Model

The stdlib is classified into three layers by host dependency (see [Hosted vs Freestanding](#hosted-vs-freestanding) above). This section documents how stdlib modules align with the execution model and where the boundaries are.

### Module-to-layer mapping

| Module | Layer | Capabilities | Host dependencies |
|--------|-------|-------------|-------------------|
| `option` | Core | None | None |
| `result` | Core | None | None |
| `mem` | Core | None | None — `sizeof`/`alignof` are compile-time |
| `slice` | Core | None | None |
| `math` | Core | None | None |
| `fmt` | Core | None | None |
| `hash` | Core | None | None |
| `parse` | Core | None | None |
| `test` | Core | None | Uses `printf` for output (compiler-inserted) |
| `alloc` | Alloc | `Alloc`, `Unsafe` | `malloc`, `realloc`, `free`, `abort` |
| `vec` | Alloc | `Alloc` | Via `alloc` |
| `string` | Alloc | `Alloc` | Via builtins (malloc/free/memcpy) |
| `bytes` | Alloc | `Alloc` | Via `alloc` |
| `text` | Alloc | `Alloc` | Via `alloc` |
| `deque` | Alloc | `Alloc` | Via `alloc` |
| `heap` | Alloc | `Alloc` | Via `alloc` |
| `ordered_map` | Alloc | `Alloc` | Via `alloc` |
| `ordered_set` | Alloc | `Alloc` | Via `alloc` |
| `bitset` | Alloc | `Alloc` | Via `alloc` |
| `map` | Alloc | `Alloc` | Via `alloc` |
| `set` | Alloc | `Alloc` | Via `alloc` |
| `path` | Alloc | `Alloc` | Via `alloc` (path manipulation only, no fs) |
| `io` | Hosted | `Unsafe`, `Console` | TextFile: `fopen`/`fclose`/`fread`/`fwrite`; print/println: `Console` |
| `fs` | Hosted | `Unsafe` | `fopen`, `fread`, `fwrite`, `fseek`, `ftell`, `fclose` |
| `env` | Hosted | `Unsafe` | `getenv`, `setenv`, `unsetenv` |
| `process` | Hosted | `Unsafe`, `Process` | `fork`, `execvp`, `waitpid`, `kill`, `getpid` |
| `net` | Hosted | `Unsafe`, `Network` | `socket`, `bind`, `listen`, `accept`, `connect`, `send`, `recv` |
| `time` | Hosted | `Unsafe` | `clock_gettime`, `nanosleep`, `time` |
| `rand` | Hosted | `Unsafe`, `Random` | `rand`, `srand` |

### What the execution model constrains in stdlib

1. **No hidden allocation.** Every stdlib function that allocates must declare `with(Alloc)`. The `--report alloc` output makes this auditable.
2. **No hidden host access.** Hosted modules require their respective capability (`Network`, `Process`, `Random`, `Console`, etc.) or `Unsafe` for direct libc calls. A function that only does computation cannot accidentally pull in network or filesystem dependencies.
3. **Deterministic cleanup.** All stdlib types that own resources provide explicit `free()`/`drop()`/`close()` methods. There is no implicit destructor insertion.
4. **Abort-on-OOM.** All allocation through `std.alloc` and compiler builtins aborts on null. Stdlib types built on `Vec`/`String` inherit this guarantee.

### What is not yet enforced

- The layer classification is documentary, not compiler-enforced. A core module could import a hosted module without a compiler error (the capability system would catch the effect mismatch, but there's no "this module is core-only" annotation).
- `test` module output goes through compiler-inserted `printf` calls, technically making it hosted, but tests are not production code.

---

## Execution Profiles

Concrete does not yet implement execution profiles, but the direction is documented here so the constraints inform Phase F and G work.

### What execution profiles would provide

An execution profile is a set of compiler-enforced restrictions on what a program (or module/package) may do at runtime. The goal is to support high-integrity use cases without making all Concrete code live under maximum restriction.

### Planned profiles

| Profile | Restrictions | Use case |
|---------|-------------|----------|
| **`default`** | No restrictions — full hosted Concrete | General-purpose systems programming |
| **`no_alloc`** | No `Alloc` capability allowed; no malloc/realloc/free | Embedded, interrupt handlers, boot code |
| **`bounded_alloc`** | `Alloc` allowed but with compile-time allocation budget | Safety-critical code with bounded resource use |
| **`no_unsafe`** | No `Unsafe` capability; no raw pointer operations | Application code that should not touch raw memory |
| **`no_ffi`** | No `extern fn` calls; no `Unsafe` | Pure Concrete code with no foreign dependencies |
| **`high_integrity`** | `no_unsafe` + `bounded_alloc` + no ambient authority growth | Auditable, provable subsystems |

### How profiles would work

1. **Declaration**: a module or package declares its profile (e.g., `#[profile(no_alloc)]`)
2. **Enforcement**: the compiler rejects code that violates the profile's restrictions during capability checking
3. **Reporting**: `--report profile` shows which functions/modules satisfy which profiles and where violations occur
4. **Composability**: a `high_integrity` module can call `no_alloc` and `no_unsafe` code. A `default` module can call any profile.

### What makes this feasible

Concrete's existing capability system already tracks `Alloc`, `Unsafe`, `Network`, `Process`, `Random`, `Console`, etc. Profiles are essentially named capability budgets — a `no_alloc` profile is "reject any function that requires `Alloc`." The enforcement mechanism already exists; profiles would add named sets and module-level declarations.

### Relationship to proofs

The `high_integrity` profile would closely align with `ProofCore` eligibility. Code that satisfies the high-integrity profile is more likely to be provable because it avoids `Unsafe`, bounds allocation, and has no FFI dependencies. The profile and proof eligibility checks could share the same underlying filter.

---

## Performance Validation Direction

Concrete does not yet have performance benchmarks or regression tracking. This section documents the intended approach so it can be implemented incrementally.

### Principles

1. **Representative workloads, not microbenchmarks.** Performance should be measured on programs that resemble real use, not isolated operations. The integration test corpus (12 programs) is a starting point.
2. **Compilation time matters.** The compiler itself is a performance-sensitive artifact. `lake build` time and per-file compilation time should be tracked.
3. **Runtime performance is secondary to correctness.** Concrete delegates optimization to LLVM (`-O2`). The compiler's job is to emit clean, correct LLVM IR — not to implement its own optimization passes.
4. **Observability over cleverness.** Debug info quality, stack trace usefulness, and report truthfulness should not be sacrificed for performance. Optimization policy should explicitly account for these.

### What should be measured

| Metric | How | Priority |
|--------|-----|----------|
| Full test suite time | `time bash run_tests.sh --full` | High — developer experience |
| Compiler build time | `time lake build` | High — contributor experience |
| Per-test compilation time | Compiler output cache miss timing | Medium |
| Runtime performance of integration programs | Wall-clock time of compiled binaries | Low (LLVM handles optimization) |
| Binary size | `ls -la` on compiled outputs | Low |

### What counts as a regression

- Test suite time increasing >20% without new tests added
- Compiler build time increasing >30% without new passes added
- Any integration program becoming >2x slower at `-O2` without code changes

### What is explicitly out of scope

- Custom optimization passes in the Concrete compiler (rely on LLVM)
- Target-specific peephole optimizations
- Benchmark suites that don't correspond to real usage patterns
- Performance comparisons with other languages (premature at this stage)

### Future directions

| Direction | Description | Phase |
|-----------|-------------|-------|
| Compile-time baselines | Track `lake build` and test suite times in CI | F |
| Integration program timing | Automated wall-clock measurement of compiled integration tests | F |
| Binary size tracking | Report compiled binary sizes as part of test output | G |
| Profiling integration | Document how to profile Concrete programs (perf, instruments, etc.) | G |

---

## Verified FFI Envelopes and Structural Boundedness

This section documents the direction for making FFI boundaries and resource usage mechanically verifiable, beyond the current trust-based model.

### Verified FFI envelopes

Today, `extern fn` contracts are trust-based: the programmer declares the signature, and the compiler trusts it. A verified FFI envelope would add mechanical checking:

| Aspect | Current | With verified envelopes |
|--------|---------|------------------------|
| Parameter types | Programmer-declared, compiler trusts | Checked against C header or binding spec |
| Return type | Programmer-declared | Checked against C header or binding spec |
| Null safety | Not checked (user responsibility) | Envelope can declare "never returns null" or "may return null" |
| Buffer size | Not checked | Envelope can declare size constraints |
| Ownership transfer | Not expressed | Envelope can declare "takes ownership" vs "borrows" |

### How envelopes would work

1. **Declaration**: an `extern fn` can optionally have an envelope annotation describing pre/postconditions
2. **Verification source**: envelopes could be checked against C header files, manual annotations, or generated bindings
3. **Compiler integration**: the compiler would reject calls that violate envelope constraints (e.g., passing a potentially-null pointer to an envelope that requires non-null)
4. **Report integration**: `--report ffi` would show which extern functions have verified envelopes and which are trust-only

### Structural boundedness reporting

Structural boundedness means the compiler can statically determine resource usage bounds for a function or module:

| Property | What it means | How it's checked |
|----------|--------------|------------------|
| Allocation-free | Function never allocates | No `Alloc` in capability set |
| Bounded allocation | Function allocates at most N bytes | Static analysis of allocation arguments (future) |
| Stack-bounded | Function uses at most N bytes of stack | Static analysis of allocas (future) |
| No FFI | Function makes no foreign calls | No `Unsafe` in capability set, no extern fn calls |
| Terminating | Function always terminates | Fuel-bounded or structurally decreasing recursion (future) |

### What exists today

- **Allocation tracking**: `--report alloc` shows which functions allocate and where
- **Capability tracking**: `--report caps` shows full capability requirements
- **Unsafe tracking**: `--report unsafe` shows trusted/extern boundaries
- **Proof eligibility**: `ProofCore` extraction identifies the pure, provable fragment

### What is needed

- **FFI envelope syntax and checker** (Phase F/G)
- **Static allocation size analysis** for bounded-allocation profiles (Phase F)
- **Stack depth analysis** for stack-bounded guarantees (Phase G)
- **`--report boundedness`** that summarizes which functions satisfy which structural bounds (Phase F)

---

## Concurrency and Execution Model

Concrete does not yet implement concurrency. This section documents the intended direction so that runtime, capability, and ownership decisions made now remain compatible with the concurrency model. The more specific async/evidence research direction lives in [../research/stdlib-runtime/async-concurrency-evidence.md](../research/stdlib-runtime/async-concurrency-evidence.md).

### Design principles

1. **Explicit over ambient.** Thread creation, blocking, and synchronization should require explicit capabilities, not be available by default.
2. **Structured over detached.** Concurrent work should belong to explicit scopes with defined lifetimes. Fire-and-forget spawning should be rare and marked.
3. **Threads first, async later.** OS threads are the base primitive. Evented I/O and executor-driven async are specialized later additions, not the default.
4. **Ownership across threads.** Values moved into spawned threads are consumed (linear move). Shared mutable state requires explicit synchronization types.
5. **Capability-gated.** Spawning, blocking, waiting, synchronization, time, and eventual async/evented I/O should be visible as capabilities or explicit runtime values. The current research candidates include `Async`, `Concurrent`, `Sync`, `Clock`, and `Cancellable`; exact names are not implemented or frozen.

### First concurrency model (future Phase 12 work)

The initial model targets hosted environments only:

| Component | Design |
|-----------|--------|
| Primitive | OS threads via libc `pthread_create`/`pthread_join` |
| Spawn API | `thread::spawn(f, arg) -> JoinHandle<T>` in stdlib |
| Join API | `handle.join() -> T` — blocks until thread completes |
| Ownership | Arguments to `spawn` are moved (consumed). Return value is moved back via `join`. |
| Channels | `channel::new<T>() -> (Sender<T>, Receiver<T>)` — bounded MPSC |
| Capabilities | Thread creation, blocking, and channel operations must be capability-gated. Exact capability names are still research. |
| Shared state | Not in first model. Mutex/atomics are Stage 2. |

### What the capability system should provide

```
fn process_batch(data: Vec<Work>) with(Concurrent, Alloc) -> Vec<Result> {
    let (tx, rx) = channel::new::<Result>(data.len());
    for item in data {
        thread::spawn(fn(item: Work, tx: Sender<Result>) with(Alloc) {
            tx.send(do_work(item));
        }, item, tx.clone());
    }
    return rx.collect();
}
```

This is illustrative, not current syntax. The intended signature makes visible:
this function creates concurrent work and allocates. `--report caps` should
eventually show exactly which functions in a module require optional overlap
(`Async`), required concurrent progress (`Concurrent`), synchronization
(`Sync`), clock access (`Clock`), or cancellation (`Cancellable`).

### Staging

| Stage | Scope | Phase |
|-------|-------|-------|
| 1 | OS threads, spawn/join, channels, move ownership | J |
| 2 | cross-task transfer rules, synchronized shared-state types, mutex, atomics | J |
| 3 | Audit/report integration (where threads spawn, where blocking occurs) | J |
| 4 | structured concurrency scopes, linear handles, deadlines, race/select | J+ |
| 5 | deterministic simulation backend and concurrency evidence artifacts | J+ |
| 6 | evented I/O runtime (specialized, not default) | J+ |

### What concurrency does NOT change

- The runtime boundary remains minimal (no GC, no green threads, no event loop in the default runtime)
- Linear ownership semantics are unchanged — `spawn` consumes the closure's captured values
- Allocation model is unchanged — threads use the same libc malloc with abort-on-OOM
- Failure model is unchanged — no panic/unwind, threads that fail return errors via channels or join

### What to avoid

- Rust-style async/await as a first-class language feature before the thread model is stable
- Runtime fragmentation (multiple incompatible executor ecosystems)
- Hidden executors or schedulers in stdlib
- Function coloring that forces non-concurrent code into async signatures
- Ambient concurrency — spawning should be as visible as allocation
- Detached tasks outside a structured scope as the default lifecycle model

See [../research/stdlib-runtime/concurrency.md](../research/stdlib-runtime/concurrency.md) for the full near-term design, [../research/stdlib-runtime/long-term-concurrency.md](../research/stdlib-runtime/long-term-concurrency.md) for the long-term layered model, and [../research/stdlib-runtime/async-concurrency-evidence.md](../research/stdlib-runtime/async-concurrency-evidence.md) for the evidence-bearing async/concurrency research direction.

---

## Summary

| Aspect | Current state | Future direction |
|--------|--------------|------------------|
| Environment | Hosted only (POSIX + libc) | Freestanding mode later |
| Targets | 64-bit, x86_64 + aarch64, POSIX | Cross-target CI, 32-bit, Windows |
| Startup | `main` → `user_main`, no init | No change needed |
| Shutdown | Return from `main`, OS cleanup | No change needed |
| Failure | No panic, no unwind, explicit errors, abort-on-OOM | Allocator traits |
| Allocation | libc malloc/realloc/free, abort-on-OOM | Allocator traits, arenas |
| Deallocation | Linear ownership + explicit `defer` | Automatic `Destroy` insertion possible |
| Stack | Unbounded, OS guard page | Bounded stack analysis for high-integrity |
| Capability tracking | `Alloc` tracks allocation, `Unsafe` tracks pointers | Authority budgets, sandboxing |
| Stdlib layers | Core/Alloc/Hosted classification documented | Compiler-enforced layer boundaries |
| Execution profiles | Not yet implemented; direction documented | `no_alloc`, `bounded_alloc`, `high_integrity` |
| Performance | LLVM `-O2`, no custom optimizations | Compile-time baselines, integration timing |
| FFI ownership | Linear types consumed by-value; raw pointers untracked | Verified FFI envelopes, ownership annotations |
| FFI calling convention | `#[repr(C)]` structs passed by value for extern fn | Already implemented |
| Boundedness | Capability reports show allocation/unsafe usage | Static allocation/stack analysis, `--report boundedness` |
| Concurrency | Not yet implemented; research documented | OS threads first, structured scopes, capability-gated concurrency, simulation-backed evidence later |
