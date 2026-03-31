# Phase H Findings

Status: open findings ledger

This note records what real programs exposed during Phase H and classifies each issue before it turns into roadmap or language work.

The goal is to prevent three failure modes:

- treating every workaround as a language-design problem
- treating every ergonomics issue as syntax debt
- letting real-program findings disappear into commit history

## Classification

Each finding should be tagged as one or more of:

- `language`
- `stdlib/runtime`
- `tooling/workflow`
- `backend/performance`
- `formalization impact`

## First-Wave Programs

### Policy Engine

What it exposed:

- enum fields inside structs originally panicked layout
- standalone examples needed an always-available print path
- string-heavy output wanted a better append path than repeated `string_concat`

What closed:

- Bug 005: enum-in-struct layout
- Bug 007: standalone print builtins
- Bug 011: in-place string building

What remains:

- formatting/interpolation
- ~~qualified module access~~: fully closed — `mod::fn`, same-name collisions, and inline sibling `::` access all work

### MAL Interpreter

What it exposed:

- parser-heavy code needed substring extraction
- loop-carried string building needed mutation-oriented helpers
- standalone benchmarking wanted an always-available timing path
- interpreter runtimes want stronger collection/data-structure support
- deep recursion raises runtime/stack questions beyond pure language surface

What closed:

- Bug 010: substring extraction
- Bug 011: in-place string building
- Bug 012: standalone timing

What remains:

- runtime-oriented collection maturity
- final runtime argument surface design
- runtime/stack pressure clarity

### JSON Parser

What it proved:

- Concrete’s capability system makes authority boundaries legible at the signature level in a way that is immediately useful in real code
- the ownership model is real enough to shape parser structure, not just to decorate APIs
- the language can already carry a non-trivial recursive-descent parser with pools, modules, `Copy` structs, `Vec` generics, and recursive value construction

What felt strong:

- visible authority plus visible ownership discipline is Concrete’s clearest differentiator
- pure helpers are visibly pure, allocating functions visibly allocate, and effectful output visibly declares capabilities
- the builder-builtin approach is verbose but honest: no hidden allocations, no extra grammar, no disguised effects
- module structure, builtin interception, `Copy` structs, and generic `Vec` support were strong enough to carry a real parser

What felt awkward:

- explicit linear-ownership pressure still forces code reshaping patterns that do not yet feel idiomatic
- `drop_string` pressure remains a real signal: cleanup is honest, but repeated destruction can become easy to forget and mechanically noisy
- `&mut` string-building patterns are workable but repetitive
- the lack of destructuring or non-enum pattern-style binding makes some parser/test code more verbose than it needs to be
- repeated multi-pool argument plumbing becomes noisy without better helper/abstraction patterns

What it implies:

- the central Phase H question is no longer “can Concrete carry real programs?” but “do explicit patterns stabilize into disciplined idioms or sustained verbosity?”
- future fixes should prefer compression patterns over hidden magic:
  - helper APIs
  - cleanup idioms
  - stronger stdlib conventions
  - qualification and abstraction tools that preserve explicitness
- syntax growth should remain the last step, not the first response, unless repeated real-program evidence shows that library and workflow patterns are insufficient

What closed after scoped `defer` landed:

- the JSON parser now uses `defer` throughout and dropped roughly 40 lines of cleanup boilerplate
- the biggest win was `defer cleanup(...)` in `main`, which turned repeated `if err != 0 { cleanup(...); return err; }` blocks into plain early returns
- repeated keyword-matching and output cleanup paths also became smaller and less error-prone
- this is evidence that explicit cleanup can become materially less noisy without hiding ownership or destruction

### JSON Parser Benchmark

What it exposed:

- alloca inside loop bodies grows the stack every iteration (LLVM never frees until function return); with 24 String-sized allocas in `parse_value`, deep recursion hit stack overflow at ~130k iterations
- string literal assignment inside a loop generated invalid IR: `ensureValAsPtr` had no `.strConst` case, producing `store %struct.String @global` instead of materializing the string
- compiling without `-O2` made the benchmark 3.6x slower than necessary: per-character `string_char_at` calls dominate at -O0 but are fully inlined at -O2

What closed:

- Bug 013: alloca hoisting — all allocas now emitted in function entry block via `entryAllocas` field in EmitSSAState
- Bug 014: string literal in loop — `.strConst` case added to `ensureValAsPtr`, routes through `materializeStrConst`
- Bug 015: missing `-O2` — clang invocations now pass `-O2` for both regular and test compilation

What it proved:

- at -O2, Concrete's JSON parser parses 9.3MB in ~40ms, matching or slightly beating Python's `json.loads` (46ms) on the same file
- earlier measurements showing Concrete at 185ms total were benchmark-mode artifacts: 145ms parse at -O0 + 40ms byte-by-byte string construction
- the parser implementation is strong; the optimizer/backend story matters a lot for real workloads
- `string_reserve` pre-allocation dropped the load phase from 40ms to 3ms at -O2 (LLVM inlines the push loop into a near-memcpy)

What was added:

- `string_reserve(&mut String, cap)` builtin across all compiler passes (Intrinsic, BuiltinSigs, Check, Elab, EmitBuiltins)
- `Bytes.to_string()` zero-copy ownership transfer in std/src/bytes.con
- regression tests: alloca hoisting stress, string literal in loop, string_reserve

Benchmark results (9.3MB JSON, warm cache):

| | Load | Parse | Total |
|---|---|---|---|
| Concrete -O0 (old default) | 40ms | 145ms | 185ms |
| Concrete -O2 (new default) | 3ms | 40ms | 43ms |
| Python json.loads | — | 46ms | 46ms |

### Grep-Like Tool Benchmark

What it proved:

- the JSON result generalizes to a structurally different workload: streaming/text scanning rather than recursive descent over one preloaded buffer
- Concrete at `-O2` is competitive on line-oriented text processing and pattern matching, not only parser-style workloads
- output cost now shows up separately from match cost in a useful way: count-only is much cheaper than printing matched lines

What surfaced:

- runtime argument access (`argc` / `argv`) is now a real user-facing need for systems utilities, not just an implementation detail
- standalone/project friction still matters because richer stdlib/project surfaces are easier to justify once programs start behaving like real command-line tools

Benchmark results (13MB log-like file, 200k lines, pattern `error`):

| Tool | Count-only | With output |
|---|---|---|
| Concrete `cgrep` | 35ms | 95ms |
| Python | 34ms | — |
| macOS `grep` | 83ms | 88ms |

What it implies:

- Concrete's `-O2` performance is now credible across at least two recognizable text workloads
- the next useful pressure point should be runtime/control-flow heavy code again (bytecode VM) or a flagship critical-software workload (artifact/update verifier), not another parser-only benchmark

### Bytecode VM Benchmark

What it proved:

- Concrete is now clearly in a real systems-language band on runtime-heavy code, not only on parser/text workloads
- the VM exposed a backend/codegen policy issue that real parser/text workloads did not: tiny vec builtins were not being inlined in the dispatch loop
- the current collection surface is good enough to build a real VM, and the remaining performance question turned out to be optimizer shaping rather than an inherent collection-cost wall

Benchmark results (`fib(35)` workload):

| VM | Time |
|---|---|
| Concrete `-O2` before vec inlining fix | ~785ms |
| Concrete `-O2` after vec inlining fix | ~257ms |
| C `-O2` heap-`Vec` version | ~260ms |
| Python | 15,223ms |

What it exposed:

- the dominant initial gap to C was not bounds checks or a fundamental safety tax; it was function-call overhead from non-inlined `vec_get`, `vec_set`, `vec_push`, `vec_pop`, and `vec_len` in the hot dispatch loop
- once vec builtins were marked `alwaysinline`, the gap to the comparable C heap-`Vec` implementation disappeared on this benchmark

What it implies:

- Concrete is still about 59x faster than Python here, so the language/runtime path is clearly credible
- this benchmark no longer supports the claim that Concrete currently pays a large unavoidable abstraction cost in safe collection code
- future performance work should focus first on backend inlining policy and other compiler/codegen cliffs before assuming the surface model itself is too expensive

### Artifact Verifier (conhash)

What it proved:

- Concrete's capability system is not just a correctness feature — it is the audit trail itself
- an auditor can verify from signatures alone that the hasher (`with(Alloc)`) never touches the filesystem or network, the file reader (`with(File, Alloc)`) cannot leak data to the console, and the reporter (`with(Console)`) cannot read or modify files
- SHA-256 implemented in pure Concrete (bitwise ops on `u32`) matches `shasum` and Python `hashlib` at ~23ms on 9.3MB — LLVM -O2 optimizes Concrete's bit manipulation to near-C quality
- this is the first Phase H program where the capability annotations are the *point*, not a side benefit

What it exposed:

- the `trusted` boundary is clean: only `read_file_raw` and `sha256` (which needs raw pointer arithmetic for padding/block processing) require `trusted`, everything else is safe Concrete
- linearity friction with conditional initialization (`if argc >= 3 { ... }`) required a helper function workaround — evidence for the destructuring/conditional-init open finding

Benchmark results (9.3MB file):

| Tool | SHA-256 time |
|---|---|
| Concrete `conhash` | ~23ms |
| `shasum` (Perl/C) | ~25ms |
| Python `hashlib` (C openssl) | ~20ms |

### C Comparison Benchmarks

All five example programs were benchmarked head-to-head against equivalent C implementations compiled with `cc -O2`. Concrete programs compiled through the standard pipeline (LLVM IR + `clang -O2`).

| Example | Workload | C avg | Concrete avg | Ratio |
|---|---|---|---|---|
| verify (conhash) | SHA-256, 10MB file | 98ms | 101ms | ~1.0x |
| vm | fib(35) bytecode | 303ms | 274ms | ~0.9x |
| policy_engine | RBAC eval, 27 cases | 64ms | 70ms | ~1.1x |
| json | recursive-descent, 27 cases | 64ms | 78ms | ~1.2x |
| grep (plain search) | 100k lines, pattern match | 12ms | 23ms | ~1.9x |
| grep (case-insensitive) | 100k lines, -i -n | 23ms | 40ms | ~1.7x |
| grep (count only) | 100k lines, -c | 11ms | 18ms | ~1.6x |

What this shows:

- verify, vm, policy_engine, json are at rough parity with C (0.9x–1.2x) — this is the expected result for a language that compiles to LLVM IR with `-O2`
- vm at 0.9x is interesting but should be presented as parity, not a consistent advantage, until repeatedly confirmed across more workloads
- grep is the outlier at 1.6x–2.8x — the string/output path (`println`, per-line string operations) is the bottleneck, giving a concrete next optimization target
- the case-insensitive gap (2.8x) is larger than plain search (1.6x), suggesting per-character string processing cost is the dominant factor

What it implies:

- Concrete's `-O2` backend produces code in the same performance class as hand-written C for compute-heavy and collection-heavy workloads
- the remaining gap is concentrated in string I/O paths, not in the core language model or ownership overhead
- after switching the compiler's print builtins from raw `write()` syscalls to buffered libc I/O (`printf`/`putchar`), the case-insensitive gap dropped from 2.8x to 1.7x; lower-level stdlib I/O helpers in `std.io` still expose direct/unbuffered `libc_write`-based behavior where appropriate; remaining gap is per-character `to_lower` cost rather than I/O overhead

## Second-Wave Evidence

The second-wave workload set is now no longer only a roadmap intention. Initial implementations exist for all four current second-wave targets and they already exposed useful pressure:

### TOML Parser

What it proved:

- Concrete can carry a richer structured-parser workload than JSON while staying in the same general implementation style
- external conformance-oriented parser work is feasible without inventing a radically different architecture from the JSON parser

Current state:

- compiles and runs
- roughly 930 lines
- supports key-value pairs, strings, integers, booleans, arrays, tables, inline tables, comments, and line-aware error reporting
- uses a string-pool architecture similar to the JSON parser to avoid ownership/double-free pitfalls

### File Integrity Monitor

What it proved:

- filesystem-walking and manifest-oriented integrity tooling is a strong continuation of the verifier story
- capability separation still reads well in a scanner/reporter style tool

What it exposed:

- Bug 016: cross-module generic monomorphization/linking is still broken for real package builds of `HashMap<String, String>`

Current state:

- compiles and runs
- all four outcomes work: `OK`, `MODIFIED`, `ADDED`, `REMOVED`
- used direct manifest parsing as a workaround because `HashMap<String, String>` did not link correctly in package builds

### Key-Value Store

What it proved:

- persistence and replay-oriented workloads are within reach for Concrete's current stdlib/runtime model
- append-only log and compaction patterns can be expressed cleanly enough to be a meaningful storage-pressure workload

What it exposed:

- Bug 016: the same cross-module generic monomorphization/linking failure for `HashMap<String, String>` appears independently here, which makes it strong bug evidence rather than a one-off example quirk

Current state:

- compiles and runs
- append-only log with replay and `SET` / `GET` / `DELETE` / `LIST` / `COMPACT`
- used parallel `Vec`s as a workaround for the current `HashMap<String, String>` package-build failure

### Simple HTTP Server

What it proved:

- a blocking single-threaded networked/service-style component is viable enough to compile through the current language and stdlib surfaces

What it exposed:

- Bug 017: `std.net` currently hardcodes Linux socket constants for `setsockopt`, which breaks macOS (`SOL_SOCKET` and `SO_REUSEADDR` values differ)

Current state:

- server code compiles
- macOS bind/reuse behavior is blocked by the stdlib socket-constant bug rather than by the server structure itself

### What the second wave already changed

- the second-wave list is now evidence-backed rather than purely aspirational
- `TOML parser` looks like a clean exemplar candidate
- `file integrity monitor` and `key-value store` found the HashMap cross-module linking bug (now fixed); both now use direct `HashMap<String, String>` storage rather than the earlier workaround shapes, though `kvstore` still intentionally keeps a parallel key `Vec<String>` for enumeration and swap-remove semantics
- `simple HTTP server` found the macOS socket constants bug (now fixed) and later exercised the stack-array borrow path that exposed Bug 018; it now builds/runs without the earlier heap-buffer workaround
- `Lox interpreter` (1,052 loc) compiles/runs cleanly — tree-walk eval with closures, scoping, control flow
- the most important new findings from this wave were:
  - Bug 016: cross-module generic monomorphization/linking for `HashMap<String, String>` in package builds — **fixed** in `bdb2d7f` (linker alias resolution in Mono.lean lookupFn + EmitSSA svalToOperand + capability mismatch in HashMap/HashSet constructors)
  - Bug 017: Linux-only socket constants in `std.net` — **fixed** in `bdb2d7f` (runtime uname() platform detection, platform-aware sockaddr_in filling)
  - Bug 018: borrowing a stack array for writable FFI access can create a copy instead of a stable reference — **fixed**. Root cause: Lower.lean's borrow/borrowMut handlers emitted `.cast` for arrays (which are always stack-allocated), and EmitSSA's cast handler saw `[N x T]` → `ptr` and took the "non-ptr → ptr for pass-by-ptr" path, creating a new alloca+store. Fix: arrays no longer emit `.cast` in borrow handlers; the existing register is retyped directly since the alloca result is already a pointer.
  - Bug 019: method-level generics crash at lowering — **fixed** in `c0c5b54`. Two issues: (a) Parser created bare Self type (`Box`) instead of `Box<T>` for self parameters in generic impls; (b) generic structs like `Box<T>` were only instantiated once at the LLVM level — `Box<i32>` and `Box<i64>` shared the same layout, causing silent value truncation. This blocked any method with its own type parameter (`fn fold<A>`, `fn map<U>`). Fix: `selfTy` now uses `Ty.generic` with type params, and post-monomorphization pass creates distinct LLVM struct types for each instantiation.
- remaining cleanup:
  - `TOML parser` can be treated as a likely exemplar once its tree is cleaned up
  - `file integrity monitor` now uses direct `HashMap<String, String>` / `HashSet<String>` structures
  - `key-value store` now uses direct `HashMap<String, String>` storage, while intentionally retaining a parallel key `Vec<String>` for explicit enumeration order and swap-remove behavior
  - `simple HTTP server` no longer needs the Bug 018 heap-buffer workaround after the borrow fix; remaining work is ordinary example polish rather than a compiler/std blocker
  - Bug 019 (method-level generics) is the most impactful compiler fix from second-wave work: it unblocked `fold<A>` on all containers and generic method patterns generally

## Phase H Retrospective

### What Phase H proved

Concrete is credible on real programs. Policy engine, MAL interpreter, JSON parser, grep, bytecode VM, and artifact verifier all worked. This is no longer a toy-language question.

`-O2` changes the performance story completely. JSON and grep show Concrete competitive with Python on real text workloads. Earlier "Concrete is much slower" conclusions were mostly `-O0` artifacts.

Real workloads found real compiler bugs: enum-in-struct layout panic, cross-module string literal collision, alloca-in-loops stack blowup, string literal in loop invalid IR, const lowering gap, if-expression gap, standalone print/timing gaps, substring/loop string-building gaps. These were valuable because they came from programs, not synthetic cases.

`defer` was high leverage. It removed significant cleanup boilerplate, now has credible scope semantics, and explicit cleanup still works but with much less noise.

Concrete's differentiator is real. Visible authority in signatures, visible ownership/cleanup in code — now proven under real code, not just in docs. The artifact verifier is the clearest demonstration: capability signatures *are* the security audit.

### What Phase H fixed

Phase H closed a meaningful set of issues that were exposed only by sustained real-program work:

- standalone/project workflow is credible for current use: package mode, builtin std resolution, examples moved onto stdlib surfaces
- text/output is practical: mixed-arg `print` / `println` landed, buffered compiler print builtins removed the worst output-path overhead, and parser/grep-style programs no longer need awkward print scaffolding
- runtime argument access has a stable current surface via `std.args`
- qualified module access is complete for the practical cases that real programs needed, including same-name collisions and inline sibling `::` access
- a first round of runtime-oriented collection support landed through HashMap iteration helpers
- major backend shaping cliffs were identified by real workloads and fixed where the evidence was strongest, especially the VM inlining cliff

### What the main open question became

Not "can Concrete express real programs?" but "do its explicit patterns become stable idioms or stay as exhausting ceremony?"

### Priority fixes from Phase H evidence

1. **~~Standalone vs project dependency resolution~~** — CLOSED: `concrete build` now works with `Concrete.toml`, `mod X;` directory modules, and cross-module imports; `cgrep` and `conhash` examples converted to use `std.fs.read_to_string` / `std.fs.read_file`; current `std = { path = "..." }` is a temporary hack — Phase J should make std a builtin dependency
   - update: builtin std resolution is now landed as well; std is found automatically relative to the compiler binary, with `CONCRETE_STD` as an override for unusual setups
2. **Formatting / interpolation / text output** — mixed-arg `print` / `println` landed (commit `1b0d21f`); remaining question is whether interpolation is still needed
3. **Runtime-oriented collection maturity** — HashMap iteration (`for_each`, `keys`, `values`) landed; remaining gap is nested mutable structures and frame-friendly patterns for interpreters/VMs
4. **Backend inlining / codegen policy cliffs** — the VM showed that tiny builtin calls in hot loops can distort performance dramatically if LLVM is not given enough shape information
5. **~~User-facing runtime argument surface~~** — CLOSED. `std.args` module provides `count()` and `get(idx)` API; examples converted; test-mode stubs handle the symbol gap
6. **~~Qualified module access~~** — CLOSED. File-based `mod::fn` access, mixed imported + qualified access, two-submodule qualified access, top-level + qualified coexistence, qualified submodule `extern fn`, qualified submodule struct/import, same-name collision handling, and inline sibling `::` access all work.
   - same-name collisions: solved via Elab-time `prefixModuleFnNames` which renames submodule definitions consistently across all passes, with ambiguity detection (two submodules defining `add` requires qualified `math::add` / `util::add`)
   - inline sibling modules: `mod A {}` / `mod B {}` can now use `A::fn()` from `B` without explicit imports; sibling function sigs are injected across Resolve, Check, and Elab passes with linker aliases
7. **Runtime / stack pressure classification** — MAL exposed this; still needs a cleaner language-vs-runtime-vs-tooling decision

## Phase H Close-Out Judgment

Phase H is effectively past the discovery stage. The project no longer needs this phase to prove that Concrete can handle real programs or that its backend can reach C-class performance on serious workloads.

What remains is narrower:

- a small set of runtime/stdlib/ergonomics questions that still need classification rather than broad new language growth
- continued benchmark interpretation where new workloads expose a concentrated bottleneck
- a final judgment about which remaining pain points belong to Phase H cleanup versus later package/runtime/formalization work

The center of gravity should now move toward Phase J:

- the biggest remaining structural gap is package/workspace/incremental maturity, not proof-of-life examples
- remaining Phase H issues are mostly library/runtime/tooling follow-through, not existential validation of the language
- Phase I and Phase J now have better inputs because the language has already been pressured by real programs instead of only by pass tests and medium examples

Phase H should therefore be treated as:

- proven in its core purpose
- still carrying a short cleanup tail
- no longer the main strategic center of the roadmap

## Remaining Open Findings

### Formatting / interpolation

- Class: `stdlib/runtime`, possibly `language`
- Why it matters: real programs need readable output, logs, diagnostics, and message assembly
- Current state: mixed-arg `print` / `println` builtins landed (commit `1b0d21f`); manual string *building* is still verbose but output is now practical. Six Phase H programs (policy engine, MAL, JSON parser, grep, bytecode VM, artifact verifier) have all been written without string interpolation. `print`/`println` plus builder APIs have been sufficient for all real-program output needs so far. Interpolation remains available as a future option if sustained evidence from larger programs justifies it, but current evidence does not require it.

### Destructuring let

- Class: `language`
- Why it matters: parser/runtime code wants clearer binding of paired results
- Current state: still an open surface question, not a confirmed must-add

### Runtime-oriented collections

- Class: `stdlib/runtime`
- Why it matters: interpreters, analyzers, and schedulers want maps, nested mutable structures, and clearer frame-friendly patterns
- Current state: full traversal surface landed — `for_each`, `fold<A>`, `keys()`/`values()`/`elements()` on Vec, HashMap, and HashSet. Bug 019 fix (`c0c5b54`) unblocked method-level generics which was the prerequisite for `fold<A>`. Remaining gap is nested mutable structures and frame-friendly patterns. Design decision: no iterator tower, no closures, no cursor/lifetime model — explicit per-container traversal only.

### ~~Stack array borrow-copy (Bug 018)~~ — FIXED

- Class: `backend/performance`, `language`
- Why it matters: `&buf` on a stack `[u8; 4096]` created a fresh copy each time. FFI writes into copy #1, subsequent reads saw copy #2 (all zeros). This was systemic — affected any code that passed `&array` to FFI for mutation then read it back.
- Resolution: In Lower.lean's borrow/borrowMut handlers, arrays (always stack-allocated) no longer emit a `.cast` instruction. The existing register is retyped directly since the alloca result is already a pointer. This avoids EmitSSA's cast handler creating a redundant alloca+store (which produced invalid LLVM IR with a type mismatch). HTTP server heap-buffer workarounds can now be replaced with direct stack arrays.

### Standalone vs project UX

- Class: `tooling/workflow`
- Why it matters: examples and benchmarks should not need awkward scaffolding to reach common stdlib utilities
- Current state: **closed for current workflow purposes** — `concrete build`, `concrete run`, and `concrete test` now work in package mode; std is resolved automatically relative to the compiler binary, with `CONCRETE_STD` as an override; remaining work is broader package/workspace maturity, not basic std access

### ~~Runtime argument surface~~ — CLOSED

- Class: `stdlib/runtime`, `tooling/workflow`
- Resolution: `std.args` module landed with `count() -> Int` and `get(idx: Int) -> String` API. `cgrep` and `conhash` examples converted to use it. Test-mode stubs provide `__concrete_get_argc` (returns 0) and `__concrete_get_argv` (returns null) so stdlib compilation succeeds without the main wrapper.

### Backend inlining / codegen policy cliffs

- Class: `backend/performance`
- Why it matters: the bytecode VM initially showed a large gap to C that disappeared once vec builtins were marked `alwaysinline`; this is strong evidence that backend shaping can dominate perceived language cost
- Current state: the first major VM performance cliff was fixed; future performance analysis should check for similar missed-inlining or missed-shaping cases before proposing new unsafe or unchecked surfaces

### Runtime / stack pressure

- Class: `backend/performance`, `stdlib/runtime`
- Why it matters: deep-recursive workloads expose execution-model limits that should be understood before later runtime/concurrency work
- Current state: observed in MAL benchmarks, still not classified into final ownership

## Rule

Before any new surface change is adopted from a Phase H finding:

1. classify the issue
2. decide whether it belongs in language, stdlib, tooling, or runtime
3. write the narrowest design that solves the real problem
4. record why library/workflow fixes are insufficient if syntax is being proposed

## Standing Phase H Question

For every serious program, ask:

- are explicit authority and ownership patterns becoming stable idioms?
- or are they remaining honest but exhausting ceremony?

That question is now one of the most important evaluation criteria for the phase.
