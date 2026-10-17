# C, Rust and Zig Package Interop — Design Decision

Status: direction decided; implementation pending. Updated 2026-10-17 under
[R-0285](../../ROADMAP.md#task-r-0285), with ABI validation under
[R-0283](../../ROADMAP.md#task-r-0283)/[R-0287](../../ROADMAP.md#task-r-0287)
and exports under [R-0286](../../ROADMAP.md#task-r-0286).
The generator, package integration and proposed tooling below are **not built yet**.
[FFI.md](FFI.md) describes the direct foreign declarations implemented today.

## Product contract

**Applications declare a dependency, use normal Concrete imports, and run the normal
build/test commands. The tooling handles the foreign-language machinery.** This contract
applies equally to C, Rust and Zig libraries, within a documented supported interface.
Applications do not maintain foreign declarations, adapter code, generated files, build
scripts or compiler/linker flags for those bindings.

There are two authoring layers: an application and a reusable binding package. A binding
package can live beside an application initially and later be distributed independently.
Its reviewed boundary description is reused by every downstream application. Registry
discovery can follow local packages without changing application imports or calls.
Supported public interfaces must be bindable without editing or forking upstream sources.

The initial application manifest can use the existing local dependency shape:

```toml
[dependencies]
rocksdb = { path = "native/rocksdb" }
leanvm = { path = "native/leanvm" }
codec = { path = "native/codec" }
```

The intended application surface is ordinary Concrete code. These names illustrate APIs
to generate; they are not packages or APIs shipped today:

```con
import rocksdb.{Database, DbError};
import leanvm.{verify_signature};
import codec.{Decoder, DecodeError};
```

The application sees typed methods, linear handles, scoped buffers, `Result` values and
explicit capabilities. Normal imports also provide completion, documentation, source
navigation and diagnostics. Foreign language selection belongs in the binding package.

## Binding creation and reuse

For an available binding, resolve the selected package and its reviewed description
automatically. Do not search for or substitute an unpinned binding during a locked build.
For an uncovered library, the maintainer selects an upstream dependency and public API;
tooling inspects it and produces a binding draft. It derives established signatures and
layouts, identifies unsupported items and asks only for semantic facts it cannot establish.

The maintainer reviews those facts once. Regeneration uses that saved description;
applications and routine builds do not answer the same questions again. On an upgrade,
retain facts only for unchanged compatible interfaces and request review of affected facts.
Never carry a reviewed ownership rule silently onto a changed upstream declaration.
Builds remain noninteractive for local development and CI; missing facts produce a binding
maintainer diagnostic rather than a prompt in every consuming application.

A binding package contains the upstream source/version and feature selection, the selected
API and concrete type instantiations, reviewed boundary facts, optional narrowly scoped
custom adapter code, and conformance fixtures. Generated shims, Concrete modules, headers
and documentation are derived outputs. Editing generated output is never the repair path.
An explicit bridge interface remains an advanced authoring option for difficult APIs.
For C libraries without standard package metadata, record the supported upstream build
recipe once in the binding package, using existing build-system integration where possible.
Tooling must not guess arbitrary build instructions or make each application repeat them.

The description must express at least the following facts. Its file format and proposed
`concrete ffi`/`concrete bindgen` command names remain implementation decisions; acceptance
is defined by the workflow, not by introducing another required command.

| Declared or derived fact | Generated Concrete-facing meaning |
|---|---|
| Selected function/type and upstream source span | Ordinary named module item with original documentation and source navigation |
| Constructor and matching cleanup operation | An owned linear handle and an explicit consuming close/drop operation |
| Pointer plus length, call-scoped borrow, no retention | A checked scoped byte-buffer argument |
| Retained input or a view into another object | An explicit owner relationship supported by Concrete, or an unsupported-interface diagnostic |
| Allocator, output length and matching library deallocator | An owned foreign buffer with that library's cleanup operation |
| Nullability, status codes, output initialization and error ownership | Typed `Option`/`Result` values and cleanup on every error path |
| File/network authority, blocking and reentrancy | Required capabilities and visible execution constraints |
| Thread affinity, sharing and callback lifetime | Enforced handle restrictions or an adapter that keeps work on its owning runtime |
| Layout, calling convention, target and symbol names | ABI declarations and executable round-trip fixtures |
| Failure/abort behavior and trusted assumptions | Generated boundary documentation and build/audit metadata |

Every fact records its origin: upstream/compiler-derived, checked by a named control, or
reviewed as an assumption. Signature extraction, tests and a maintainer assertion are
different evidence. A name such as `free` cannot establish ownership; an AI-generated
draft cannot approve a safety assertion. Missing facts block only the affected safe API
and produce a precise maintainer diagnostic, not a silently unsafe substitute.

## One boundary representation, three front ends

Use one versioned internal boundary description for both sides of generation, ownership
and capability checks, documentation and test harnesses. C, Rust and Zig front ends feed
that description. This is a tooling representation, not a new proof object or receipt
schema; evidence identities and compatibility continue to use their existing owners.

| Source language | Automatic input processing | Generated native side |
|---|---|---|
| C | Parse selected headers with Clang using the actual target, defines, include paths and language flags | Checked declarations plus wrappers for supported macros, inline functions or inconvenient calling shapes |
| Rust | Inspect supported package/API metadata; select concrete functions and types | A typed C-compatible adapter compiled by Rust against the actual crate |
| Zig | Inspect the selected module with the pinned Zig compiler and supported comptime/type metadata; instantiate selected comptime arguments | A C-compatible exported adapter compiled by Zig against that module |

C is the initial shared ABI boundary, with a named target-specific supported subset.
Rust's native ABI and Zig's ordinary internal layouts are not compatibility contracts.
Keep internal objects opaque. Lower slices to pointer/length pairs and errors/options/
results to explicitly defined tags and payloads or status/out-parameter operations.
Do not pass a native Rust `Result`, Zig error union, allocator object or slice layout
directly to Concrete. Shared records use explicitly validated C-compatible layouts.

For Rust, the generated adapter must typecheck against the resolved crate, features and
concrete instantiations. Metadata extraction is versioned with its supported toolchain;
there is no assumption of stable reflection over arbitrary Rust APIs. Generics and trait
objects may stay inside Rust behind selected concrete operations and opaque handles.
An existing C-compatible Rust API can reuse the C-header path.

For Zig, reuse existing C-compatible exports when available. Otherwise generate exports
around supported public operations, with comptime choices fixed by the binding package.
Resolve its `build.zig`/`build.zig.zon` inputs and dependency graph with the pinned toolchain.
Keep allocation and deallocation in the same declared allocator domain. Arbitrary comptime
programs are not automatically stable APIs, and a declaration being public does not make
its ownership semantics known.

Begin with scalars, explicit pointer/length buffers, opaque handles and status/out-parameter
calls. Add aggregate forms only after R-0283/R-0287 validate them on each advertised target.
Complex C macros, varargs, native generic/lifetime shapes and retained references are
supported only through an explicitly tested adapter shape; otherwise diagnose them.
The initial target matrix must be named. This decision does not promise every native
library, platform or foreign ABI feature at launch.

## Ownership, capabilities and performance

Safe generated wrappers follow Concrete's existing rules. Raw pointer-sensitive operations
remain inside the audited adapter. A safe application call requires the capabilities its
operation declares; generation must not erase effects or mark arbitrary externs trusted.
Do not introduce implicit scope-end destruction or returned references through bindings.
Expose explicit consuming cleanup, usable with existing cleanup patterns; fallible cleanup
keeps a result and a defined ownership state after failure.

The wrapper must account for null/length consistency, integer-width conversions, output
initialization, alignment, aliasing, partial construction and cleanup on every supported
return path. A library-allocated buffer never becomes an ordinary Concrete-owned allocation
without an explicit allocator-compatible transfer. Callback registration owns or borrows
its context under a declared lifetime; unregister/shutdown establishes when callbacks stop.
Until the language can express a relationship safely, expose an owned operation, a reviewed
runtime adapter or a diagnostic rather than an escaping reference or an unrestricted handle.

Calls are native calls, with no RPC or serialization layer required. Support call-scoped
borrowing when its contract permits it. Document conversions, copies, allocations and
blocking operations; retain measurements for the acceptance packages. Do not copy every
buffer to conceal missing lifetime support, or promise zero-copy when the API requires
an ownership transfer or representation conversion.

Ordinary foreign errors map to `Result`; process termination does not. No foreign unwinding
may cross an unsupported boundary. Record and test Rust panic policy and Zig panic/trap/OOM
behavior. Catch only recoverable cases actually supported by the runtime and cleanup rules;
do not claim that `panic=abort`, a Zig trap, or an arbitrary C crash becomes a normal error.

## Complex runtimes

A networking package may own its Rust or Zig runtime and expose a Concrete `Network`
handle. Submit messages and receive typed events through bounded queues, with poll/wait,
cancellation and shutdown. The package handles runtime initialization and native callbacks.
Applications do not manage Cargo, Tokio startup or raw callback contexts.

Specify queue-full behavior, maximum retained bytes, backpressure, thread affinity,
reentrancy, completion versus cancellation, callback quiescence and shutdown ordering.
Keep buffers and handles alive until the declared completion point. This is a dependency
adapter with explicit contracts, not general Concrete async or a concurrency-safety proof.

## Build, cache and upgrade contract

A normal build performs these operations automatically for supported bindings:

1. Resolve pinned binding packages, upstream dependencies, toolchains and target settings.
2. Reuse reviewed boundary facts; derive and validate the selected interface.
3. Generate or reuse the native adapter, Concrete module, headers and documentation.
4. Invoke Cargo, the C build/compiler, or Zig's build/compiler integration as required.
5. Validate the supported ABI, compile Concrete callers and link the selected native artifacts.

Integrate native dependencies into one build graph, including library search order,
transitive libraries, runtime requirements and shared-library deployment where supported.
Pin the compiler, linker, sysroot/SDK identity, target CPU/features, build mode, selected
features/defines, source and transitive lock inputs, boundary description, generator version
and custom adapter code. Toolchain provisioning uses the repository's explicit development
environment or configured toolchain source, with an actionable diagnostic when unavailable.
Application authors do not manually reproduce native compiler commands.

Cache keys cover those inputs and the dependency/header closure, including generated inputs
and declared build-script/environment inputs. Deterministic generation gives identical
glue for identical inputs, without machine paths or timestamps. Do not cache a build as
reproducible when its inputs are unknown. A warm unchanged build should perform no binding
regeneration or foreign recompilation. After dependency resolution and toolchain provisioning,
a retained offline build must work from the populated cache without hidden downloads.

Generation and native builds publish artifacts atomically. An error cannot silently link a
previous successful artifact. Invalidate affected outputs on an upstream, header, feature,
reviewed contract, toolchain or target change. Retain build provenance and native artifact
identities in audit metadata; a trusted native dependency does not gain a functional proof
because its adapter was generated or its ABI tests passed.

Upgrades show the changed public API, boundary facts, effects and copying/allocation behavior.
Report errors at the Concrete import/call or maintained binding description, with the native
source location and build log available. Preserve supported/unsupported coverage in generated
docs. A breaking API, missing cleanup rule or unsupported target fails before linking the
new program. A semantic change can still require upstream review and functional tests even
when its signature is unchanged.

## Delivery and acceptance

R-0285 follows the current soundness/identity work. Implement one shared generation/build
path and exercise it on four bounded cases before closing the interop checkpoint:

| Acceptance package | Behavior to retain |
|---|---|
| RocksDB C API | Open/read/write/close, ordinary errors and library-owned buffer cleanup |
| A real ethlambda Rust crypto operation | Pinned upstream verification, typed input/result and malformed-input behavior |
| A minimal libp2p Rust adapter | Message exchange with a Rust peer, bounded queues, cancellation and orderly shutdown |
| A pinned upstream Zig library | A real compression or hashing operation, concrete comptime choices where needed, and an allocator-backed output/cleanup path |

Select and record the Zig upstream and revision before its gate is claimed complete. A
locally invented arithmetic shim is not its acceptance workload. These four cases exercise
the interface subset; wrapping every client dependency is not a prerequisite to starting
[R-0490](../../ROADMAP.md#task-r-0490).

A non-author starts from a clean application and completes dependency setup, ordinary
imports, build/run/test, warm and offline rebuilds, and a dependency upgrade. For all four
packages require **zero handwritten application FFI/adapter code, zero application build
scripts or manual compiler/linker flags, and zero edits to generated files**. Count any
maintained adapter code in its reusable binding package and the client port inventory.
Record setup steps, build/cache cost, copy/allocation cost and diagnostic quality.

Retain positive round trips and negative controls for ownership transfer, wrong deallocator,
use after consuming close, escaping/retained borrows, missing capabilities, malformed data,
unsupported ABI/target, native build failure, stale cache reuse and incompatible upgrades.
For runtimes include saturation, callback-after-close, cancellation and shutdown controls.
Check every advertised target with actual Concrete/native callers, not layout arithmetic
alone; use applicable sanitizers and foreign runtime checks. Register mutations for the
authority, ownership and invalidation rules the gates claim to enforce. A checked adapter
or passing tests do not prove arbitrary upstream behavior.

## Relationship to current language and prior art

Foreign declarations and R-0484's effect checks already exist. The remaining work is safe
wrapper generation, package/build integration and validation of the advertised ABI subset.
The supported surface must grow through the acceptance cases and actual port blockers.
Keep explicit native adapters available without making every application author write them.
See [ABI.md](ABI.md) for current layout limits and
[SAFETY.md](../language/SAFETY.md) for the trust-boundary model.

Primary references informing this direction, checked 2026-10-17:

- [Clang LibTooling](https://clang.llvm.org/docs/LibTooling.html) for tooling over real C declarations.
- [CXX's boundary concepts](https://cxx.rs/concepts.html) for a selected shared interface and opaque language-owned objects; its generator is not a Concrete implementation.
- [Rust ABI rules](https://doc.rust-lang.org/reference/items/external-blocks.html#abi) and [FFI unwinding rules](https://doc.rust-lang.org/nomicon/ffi.html#ffi-and-unwinding).
- [Zig C-library exports](https://ziglang.org/documentation/0.17.0/#Exporting-a-C-Library), [C-compatible structs](https://ziglang.org/documentation/0.17.0/#extern-struct), and the [Zig build system](https://ziglang.org/learn/build-system/). Implementation must pin the version it supports rather than follow a moving documentation branch.
