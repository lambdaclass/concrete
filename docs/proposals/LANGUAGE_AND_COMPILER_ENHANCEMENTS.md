**PROPOSED (2026-11-22); awaiting Codex and second-reviewer review and owner decision. Schedules roadmap scope and design proposals only; decides no language rule.**

# Language and Compiler Enhancements Proposal (2026-11-22)

**Scope.** This document collects high-leverage language, specification, compiler and tooling enhancements designed specifically around Concrete's existing architectural invariants—strict linearity, second-class references, dual-key authority (`with(...)` plus linear resource grants), executable Lean Core semantics, one `Layout` authority, proved ghost erasure, and mandatory termination variants—and maps each item to its exact home in [ROADMAP.md](../../ROADMAP.md).

Every item is evaluated against [docs/PHILOSOPHY.md](../PHILOSOPHY.md#the-rules) and the [admission test](../PHILOSOPHY.md#admission-test). Nothing in this document decides any open language question or reopens any decided rule on its own: every language proposal (`R-0713`–`R-0716`, `R-0718`, plus the early-audit options in the [appendix](#appendix-early-audit-options)) requires an explicit owner decision recorded in [docs/decisions.md](../decisions.md) before any syntax or semantic rule enters `ConcreteSpec`.

## Roadmap mapping at a glance

To keep [every fact once](../PHILOSOPHY.md#the-rules) (rule 5), ideas that refine or pull forward existing roadmap obligations extend their current rows; genuinely new capabilities receive proposed rows `R-0711`–`R-0718` (subject to rebase if other IDs land first).

### New roadmap tasks (`R-0711`–`R-0718`)

| Task | Phase | Kind | Summary | Owner-gated? |
|---|---|---|---|---|
| [R-0711](../../ROADMAP.md#r-0711) | C | Compiler | [Verified optimizer hints from `CheckedCore` and `Facts`](#comp-verified-hints) | No |
| [R-0712](../../ROADMAP.md#r-0712) | C | Spec / checker | [Deterministic bitmask, modulo and shift transfer rules](#comp-bitmask-rules) | No |
| [R-0713](../../ROADMAP.md#r-0713) | C | Language | [`&out T` write-only second-class references](#lang-out-ref) | **Yes** |
| [R-0714](../../ROADMAP.md#r-0714) | C | Language / layout | [Zero-`unsafe` binary wire formats (`ByteSafe` + explicit-endian scalars)](#lang-wire-formats) | **Yes** |
| [R-0715](../../ROADMAP.md#r-0715) | C | Language / std | [Zero-sized erased proof witnesses (`Witness<P>`) and linear typestate](#lang-typestate-witness) | **Yes** |
| [R-0716](../../ROADMAP.md#r-0716) | C | Language / const | [Const-generic capacity parameters (`<const N: u64>`) + pure compile-time data eval](#lang-const-generics-eval) | **Yes** |
| [R-0717](../../ROADMAP.md#r-0717) | D | Tooling | [Deterministic Simulation Testing (`concrete sim`) over `main`'s grants](#lang-dst) | No |
| [R-0718](../../ROADMAP.md#r-0718) | D | Language / alloc | [Scoped scratch sub-arenas, argument-inferred brands and O(1) arena reset](#lang-scratch-brands) | **Yes** |

### Existing roadmap tasks extended or pulled forward

| Existing task(s) | Phase | Enhancement |
|---|---|---|
| Step bounds + `R-0574` | A / B | [Automatic worst-case complexity / gas / WCET bounds from `#[variant]`](#lang-step-bounds) |
| `R-0583`, `R-0299` | B / E | [Certified SSA dominator & register-allocation validators + proof-producing e-graphs](#comp-validators-egraphs) |
| `R-0593`, `R-0599`, `R-0177` | B / E | [Proof-guided structured LLM repair diagnostics and `Obligation_*.lean` stubs](#comp-repair-trace) |
| `R-0586` | B | [`concrete trace` Miri-grade reference debugger over hybrid Core state](#comp-repair-trace) |
| `R-0556`, `R-0149` | C / G | [Two-level declaration Merkle hashing (`interface_hash` vs `body_hash`)](#comp-merkle-incremental) |
| `R-0495`, `R-0177` | E (pulled to Slice 8) | [`concrete extract-lean` pure-functional model + auto-proved refinement](#comp-extract-lean) |
| `R-0517`, `R-0505` | E | [Direct LCB machine code + proved Mach-O/ELF binary validator with in-Lean stack bounds](#comp-binary-validator) |
| `R-0201` | F (diff pulled earlier) | [`concrete diff` semantic assurance diffing and `Concrete.toml` ratchets](#comp-assurance-diff) |

---

## Part 1: Language and Systems-Model Enhancements

### <a id="lang-out-ref"></a>1. `&out T` write-only second-class references (`R-0713`)

**Motivation.** Under current rules every local binding must be initialized before use and array repeats require `Copy` (for example `[0u8; 65536]`). Passing stack storage into a callee such as `read(fd, buf)` or placement-initializing a large struct therefore either pays an unnecessary `memset` or forces `unsafe` raw pointers. Rust's `MaybeUninit<T>` shifts the initialization obligation into `unsafe` code; SPARK's `Relaxed_Initialization` requires whole-flow analysis.

**Proposed design.** Exploit Concrete's existing strict linearity and second-class references to add the linear dual of `&mut T`:

- A normal linear binding `x: T` starts initialized and must be consumed exactly once.
- An `&out T` parameter (analogous to Hylo's `set` parameters) starts **uninitialized** and must be written/initialized **exactly once** on every normal return path before the call returns.
- Because references are strictly second-class, `&out T` cannot escape, be stored in structs, or be returned; checking stays purely local and lexical with zero lifetime parameters.
- On returning paths, a caller passing an uninitialized local slot transitions that local from uninitialized to initialized with guaranteed zero-copy placement construction (guaranteed RVO).
- On error paths, the callee must leave the slot in a single statically determined state (either uninitialized, or returned through a linear outcome token such as `Init` / `Uninit`), never ambiguously half-initialized.

**Philosophy alignment.** Passes [admission test](../PHILOSOPHY.md#admission-test): one form for uninitialized output placement (AT 1), explicit in every signature and call site (AT 2, AT 4), local lexical invariant without lifetimes (AT 4, AT 5), eliminates hidden zero-init overhead and `unsafe` placement casts (AT 7, AT 14).

**Roadmap home.** [R-0713](../../ROADMAP.md#r-0713) (Phase C, after Slice 13 borrows).

**Owner questions required.**
1. Syntax spelling (`&out T`, `out &mut T`, or another form) and call-site marker.
2. Whether uninitialized local declarations (`let x: T;` filled by one `&out` call before any read) are permitted, or only placement expressions returning `T`.
3. Error-path contract when an `&out T` callee returns `Result<_, E>` (require uninitialized on `Err`, or support partial-init tokens for buffer reads).

---

### <a id="lang-wire-formats"></a>2. Zero-`unsafe` binary wire formats: `ByteSafe` and explicit-endian scalars (`R-0714`)

**Motivation.** Across the adopted [showcases](EXAMPLES.md#showcases)—PNG, QOI, EVM, WebAssembly, RocksDB SSTables, ChaCha20/Noise, HTTP/TCP and ethlambda (SSZ/RLP)—a large share of systems code decodes and encodes unaligned, endian-specific binary formats. In C and Rust this is where code drops to `#[repr(packed)]` and `unsafe` casts; in Concrete it also sits directly on the unresolved conflict between [OD111](../decisions.md#od111)/[OD147](../decisions.md#od147) and [packed removal until foreign-ABI demand](../decisions.md#recovered-59-23).

**Proposed design.** Leverage Concrete's single Lean-proved `Layout` and `Rep` authority ([C3](FOUNDATION.md#c3-target-layout-and-representation), `R-0609`, `R-0502`):

- Introduce 1-byte-aligned explicit-endian scalar types (`le_u16`, `be_u16`, `le_u32`, `be_u32`, `le_u64`, `be_u64`, and signed counterparts) with explicit, zero-hidden-conversion methods to/from host integers (`to_host`, `from_host`).
- Introduce a compiler-checked structural property `ByteSafe` (checked by `Layout`, like `Copy`—zero macros or derive machinery): a type is `ByteSafe` iff every bit pattern is valid, every field is `ByteSafe`, and `Layout` proves zero padding bytes.
- Provide safe, zero-`unsafe` byte-slice / `[u8; N]` decode and encode primitives for any `ByteSafe` type: zero alignment UB, zero uninitialized padding leakage, zero host-endianness bugs, and trivial Lean `rep_roundtrip` proofs.
- Because explicit-endian scalars already have alignment 1 and `ByteSafe` rejects padding, safe wire-format structs need no `#[repr(packed)]`, cleanly separating safe wire formats from foreign C-ABI packed structs.

**Philosophy alignment.** Directly satisfies rule 1 (removes an entire class of alignment/endian/padding defects), rule 5 (single `Layout` authority), rule 15 (no macros/derives), and [AT 14](../PHILOSOPHY.md#admission-test) (audited codecs without `unsafe`).

**Roadmap home.** [R-0714](../../ROADMAP.md#r-0714) (Phase C, Slice 15/25 boundary, coordinated with `R-0609` and `R-0071`).

**Owner questions required.**
1. Exact scalar set and naming (`le_u16`..`be_u64`, signed variants, `u128`/`i128`).
2. Whether `ByteSafe` is a compiler-checked structural marker/trait and whether opt-in declaration is required on structs so adding a padding field is caught at the struct declaration site.
3. Reconciliation with [OD111](../decisions.md#od111), [OD147](../decisions.md#od147) and [recovered-59-23](../decisions.md#recovered-59-23) (keeping `#[repr(C, packed)]` strictly FFI-only or retaining it).

---

### <a id="lang-typestate-witness"></a>3. Linear typestate and zero-sized erased proof witnesses (`Witness<P>`) (`R-0715`)

**Motivation.** Rust is affine: callers can silently drop an uncommitted transaction, an unfinished handshake, or a half-flushed writer. Concrete is strictly linear AND already models zero-sized types with full linear obligations (`Q3.8`), and proves ghost erasure ([C9](FOUNDATION.md#c9-identities-primitive-contracts-evidence-and-ghost-erasure), `R-0615`). Together these give Concrete strictly stronger compile-time protocol enforcement and zero-cost validation chaining than affine languages.

**Proposed design.** Standardize and support two idiomatic patterns directly on top of existing Core linearity and C9 ghost erasure:

1. **Must-finish linear typestate protocols in std:** APIs such as `Db::begin(&mut db) -> Tx<Active>` whose only consumers are `commit` or `rollback` (or typed failure recovery). Forgetting to finish a protocol or double-finishing is a compile-time error at 0 bytes of runtime overhead.
2. **Zero-sized erased proof witnesses (`Witness<P>`):** Validation functions return a 0-byte witness token whose logical meaning is linked to a predicate/fact (for example `validate_utf8(&bytes) -> Option<Witness<ValidUtf8>>` or `check_sorted(&arr) -> Option<Witness<Sorted>>`), erased completely by C9 ghost erasure into 0 runtime bytes. Downstream functions accept the witness instead of re-checking at runtime, storing redundant boolean flags, or repeating identical `#[requires]` clauses across long call chains.

**Philosophy alignment.** Zero new runtime machinery (rule 8), strict linearity already present (rule 9), proved erasure (rule 12 / C9), makes protocol completion and validation provenance visible in signatures (rule 3, AT 10).

**Roadmap home.** [R-0715](../../ROADMAP.md#r-0715) (Phase C, Slice 8 / 18, coordinated with `R-0615`, `R-0694`, `R-0071`).

**Owner questions required.**
1. Whether `Witness<P>` is a dedicated ghost/proof type constructor in `ConcreteSpec` or a std pattern over zero-sized private-constructor structs with `#[invariant(...)]` (Slice 17).
2. Interaction between a witness token and mutation of the referenced data (ensuring lexical borrow-freezing or versioned/linear binding prevents stale witnesses after mutation).

---

### <a id="lang-const-generics-eval"></a>4. Const-generic capacity parameters (`<const N: u64>`) and pure compile-time data evaluation (`R-0716`)

**Motivation.** Concrete currently has two major friction points for zero-heap and predictable-policy systems code (Slices 0–18, embedded/realtime, crypto/codec showcases):

1. **No `<const N: u64>` generics:** Without const-generic capacity parameters, std cannot provide generic fixed-capacity containers (`FixedVec<T, N>`, `RingBuf<T, N>`, `FixedStr<N>`) or generic array algorithms (`fn crc32<const N: u64>(buf: &[u8; N]) -> u32`). Note that [IDEAS.md](IDEAS.md) already flagged fixed-capacity non-allocating std (`#8`) as `RETAINED-ONLY`.
2. **Separate `CExpr` mini-language for constants:** Today `ConcreteSpec/Spec/Const.lean` maintains a tiny separate expression language restricted to scalar integer/boolean expressions ([constants decision](../decisions.md#decided-2026-11-08-constants)). Generating verified static lookup tables (CRC32, AES S-boxes, ChaCha constants, UTF-8/HTTP DFA tables, EVM opcode tables) therefore requires either hand-pasted magic hex arrays or external build generators.

**Proposed design.**
- **Value-level `<const N: u64>` parameters:** Strictly monomorphized at Mono Core alongside type parameters—no dependent types, no type-level arithmetic solver beyond constant folding of closed `u64` expressions, checked by `R-0510` checked specialization. Unlocks the entire zero-heap predictable stdlib.
- **Unified pure compile-time data evaluation (constants-only):** Replace the duplicate `CExpr` evaluator by allowing `const` initializers to call any **pure** (`with()` empty), **zero-heap**, **proved-terminating** Concrete function, evaluated deterministically by the exact same fuel-bounded Core evaluator (`R-0614`, C8) with a bounded fuel/size limit (`R-0528`).
  - Zero macros, zero source generation, zero AST manipulation, zero semantic drift between compile-time and run-time evaluation.
  - Compile-time lookup tables become ordinary, auditable, Lean-provable Concrete functions.

**Philosophy alignment.** Eliminates duplicate constant-expression semantics (rule 5), keeps macros permanently refused ([What Concrete refuses](../PHILOSOPHY.md#what-concrete-refuses)), and stays strictly within the narrow "constants-only" reopening rule for general compile-time evaluation in [docs/PHILOSOPHY.md](../PHILOSOPHY.md#what-concrete-refuses) (row `P17b`).

**Roadmap home.** [R-0716](../../ROADMAP.md#r-0716) (Phase C, Slices 11–15 / 17, coordinated with `R-0510`, `R-0574`, `R-0614`).

**Owner questions required.**
1. Reopening of narrow constants-only compile-time function evaluation under [PHILOSOPHY.md](../PHILOSOPHY.md#what-concrete-refuses) and [decided-2026-11-08-constants](../decisions.md#decided-2026-11-08-constants) (including fuel/size caps and determinism requirements).
2. Syntax and admitted type set for const parameters (`u64` only initially vs all integer types) and whether const expressions in types are restricted to bare parameters and integer literals.

---

### <a id="lang-scratch-brands"></a>5. Scoped scratch sub-arenas, argument-inferred brands and O(1) bulk arena reset (`R-0718`)

**Motivation.** Concrete's branded allocator model (`Q1.1`–`Q1.13`, [allocation decision](../decisions.md#decided-2026-11-02-allocation)) gives memory-safe arena lifetimes without a borrow-checker lifetime language, but three friction points show up immediately in parsers, HTTP servers, compilers, and packet processors:

1. Reclaiming temporary per-iteration allocations via consuming `reset(a)` requires existential `exists`/`pack`/`unpack` brand plumbing across loop edges.
2. Writing explicit brand type arguments `<A>` at every container call (`Vec::push::<A>(&mut alloc, &mut v, x)`) repeats a brand already visibly present in `&mut alloc`'s checked type.
3. Under strict linear ownership (`Q1.4`), an arena containing only pure memory values (`Heap<T, A>` with no external OS handles) still requires walking and individually destroying values before resetting unless an O(1) bulk-invalidation form exists.

**Proposed design.** Three surgical additions that preserve 100% of brand soundness ([C5](FOUNDATION.md#c5-dynamic-borrow-brands-and-authority)):

1. **Scoped scratch sub-arenas:** Allow lexically borrowing `&mut Allocator<A>` to open a child bump-arena with a fresh generative local brand `B` that rewinds to its saved mark on block exit (`borrow_scratch` / `with_scratch` form—never using closures, preserving Concrete's permanent no-closures rule via a dedicated block construct like `borrow`). Because `B` cannot escape the lexical block, temporary parser/request/codec allocations reclaim in O(1) at block exit with zero existential brand syntax.
2. **Argument-inferred brand parameters (matching [OD46](../decisions.md#od46)):** Infer brand parameters solely from checked argument types at the call site, refusing mismatches and refusing uninferable brands. Capabilities remain strictly explicit at every capability-polymorphic call site (preserving the 2026-11-05 capability-argument rule).
3. **O(1) bulk invalidation for pure-memory arena values:** For a type marker / arena-owned wrapper whose contained types hold no external linear resources (only pure memory, checked structurally by the compiler), permit O(1) arena reset to reclaim the whole region without an O(n) destructor walk, while still refusing to bulk-discard any arena holding files, sockets, grants, or custom `Destroy` resources.

**Philosophy alignment.** Keeps brands generative and sound (rule 1, rule 10), keeps capabilities 100% explicit (rule 11), keeps closures permanently refused ([What Concrete refuses](../PHILOSOPHY.md#what-concrete-refuses)), and makes O(1) arena reclamation practical for real servers and parsers (rule 17).

**Roadmap home.** [R-0718](../../ROADMAP.md#r-0718) (Phase D, Slice 19, coordinated with `R-0611`, `R-0595`, `R-0624`).

**Owner questions required.**
1. Lexical block syntax for scoped scratch sub-arenas (closure-free block form).
2. Extending [OD46](../decisions.md#od46) argument-type inference to allocator brand parameters.
3. Revising `Q1.4` to allow O(1) bulk reset for pure-memory-only arena values while strictly forbidding bulk disposal of external resources.

---

### <a id="lang-step-bounds"></a>6. Automatic worst-case complexity / gas / WCET bounds from `#[variant]` (extends Step Bounds and `R-0574`)

**Motivation.** Concrete already requires a `#[variant(e)]` expression on every `while` loop and recursive cycle ([no-hidden-traps decision](../decisions.md#decided-project-no-hidden-traps)), and counted `for i from a until b` already has an exact iteration bound `b - a`.

**Proposed design.** Observe that whenever a `#[variant(e)]` decreases by at least 1 per iteration (or recursive call) and remains `≥ 0`, the **entry value** of `e` is automatically a mathematically proved upper bound on loop iterations or recursion depth:

- Derive symbolic upper bounds on loop iterations and abstract steps directly from `#[variant]` entry expressions and counted `for` bounds with zero extra user annotations beyond what termination already requires.
- Compose those step bounds with C7 per-lineage heap accounting (`R-0613`, `R-0595`) and final-binary stack bounds (`R-0575`, `R-0505`) inside the single facts ledger (`R-0575`).
- Report abstract step bounds and flag unbounded (`#[may_not_terminate]`) paths honestly in `concrete facts`; keep wall-clock WCET claims separated behind backend timing evidence ([step-bounds decision](../decisions.md#decided-2026-11-07-step-bounds)).

**Philosophy alignment.** Extracts maximal assurance from annotations the programmer already writes (rule 5, rule 14, rule 16), directly serving the `grep` linear-time regex, EVM gas-bounded interpreter, and WebAssembly fuel-bounded sandbox [showcases](EXAMPLES.md#showcases).

**Roadmap home.** Extends [Step bounds](../../ROADMAP.md#vertical-slices) and `R-0574` (Slices 4, 5, 8). No new owner decision required beyond existing `decided-2026-11-07-step-bounds`.

---

### <a id="lang-dst"></a>7. Deterministic Simulation Testing (`concrete sim`) for free from dual authority (`R-0717`)

**Motivation.** FoundationDB, TigerBeetle and Antithesis spend years retrofitting C++, Zig and Rust codebases for Deterministic Simulation Testing (DST)—running full storage engines, consensus nodes, and servers under virtualized time, disk, network and seeded randomness so crashes, short writes, packet drops and OOMs replay deterministically from a single seed.

Concrete's architecture provides DST by construction:
- No mutable globals of any kind ([OD8](../decisions.md#od8)).
- All external authority enters `main` exclusively as linear grant values (`Fs`, `Network`, `Time`, `Entropy`, `Alloc`) ([authority rules](../decisions.md#decided-2026-11-02-authority)).
- All external observations are typed request/response event traces ([C6](FOUNDATION.md#c6-observations-environment-and-concurrency-extension)).

**Proposed design.** Build `concrete sim`, an adversarial deterministic grant launcher:

- Run any unmodified Concrete binary (or Core program) under virtualized deterministic grants (`concrete sim --seed 42 --oom-rate 0.01 --short-writes --packet-drop 0.05`).
- Systematically fault `AllocFailure`, short reads/writes, `EINTR`, non-monotonic or jumped clocks, directory permission errors, and dropped/reordered packets while verifying linear handle conservation and post-failure invariants.
- Shrink failing event traces deterministically and replay the minimized trace directly against the Lean Core reference evaluator (`R-0586`) to separate compiler bugs from program bugs.
- Directly accelerates acceptance testing for file-copy (Slice 18), RocksDB (Slice 25), the web server (Slice 27), and ethlambda (Slice 28).

**Philosophy alignment.** Pure tooling win exploiting existing language constraints (rule 11, rule 14, rule 17); adds zero language complexity and zero hidden runtime to production binaries (rule 8).

**Roadmap home.** [R-0717](../../ROADMAP.md#r-0717) (Phase D, coordinated with `R-0586`, `R-0557`, `R-0595`). No language-rule owner decision required.

---

## Part 2: Compiler, Prover and Tooling Enhancements

### <a id="comp-verified-hints"></a>1. Verified optimizer hints from `CheckedCore` and `Facts` (`R-0711`)

**Motivation.** Today a discharged-evidence token in MIR → BIR simply omits a runtime overflow or bounds trap check. Because `CheckedCore` and `Facts` already establish mathematical proofs that C and C++ compilers only guess via undefined behavior, Concrete can safely emit stronger optimization metadata than C while keeping zero UB.

**Proposed design.**
- **LLVM backend (`R-0503`):** Emit `nuw`/`nsw` on integer `add`/`sub`/`mul`, `!range` metadata on bounded scalars/indices, `noalias` on disjoint second-class `&`/`&mut` parameters (proved non-aliasing by C5), `readonly`/`memory(none)` on pure (`with()` empty) functions without `&mut`, and `willreturn`/`nounwind` on proved-terminating functions—strictly gated by discharged-evidence tokens and proved `Facts` judgments, never inferred heuristically.
- **LCB native backend (`R-0517`):** Consume the exact same proved purity, exclusivity, non-aliasing and range facts for common subexpression elimination, loop-invariant code motion, dead pure-call elimination, and width narrowing under [Foundation Q7 option (c)](../decisions.md#foundation-q7).
- **Soundness controls:** Negative controls verify that unproved or `#[may_not_terminate]` sites never receive optimizer hints, and differential tests (`R-0578`) check observation equivalence with hints enabled versus disabled.

**Philosophy alignment.** Every optimization hint carries a single proved owner in `CheckedCore`/`Facts` (rule 1, rule 5, rule 12) and never turns an unproved fact into an assumption.

**Roadmap home.** [R-0711](../../ROADMAP.md#r-0711) (Phase C, alongside `R-0503`, `R-0574`, `R-0575`, `R-0581`).

---

### <a id="comp-bitmask-rules"></a>2. Deterministic bitmask, modulo and shift transfer rules beside octagons (`R-0712`)

**Motivation.** Octagon inequalities (`±x ± y ≤ c`) handle linear bounds well, but systems code constantly relies on bitwise and modulo idioms that octagons alone cannot prove:
- Ring-buffer wraparound (`idx & (CAP - 1) < CAP` or `idx % len < len`)
- Hash-table bucket indexing (`hash & 255 < 256`)
- UTF-8, QOI, PNG, ChaCha20 and EVM bitfield extraction (`(byte >> 4) < 16`, `byte & 0x3F < 64`)
- Overflow-free midpoint (`lo + ((hi - lo) >> 1)`)

Without lightweight deterministic rules for these idioms, programmers would have to attach manual `#[proof(...)]` theorems to routine indexing and bit-packing code.

**Proposed design.** Extend `R-0574`'s decidable, Lean-proved checker (strictly **no SMT** in admission!) with a small closed table of kernel-checked transfer rules alongside octagons:
- Bitmask bounds: `0 ≤ (x & mask) ≤ mask` (for unsigned / non-negative `mask`), plus known-zero/known-one bit tracking.
- Modulo bounds: `d > 0 → 0 ≤ (x % d) < d`.
- Right-shift bounds: `(x >> k) ≤ (max(x) >> k)` for unsigned `x` and `k < width`.
- Constant-factor / midpoint identities (`lo ≤ hi → lo ≤ lo + ((hi - lo) >> 1) ≤ hi`).

Each transfer rule is a single Lean theorem proved once against `Scalar` (`R-0608`), preserves decidability and determinism, and never introduces solver unpredictability.

**Philosophy alignment.** Preserves [no hidden traps](../decisions.md#decided-project-no-hidden-traps) and zero-SMT admission (rule 18), directly reduces proof noise in real systems workloads (rule 17), and keeps every rule small and kernel-checked (rule 6, rule 15).

**Roadmap home.** [R-0712](../../ROADMAP.md#r-0712) (Phase C, Slice 3+, extends `R-0574`).

---

### <a id="comp-extract-lean"></a>3. Pure-functional Lean extraction (`concrete extract-lean`) pulled to Slice 8 (extends `R-0495`, `R-0177`)

**Motivation.** Proving functional correctness directly over a big-step/small-step operational machine configuration (`Core.eval` with heap, frame stack, fuel, and event trace) is painful for ordinary users and LLMs alike. Because Concrete enforces a clean separable core (pure functions, linear ownership, no hidden globals, second-class references), pure functions behave mathematically like ordinary Lean functions (the Aeneas insight—already noted in `README.md`—achieved natively because Concrete already compiles inside Lean 4).

**Proposed design.** Pull proof-order item 4's pure/separable-core slice forward from Phase E to **Slice 8** (once the owner approves the supported subset, representation, and residual assumptions as required by `R-0495` and `R-0574`):

- Add `concrete extract-lean`, emitting a clean, idiomatic pure-functional Lean model `f_model` for every eligible pure/separable-core Concrete function (starting with pure arithmetic, structs, enums, arrays and counted/variant loops).
- Automatically generate and kernel-check the refinement bridge theorem `f_refines : Core.eval f = f_model` so users and LLMs write high-level functional correctness proofs (`simp`, `omega`, `grind`, induction) over `f_model` while the compiler composes them automatically into Core-level theorems.
- Keep stateful/open-module/raw-memory functional models in Phase E (`R-0495`, `R-0705`).

**Philosophy alignment.** Directly serves rule 7 (proofs attach to real Concrete code through the semantics, not a parallel hand-written reimplementation) and unlocks practical verification on Slices 8–18 showcases (`grep`, base64, ChaCha20, EVM opcodes, QOI/PNG).

**Roadmap home.** Extends `R-0495`, `R-0574`, and `R-0177` (owner-approved subset/assumptions required before implementation).

---

### <a id="comp-validators-egraphs"></a>4. Translation-validated passes and proof-producing e-graphs (extends `R-0583`, `R-0299`)

**Motivation.** Proving complex search-heavy optimization passes (SSA construction, dominance frontiers, graph-coloring register allocation, algebraic rewrite search) directly in Lean is expensive and brittle whenever a heuristic changes.

**Proposed design.** Lean fully into Concrete's existing proved-validator architecture (`R-0583`, `R-0504`, `R-0299`, and proof-order item 8):

- **SSA dominator validator:** Run fast unverified Cooper–Harvey–Kennedy / Lengauer–Tarjan dominator construction and verify the resulting `idom` tree with a ~50-line Lean-proved validator.
- **CompCert-style register-allocation validator:** Run any heuristic allocator (linear scan, graph coloring, coalescing) and verify live-range preservation and move-equivalence in a proved symbolic validator.
- **Proof-producing e-graphs (proof-order item 8):** Allow untrusted equality-saturation search over a library of kernel-checked BIR/MIR rewrite rules (`x + 0 = x`, strength reduction, algebraic folding, dead-branch pruning) that emits a tiny replay witness checked linearly by a proved Lean validator before acceptance.

**Philosophy alignment.** Decouples optimization quality from proof maintenance while keeping the trusted computing base tiny (rule 1, rule 12).

**Roadmap home.** Extends `R-0583` (validator table) and `R-0299` (translation validation / proof-order item 8).

---

### <a id="comp-binary-validator"></a>5. Direct LCB machine code + proved Mach-O/ELF binary validator with in-Lean stack bounds (extends `R-0517`, `R-0505`)

**Motivation.** Emitting text assembly and shelling out to an external assembler (`as`) leaves assembler parsing, pseudo-instruction expansion, and relocation encoding in the trusted base, while computing final-binary stack bounds in external scripts keeps stack-budget enforcement outside the kernel-checked boundary.

**Proposed design.**
- **Direct LCB machine code & relocation emission (`R-0517`):** Emit AArch64 (and later x86-64) instruction words and relocation tables directly from LCB's ISA model, removing text assembly and external `as` from the verified native path.
- **Proved Mach-O / ELF binary validator with in-Lean stack bounds (`R-0505`):** Decode the final linked executable's `.text` section directly inside Lean against LCB's ISA model, verify instruction-byte and relocation correspondence, and compute exact per-function frame sizes and call-graph maximum stack bounds inside Lean itself—retiring external Python stack-analysis scripts on the proven path and feeding `R-0575` / `R-0704` directly from the proved binary validator.

**Philosophy alignment.** Shrinks the trusted base honestly (rule 12, `R-0509`) and gives stack-budget enforcement (`R-0575`, `R-0704`) one kernel-checked owner (rule 5).

**Roadmap home.** Extends `R-0517` and `R-0505` (Phase E).

---

### <a id="comp-merkle-incremental"></a>6. Two-level declaration Merkle hashing (`interface_hash` vs `body_hash`) (extends `R-0556`, `R-0149`)

**Motivation.** In verified compilation, re-running checking, codegen, and external proof replay across an entire module tree after a private body tweak kills iteration speed and CI budgets (`R-0513`, `R-0550`).

**Proposed design.** Split every declaration's content hash into two distinct, compiler-owned semantic identities (rule 13):

- **`interface_hash`:** Hash of the declaration's signature, capability set, `#[requires]`/`#[ensures]`, struct `#[invariant]`, layout/ABI summary, and exported facts summary.
- **`body_hash`:** Hash of the private implementation body and its internal obligations.

Downstream modules, callers, and external proofs that depend only on `interface_hash` stay valid across body-only edits (while every reused proof still passes `R-0697`'s mandatory Lean kernel check against its exact proposition and dependencies—caching accelerates lookup and avoids redundant re-elaboration/codegen, never bypassing kernel verification).

**Philosophy alignment.** Directly implements [AT 5](../PHILOSOPHY.md#admission-test) (declaration-level dependence) and [AT 12](../PHILOSOPHY.md#admission-test) (separate identities for independently changing surfaces).

**Roadmap home.** Extends `R-0556` (Slice 17 semantic interfaces) and `R-0149` (Phase G incremental builds).

---

### <a id="comp-assurance-diff"></a>7. Semantic assurance diffing (`concrete diff`) and `Concrete.toml` ratchets (extends `R-0201`)

**Motivation.** For teams and automated reviewers auditing PRs—especially AI-written PRs—a textual `git diff` buries the five questions an auditor actually cares about: Did a function gain a capability? Start allocating? Add `trusted`/`unsafe`/FFI? Increase its stack or heap bound? Drop a proved theorem to unproved?

**Proposed design.** Pull `concrete diff <base> <head>` forward from Phase F once Slice 6/8 facts are live (keeping full package-scale audit bundles in Phase F):

- Compare the versioned `concrete facts` JSON outputs (`R-0575`, `R-0593`) of two commits by typed declaration identity (`R-0492`), producing a concise semantic diff of capability changes, allocation changes, trusted/foreign boundary changes, termination/may-abort changes, stack/heap/step bound regressions, and proof-status changes.
- Support declarative ratchets in `Concrete.toml` (`R-0589`, `R-0576`) so CI automatically refuses unapproved capability expansion, new `trusted` surfaces, or regressed bounds without blocking unrelated refactoring.

**Philosophy alignment.** Turns [artifact-first auditability](../PHILOSOPHY.md#the-rules) (rule 14) into an everyday code-review superpower.

**Roadmap home.** Extends `R-0201` (diffing & ratchet increment pulled earlier after Slice 6/8; full package audit bundle stays in Phase F).

---

### <a id="comp-repair-trace"></a>8. Proof-guided LLM repair diagnostics and `concrete trace` Miri-grade debugger (extends `R-0593`, `R-0599`, `R-0177`, `R-0586`)

**Motivation.** Concrete explicitly anticipates that most Concrete code will be written by language models and reviewed by humans and checkers ([docs/PHILOSOPHY.md](../PHILOSOPHY.md#what-concrete-is-for)). Turning every verification refusal and runtime bug into structured, exact semantic evidence multiplies both human and AI productivity.

**Proposed design.**
1. **Proof-guided structured refusal payloads (`R-0593`, `R-0599`, `R-0177`):** Whenever the deterministic checker refuses an obligation (overflow, bounds, precondition, termination variant), `--format=json` (and human output) includes:
   - The inferred octagon and bitmask fact set active at the failure site;
   - The exact missing inequality or obligation proposition;
   - `R-0599`'s replayed concrete counterexample when bounded search finds one (distinguishing real bugs from missing proof hints);
   - A ready-to-fill `Obligation_<id>.lean` proof stub bound to the exact obligation identity, which re-enters compilation as a kernel-checked discharged-evidence token under `R-0509` and `R-0702` once proved.
2. **Miri-grade reference debugger (`concrete trace`, extends `R-0586`):** Expose the executable Lean Core evaluator directly as an interactive/trace CLI (`concrete trace`) over the hybrid memory state ([C4](FOUNDATION.md#c4-byte-memory-and-value-validity), [C5](FOUNDATION.md#c5-dynamic-borrow-brands-and-authority), [C7](FOUNDATION.md#c7-accounting)), reporting exact byte provenance, active loan trees, allocator brands, per-lineage allocation accounting, and step-by-step traps on the authoritative semantics.

**Philosophy alignment.** Consumes existing spec machinery (`R-0586`, `R-0593`, `R-0599`, `R-0614`) with zero second semantics (rule 5, rule 6, rule 14).

**Roadmap home.** Extends `R-0593`, `R-0599`, `R-0177`, and `R-0586`.

---

## <a id="appendix-early-audit-options"></a>Appendix: Earlier Audit Follow-Up Options (Deferred to Post-Slice-0 Owner Review)

Separate from `R-0711`–`R-0718`, three small surface-language options identified during the codebase audit are recorded here as future owner questions once Slice 0 compiles end-to-end and real programs measure their cost (rule 17):

1. **Disjoint struct-field borrows and slice split-borrow (`borrow mut xs split at mid`):** Today's whole-root borrow rule (`C5`) freezes the entire root struct or array on any `&mut` borrow, preventing passing `&mut s.a` and `&mut s.b` together or splitting a mutable slice into two disjoint halves without `unsafe`. A clean, lifetime-free extension would allow sibling field borrows on distinct fields of one struct and a primitive `split_at_mut` borrow block for arrays/slices, keeping references strictly second-class.
2. **Disarmable `defer` (ownership-transfer cancellation):** Under [OD69](../decisions.md#od69) and [OD140](../decisions.md#od140), `defer` registers cleanup on every returning path, which complicates constructor functions that allocate a resource, register rollback cleanup for early-error paths, and then return the resource on the success path. An explicit disarmable guard or move-out cancellation pattern avoids awkward `Option<Handle>` wrappers while keeping cleanup 100% explicit.
3. **`Surface0` integer-literal ergonomics:** Evaluating whether small literal defaults or contextual literal typing ([literal decision](../decisions.md#decided-2026-11-08-language-literals)) reduce noise in early slices without weakening explicitness at overflow boundaries.

None of these three items schedules work yet; each waits for real-program pain after Slice 0 compiles and explicit owner disposition.
