**PROPOSED (2026-11-22; roadmap reordering decided by the owner on 2026-11-22, [decision](../decisions.md#decided-2026-11-22-roadmap-reordering)); awaiting Codex and second-reviewer review and owner decisions on language rows. Decides no language rule.**

# Language and Compiler Enhancements and Roadmap Reordering (2026-11-22)

**Scope.** This document records the [2026-11-22 roadmap reordering](#roadmap-reordering) approved by the owner and collects high-leverage language, specification, compiler, proof-velocity and tooling enhancements designed specifically around Concrete's existing architectural invariants—strict linearity, second-class references, dual-key authority (`with(...)` plus linear resource grants), executable Lean Core semantics, one `Layout` authority, proved ghost erasure, and mandatory termination variants—mapping each item to its exact home in [ROADMAP.md](../../ROADMAP.md).

Every item is evaluated against [docs/PHILOSOPHY.md](../PHILOSOPHY.md#the-rules) and the [admission test](../PHILOSOPHY.md#admission-test). Nothing in this document decides any open language question or reopens any decided language rule or governance policy on its own: every language/grant proposal (`R-0713`–`R-0716`, `R-0718`–`R-0722`, `R-0725`–`R-0726`) and every policy-relaxing velocity item in `R-0724` requires an explicit owner decision recorded in [docs/decisions.md](../decisions.md) before entering `ConcreteSpec` or enforced checks.

## <a id="roadmap-reordering"></a>Roadmap reordering (decided 2026-11-22)

Under [decided-2026-11-22-roadmap-reordering](../decisions.md#decided-2026-11-22-roadmap-reordering) and [decided-2026-11-22-llm-inference-showcase-and-float-order](../decisions.md#decided-2026-11-22-llm-inference-showcase-and-float-order), [ROADMAP.md](../../ROADMAP.md) adopts five structural ordering changes plus the Slice 18/19 float swap and `concrete-llm` showcase:

1. **Unblock Slices 0–5 from the full `A2i → A2c` freeze (strictly spec-first per vertical slice):** never build a compiler slice ahead of *that slice's own* Lean rules in `ConcreteSpec`; start Slices 0–5 on an unfrozen Core by integrating each slice's immediately needed `C1–C10` contracts first, complete all remaining `C1–C10` foundation contracts (`Brand`, `Accounting`, `Hybrid`, `Frame`) and cross-part interaction proofs in `ConcreteSpec` before freezing Core (`A2c`) at **0.1 (Slice 11)** once `file-copy` and `grep` validate real interactions, and layer Slice-12 allocation and Slice-16 raw-pointer compiler/surface increments onto the frozen foundation in Phase D.
2. **Split `B0` and move `LP0` after Slice 5:** `B0a` delivers the working Slice-0 spine (lex/parse → elaborate → Core → MIR → BIR → LLVM + Lean oracle match + per-lowering proofs + unfrozen `0-dev` `concrete facts`); `B0b` adds unverified LCB (`R-0578`), the BIR→LCB adapter proof (`R-0579`), ASan (`R-0301`) and fuzzing (`R-0272`) after `B0a` in parallel with early slices (`B1` and Slices 2–4, under the early-slice staging rule where Slices 0–4 qualify first on LLVM + Lean oracle and close full dual-backend parity across Slices 0–5 before `LP0`); `LP0` (`R-0572`) moves after Slice 5 to prove branches, loops, calls, stack frames and the multi-frame trap ABI once; `R-0702`, `R-0699`, `R-0577` and freezing the JSON schema (`R-0178`) move from per-PR Slice-0 blockers to nightly / 0.1.
3. **Reorder Phase C (Slices 6–11):** land structs + enums/`match` + `Option`/`Result`/`checked_*` (Slice 6), borrows + arrays + `R-0712` bitmask/modulo/shift rules (Slice 7), minimal modules & `const` (Slice 8), generics + traits/`Destroy`/`defer` (Slice 9), the proof surface + `R-0495` safe-subset `concrete extract-lean` + `R-0201` `concrete diff` (Slice 10), and authority + console/UTF-8 + files (`Fs`) + `R-0717` `concrete sim` (`Fs`/Entropy leg) ending at **0.1** (Slice 11). DWARF (`R-0587`) and the formatter move after 0.1 (Slice 12).
4. **Reorder Phase D (Slices 12–21):** land allocation (Slice 12), `HeapArray` (Slice 13), `Vec`/`String` (Slice 14), `OrderedMap`/`HashMap` (Slice 15), raw pointers/`trusted` (Slice 16), FFI/RocksDB (Slice 17, earliest `LD` / `legacy/` deletion) and floats (`f32`/`f64`, plus `concrete-llm` CPU & Concrete+Metal local inference showcase at Slice 18) ahead of function values + `R-0601` contracts (Slice 19), followed by clock/network/web server + `concrete sim` Time/Network faulting (Slice 20) and concurrency (Slice 21).
5. **Pull forward high-leverage checks and pause new Phase T meta-tooling:** pull `R-0712` (Slice 7), `R-0495` safe subset (Slice 10), `R-0201` `concrete diff` (Slice 10) and `R-0717` `concrete sim` `Fs`/Entropy leg (Slice 11 / 0.1) earlier; pause new `Phase T` governance meta-tooling (`R-0692`, `R-0693`, remaining `R-0710` items) unless a real bug hits `main` while running `R-0724` to cut existing proof/CI drag.

## Roadmap mapping at a glance

To keep [every fact once](../PHILOSOPHY.md#the-rules) (rule 5), ideas that refine or pull forward existing roadmap obligations extend their current rows; genuinely new capabilities and audit-identified workstreams receive proposed rows `R-0711`–`R-0727` (scheduled in the companion [ROADMAP.md](../../ROADMAP.md#tasks) update; subject to rebase if other IDs land first).

### New roadmap tasks (`R-0711`–`R-0727`)

| Task | Phase / Slice | Kind | Summary | Owner-gated? |
|---|---|---|---|---|
| `R-0711` | C (Slice 3+) | Compiler | [Verified optimizer hints from `CheckedCore` and `Facts`](#comp-verified-hints) | No |
| `R-0712` | C (Slice 7) | Spec / checker | [Deterministic bitmask, modulo and shift transfer rules](#comp-bitmask-rules) | No |
| `R-0713` | C (Slice 7) | Language | [`&out T` write-only second-class references](#lang-out-ref) | **Yes** |
| `R-0714` | C (Slice 7) | Language / layout | [Zero-`unsafe` binary wire formats (`ByteSafe` + explicit-endian scalars)](#lang-wire-formats) | **Yes** |
| `R-0715` | C (Slice 10) | Language / std | [Zero-sized erased proof witnesses (`Witness<P>`) and linear typestate](#lang-typestate-witness) | **Yes** |
| `R-0716` | C (Slice 9) | Language / const | [Const-generic capacity parameters (`<const N: u64>`) + pure compile-time data eval](#lang-const-generics-eval) | **Yes** |
| `R-0717` | C (Slice 11 / 0.1) & D (Slice 20) | Tooling | [Deterministic Simulation Testing (`concrete sim`) over `main`'s grants](#lang-dst) | No |
| `R-0718` | D (Slice 12) | Language / alloc | [Scoped scratch sub-arenas, argument-inferred brands, O(1) arena reset and brand-preserving loops](#lang-scratch-brands) | **Yes** |
| `R-0719` | C (Slice 7) | Language / borrow | [Field-disjoint borrows and scoped slice split (`borrow mut xs split at mid`)](#lang-disjoint-borrows) | **Yes** |
| `R-0720` | C (Slice 7) | Language / borrow | [Hylo-style scoped projection functions (single-source view return without lifetimes)](#lang-scoped-projections) | **Yes** |
| `R-0721` | C (Slice 9) | Language / cleanup | [Disarmable `defer` / `errdefer` and cleanup/destroy alignment](#lang-disarmable-defer) | **Yes** |
| `R-0722` | C (Slices 2, 7, 8, 11) | Language / std | [Surface literal escapes/ergonomics, `const` arrays, systems/crypto scalars + `volatile_zero` and zero-heap `args`](#lang-surface-scalars-const) | **Yes** |
| `R-0723` | B (`B0a`/`B0b`, pre-Slice-3) & A2i | Compiler / spec | [Pre-Slice-0 and pre-Slice-3 compiler & spec correctness and scaling fixes (`LLVM`, `Lex`, `LL1`, `BIR`, `LCB`, `Runtime`, `Hybrid`/`Ident`/`Exec`)](#impl-compiler-spec-fixes) | No (spec-rule changes owner-gated) |
| `R-0724` | T (active velocity) | Proof / CI / process | [Proof and CI velocity overhaul (`ConcreteSpecEvidence` split, zero-TCB proof tactics, `axioms.golden`/`landing_inventory` cleanup, CI build-once & attestation reuse, proportion rule)](#impl-proof-ci-velocity) | Policy items **Yes** |
| `R-0725` | D (Slices 17–18) | Grant / layout / tooling | [Read-only `MappedFile` (`mmap`) linear grant + layout-verified C/Metal shader struct headers](#lang-mmap-gpu-layout) | **Yes** |
| `R-0726` | D (Slice 18) | Language / backend | [`f16`/`bf16` storage & conversions + explicit fixed-width SIMD vectors (`Simd<T, N>`)](#lang-f16-simd) | **Yes** |
| `R-0727` | E | Tooling / superopt | [Verified MAP-Elites kernel and LCB schedule search gated by Lean proofs & `concrete facts`](#comp-verified-map-elites) | No |

### Existing roadmap tasks extended or pulled forward

| Existing task(s) | Phase / Slice | Enhancement |
|---|---|---|
| Step bounds + `R-0574` | A / B / C (Slices 4, 5, 10) | [Automatic iteration/depth bounds from `#[variant]` and composed abstract step bounds](#lang-step-bounds) |
| `R-0583`, `R-0299` | B / E | [Certified SSA dominator & register-allocation validators + proof-producing e-graphs](#comp-validators-egraphs) |
| `R-0593`, `R-0599`, `R-0177` | B / C (Slices 3, 10) / E | [Proof-guided structured LLM repair diagnostics and `Obligation_*.lean` stubs](#comp-repair-trace) |
| `R-0586` | B / C (Slice 11) | [`concrete trace` Miri-grade reference debugger over hybrid Core state](#comp-repair-trace) |
| `R-0556`, `R-0149` | C (Slice 8) / G | [Two-level declaration Merkle hashing (`interface_hash` vs `body_hash`)](#comp-merkle-incremental) |
| `R-0495`, `R-0177` | E (pulled to Slice 10) | [`concrete extract-lean` pure-functional model + auto-proved refinement](#comp-extract-lean) |
| `R-0517`, `R-0505` | E | [Direct LCB machine code + proved Mach-O/ELF binary validator with in-Lean stack bounds](#comp-binary-validator) |
| `R-0201`, `R-0353` | F (diff & audit dossier pulled to Slice 10/11) | [`concrete diff` semantic assurance diffing, `Concrete.toml` ratchets and `concrete build --audit` one-command audit dossier](#comp-assurance-diff) |

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

**Roadmap home.** `R-0713` (Phase C, Slice 7 borrows/arrays).

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
- Distinguish two guarantees cleanly:
  - **Host-byte-order safety (`ByteSafe`):** safe, zero-`unsafe` byte-slice / `[u8; N]` decode and encode primitives for any `ByteSafe` type eliminate alignment UB, invalid-bit-pattern UB, and uninitialized-padding leakage, with trivial Lean `rep_roundtrip` proofs—while ordinary host integers (`u16`/`u32`/`u64`) remain target-endian under `Memory.encodeInt`.
  - **Portable cross-endian wire-format safety:** eliminating host-endianness bugs across targets additionally requires multi-byte scalar fields in a wire-format struct to use the 1-byte-aligned explicit-endian scalar types (`le_*`/`be_*`) rather than host-endian integers (enforced by a wire-safe / explicit-endian check on top of `ByteSafe`).
- Because explicit-endian scalars already have alignment 1 and `ByteSafe` rejects padding, safe wire-format structs need no `#[repr(packed)]`, cleanly separating safe wire formats from foreign C-ABI packed structs.

**Philosophy alignment.** Directly satisfies rule 1 (removes an entire class of alignment/endian/padding defects), rule 5 (single `Layout` authority), rule 15 (no macros/derives), and [AT 14](../PHILOSOPHY.md#admission-test) (audited codecs without `unsafe`).

**Roadmap home.** `R-0714` (Phase C, Slice 7 borrows/arrays, coordinated with `R-0609` and `R-0071`).

**Owner questions required.**
1. Exact scalar set and naming (`le_u16`..`be_u64`, signed variants, `u128`/`i128`).
2. Whether `ByteSafe` is a compiler-checked structural marker/trait, whether opt-in declaration is required on structs so adding a padding field is caught at the struct declaration site, and whether portable wire codecs require a separate `WireSafe` / explicit-endian-fields marker on top of host-endian `ByteSafe`.
3. Reconciliation with [OD111](../decisions.md#od111), [OD147](../decisions.md#od147) and [recovered-59-23](../decisions.md#recovered-59-23) (keeping `#[repr(C, packed)]` strictly FFI-only or retaining it).

---

### <a id="lang-typestate-witness"></a>3. Linear typestate and zero-sized erased proof witnesses (`Witness<P>`) (`R-0715`)

**Motivation.** Rust is affine: callers can silently drop an uncommitted transaction, an unfinished handshake, or a half-flushed writer. Concrete is strictly linear AND already models zero-sized types with full linear obligations (`Q3.8`), and proves ghost erasure ([C9](FOUNDATION.md#c9-identities-primitive-contracts-evidence-and-ghost-erasure), `R-0615`). Together these give Concrete strictly stronger compile-time protocol enforcement and zero-cost validation chaining than affine languages.

**Proposed design.** Standardize and support two idiomatic patterns directly on top of existing Core linearity and C9 ghost erasure:

1. **Must-finish linear typestate protocols in std:** APIs such as `Db::begin(&mut db) -> Tx<Active>` whose only consumers are `commit` or `rollback` (or typed failure recovery). Forgetting to finish a protocol or double-finishing is a compile-time error at 0 bytes of runtime overhead.
2. **Zero-sized erased proof witnesses (`Witness<P>`):** Validation functions return a 0-byte witness token whose logical meaning is linked to a predicate/fact (for example `validate_utf8(&bytes) -> Option<Witness<ValidUtf8>>` or `check_sorted(&arr) -> Option<Witness<Sorted>>`), erased completely by C9 ghost erasure into 0 runtime bytes. Downstream functions accept the witness instead of re-checking at runtime, storing redundant boolean flags, or repeating identical `#[requires]` clauses across long call chains.

**Philosophy alignment.** Zero new runtime machinery (rule 8), strict linearity already present (rule 9), proved erasure (rule 12 / C9), makes protocol completion and validation provenance visible in signatures (rule 3, AT 10).

**Roadmap home.** `R-0715` (Phase C, Slice 10 proof surface, used in Slice 11 `0.1` std protocols; coordinated with `R-0615`, `R-0694`, `R-0071`).

**Owner questions required.**
1. Whether `Witness<P>` is a dedicated ghost/proof type constructor in `ConcreteSpec` or a std pattern over zero-sized private-constructor structs with `#[invariant(...)]` (Slice 8).
2. Interaction between a witness token and mutation of the referenced data (ensuring lexical borrow-freezing or versioned/linear binding prevents stale witnesses after mutation).

---

### <a id="lang-const-generics-eval"></a>4. Const-generic capacity parameters (`<const N: u64>`) and pure compile-time data evaluation (`R-0716`)

**Motivation.** Concrete currently has two major friction points for zero-heap and predictable-policy systems code (Slices 0–11, embedded/realtime, crypto/codec showcases):

1. **No `<const N: u64>` generics:** Without const-generic capacity parameters, std cannot provide generic fixed-capacity containers (`FixedVec<T, N>`, `RingBuf<T, N>`, `FixedStr<N>`) or generic array algorithms (`fn crc32<const N: u64>(buf: &[u8; N]) -> u32`). Note that [IDEAS.md](IDEAS.md) already flagged fixed-capacity non-allocating std (`#8`) as `RETAINED-ONLY`.
2. **Separate `CExpr` mini-language for constants:** Today `ConcreteSpec/Spec/Const.lean` maintains a tiny separate expression language restricted to scalar integer/boolean expressions ([constants decision](../decisions.md#decided-2026-11-08-constants)). Generating verified static lookup tables (CRC32, AES S-boxes, ChaCha constants, UTF-8/HTTP DFA tables, EVM opcode tables) therefore requires either hand-pasted magic hex arrays or external build generators.

**Proposed design.**
- **Value-level `<const N: u64>` parameters:** Strictly monomorphized at Mono Core alongside type parameters—no dependent types, no type-level arithmetic solver beyond constant folding of closed `u64` expressions, checked by `R-0510` checked specialization. Unlocks the entire zero-heap predictable stdlib.
- **Unified pure compile-time data evaluation (constants-only):** Replace the duplicate `CExpr` evaluator by allowing `const` initializers to call any **pure** (`with()` empty), **zero-heap**, **proved-terminating** Concrete function, evaluated deterministically by the exact same fuel-bounded Core evaluator (`R-0614`, C8) with a bounded fuel/size limit (`R-0528`).
  - Zero macros, zero source generation, zero AST manipulation, zero semantic drift between compile-time and run-time evaluation.
  - Compile-time lookup tables become ordinary, auditable, Lean-provable Concrete functions.

**Philosophy alignment.** Eliminates duplicate constant-expression semantics (rule 5), keeps macros permanently refused ([What Concrete refuses](../PHILOSOPHY.md#what-concrete-refuses)), and stays strictly within the narrow "constants-only" reopening rule for general compile-time evaluation in [docs/PHILOSOPHY.md](../PHILOSOPHY.md#what-concrete-refuses) (row `P17b`).

**Roadmap home.** `R-0716` (Phase C, Slice 9 generics/const, coordinated with `R-0510`, `R-0574`, `R-0614`).

**Owner questions required.**
1. Reopening of narrow constants-only compile-time function evaluation under [PHILOSOPHY.md](../PHILOSOPHY.md#what-concrete-refuses) and [decided-2026-11-08-constants](../decisions.md#decided-2026-11-08-constants) (including fuel/size caps and determinism requirements).
2. Syntax and admitted type set for const parameters (`u64` only initially vs all integer types) and whether const expressions in types are restricted to bare parameters and integer literals.

---

### <a id="lang-scratch-brands"></a>5. Scoped scratch sub-arenas, argument-inferred brands, O(1) bulk arena reset and brand-preserving loops (`R-0718`)

**Motivation.** Concrete's branded allocator model (`Q1.1`–`Q1.13`, [allocation decision](../decisions.md#decided-2026-11-02-allocation)) gives memory-safe arena lifetimes without a borrow-checker lifetime language, but four friction points show up immediately in parsers, HTTP servers, compilers, and packet processors:

1. Reclaiming temporary per-iteration allocations via consuming `reset(a)` requires existential `exists`/`pack`/`unpack` brand plumbing across loop edges.
2. Writing explicit brand type arguments `<A>` at every container call (`Vec::push::<A>(&mut alloc, &mut v, x)`) repeats a brand already visibly present in `&mut alloc`'s checked type.
3. Under strict linear ownership (`Q1.4` and `Brand.Ok.reset` requiring `NoHeap Γ β`), an arena containing only pure memory values (`Heap<T, A>` with no external OS handles) still requires walking and individually freeing every object before calling `reset` unless an O(1) bulk-invalidation form exists.
4. `Brand.lean`'s current loop rule requires brand reset across loop edges and forbids allocations from surviving a loop, blocking ordinary loops that build a list, AST, or map incrementally into a long-lived arena.

**Proposed design.** Four surgical additions that preserve 100% of brand soundness ([C5](FOUNDATION.md#c5-dynamic-borrow-brands-and-authority)):

1. **Scoped scratch sub-arenas:** Allow lexically borrowing `&mut Allocator<A>` to open a child bump-arena with a fresh generative local brand `B` that rewinds to its saved mark on block exit (`borrow_scratch` / `with_scratch` form—never using closures, preserving Concrete's permanent no-closures rule via a dedicated block construct like `borrow`). Because `B` cannot escape the lexical block, temporary parser/request/codec allocations reclaim in O(1) at block exit with zero existential brand syntax.
2. **Argument-inferred brand parameters (matching [OD46](../decisions.md#od46)):** Infer brand parameters solely from checked argument types at the call site, refusing mismatches and refusing uninferable brands. Capabilities remain strictly explicit at every capability-polymorphic call site (preserving the 2026-11-05 capability-argument rule).
3. **O(1) bulk invalidation for pure-memory arena values:** Separate pure-memory arena values (`Arena<T, β>` holding no external linear resources such as `File`, `Socket`, grants, or custom `Destroy` types, checked structurally by the compiler) from resource-carrying `Heap<T, β>` values (which still require `NoLinearHeap Γ β`), permitting O(1) `reset` to reclaim pure-memory regions without an O(n) destructor walk while strictly forbidding bulk disposal of external resources.
4. **Brand-preserving loops alongside brand-resetting loops:** Allow loops that keep an outer allocator's brand fixed without resetting it, so loop bodies can allocate into an outer arena and retain those values after the loop exits (still bounded by C7 heap accounting).

**Philosophy alignment.** Keeps brands generative and sound (rule 1, rule 10), keeps capabilities 100% explicit (rule 11), keeps closures permanently refused ([What Concrete refuses](../PHILOSOPHY.md#what-concrete-refuses)), and makes O(1) arena reclamation and arena-building loops practical for real servers and parsers (rule 17).

**Roadmap home.** `R-0718` (Phase D, Slice 12 allocation, coordinated with `R-0611`, `R-0595`, `R-0624`).

**Owner questions required.**
1. Lexical block syntax for scoped scratch sub-arenas (closure-free block form).
2. Extending [OD46](../decisions.md#od46) argument-type inference to allocator brand parameters.
3. Revising `Q1.4` / `Brand.Ok.reset` to allow O(1) bulk reset for pure-memory-only arena values while strictly forbidding bulk disposal of external resources.
4. Admitting brand-preserving loops alongside brand-resetting loops in `Brand.lean` (C5).

---

### <a id="lang-step-bounds"></a>6. Automatic iteration/depth bounds from `#[variant]` and composed abstract step bounds (extends Step Bounds and `R-0574`)

**Motivation.** Concrete already requires a `#[variant(e)]` expression on every `while` loop and recursive cycle not explicitly permitted with `#[may_not_terminate]` ([no-hidden-traps decision](../decisions.md#decided-project-no-hidden-traps)), and counted `for i from a until b` already has an exact iteration bound `max(0, b₀ - a₀)` using the snapshotted initial bounds `a₀, b₀` (under `OD47`–`OD49`, so empty ranges with `a₀ ≥ b₀` execute 0 iterations).

**Proposed design.** Distinguish **iteration / single-path recursion depth bounds** from **total execution step bounds** cleanly:

- **Automatic iteration and single-path depth bounds:** Whenever a `#[variant(e)]` decreases by at least 1 per iteration or recursive step and remains `≥ 0`, the **entry value** of `e` (and `max(0, b₀ - a₀)` for counted `for`) automatically yields a mathematically proved upper bound on **loop iterations or recursive edges along a single path** with zero extra user annotations beyond what termination already requires.
- **Composed total abstract step bounds:** Single-path depth alone does **not** bound total work (for example, branching recursion making two calls with measure `n - 1` has exponential total work despite depth `n`, and a bounded-iteration loop whose body calls a callee lacking an established termination or step bound cannot establish a finite total step bound). Deriving a total abstract step bound in the facts ledger therefore additionally requires established per-iteration body and callee step bounds, established termination of every callee, and explicit composition across all call sites (accounting for branching fan-out). Consistent with [no hidden traps](../decisions.md#decided-project-no-hidden-traps) and `R-0575`, `#[may_not_terminate]` is a permission to leave a termination obligation open—never evidence of divergence—and unused permissions are permitted: preserve an established finite bound even when a callee carries an unused `#[may_not_terminate]` permission, and report missing termination/step-bound evidence or uncomposed branching fan-out as `"not established"` (naming any responsible `#[may_not_terminate]` source permission and call path, consistent with `R-0575` and `R-0595`).
- Compose established step bounds with C7 per-lineage heap accounting (`R-0613`, `R-0595`) and final-binary stack bounds (`R-0575`, `R-0505`) inside the single facts ledger (`R-0575`).
- Report established abstract step bounds, unused permissions, and `"not established"` reasons (including responsible `#[may_not_terminate]` source permissions and call paths) honestly in `concrete facts`; keep wall-clock WCET claims separated behind backend timing evidence ([step-bounds decision](../decisions.md#decided-2026-11-07-step-bounds)).

**Philosophy alignment.** Extracts maximal assurance from annotations the programmer already writes (rule 5, rule 14, rule 16), directly serving the `grep` linear-time regex, EVM gas-bounded interpreter, and WebAssembly fuel-bounded sandbox [showcases](EXAMPLES.md#showcases).

**Roadmap home.** Extends [Step bounds](../../ROADMAP.md#vertical-slices) and `R-0574` (Slices 4, 5, 10). No new owner decision required beyond existing `decided-2026-11-07-step-bounds`.

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
- Directly accelerates acceptance testing for `file-copy` (Slice 11 / 0.1), RocksDB (Slice 17), the web server (Slice 20), and ethlambda (Slice 21).

**Philosophy alignment.** Pure tooling win exploiting existing language constraints (rule 11, rule 14, rule 17); adds zero language complexity and zero hidden runtime to production binaries (rule 8).

**Roadmap home.** `R-0717` (Phase C Slice 11 / 0.1 for `Fs` and `Entropy` faulting; Phase D Slice 12 for `Alloc` faulting and Slice 20 for `Time`/clock and `Network` faulting; coordinated with `R-0586`, `R-0557`, `R-0595`). No language-rule owner decision required.

---

### <a id="lang-disjoint-borrows"></a>8. Field-disjoint borrows and scoped slice split (`borrow mut xs split at mid`) (`R-0719`)

**Motivation.** `ConcreteSpec/Spec/Own.lean` already records `path : List Projection` on every `Loan`, yet `Own.Compatible` ignores `path` completely and locks the whole root. As a result, routine safe systems code—`f(&mut db.wal, &db.options)`, `f(&mut vm.stack, &mut vm.mem)`, `add(&mut r, r.step)`, or borrowing `row[cur]` alongside `row[prev]` in a PNG scanline decoder—is refused and forces artificial struct splitting or temporary copies.

**Proposed design.**
- Upgrade `Own.Compatible` in C5 (`R-0611`) so same-root loans are compatible whenever their projection paths diverge at distinct struct fields (`.field f₁ ≠ .field f₂`), while keeping whole-array exclusion on `.index` so no dynamic aliasing proof is needed.
- Add one closure-free scoped split form for mutable arrays and slices (`borrow mut xs split at mid ... (lo, hi) { ... } [else { ... }]`), granting two disjoint mutable range views over `[0, mid)` and `[mid, len)` within the lexical block with zero lifetimes and purely local proof obligations.

**Philosophy alignment.** Exploits projection information already present in `Own.Loan` (rule 5), keeps references strictly second-class with zero lifetimes (rule 10, AT 4), and directly unblocks the RocksDB, EVM, and PNG showcases (rule 17).

**Roadmap home.** `R-0719` (Phase C, Slice 7 borrows/arrays, coordinated with `R-0611`).

**Owner questions required.**
1. Revising whole-root `Own.Compatible` to permit sibling distinct-field projections while keeping `.index` whole-array exclusive.
2. Exact syntax and out-of-bounds / empty-half contract for `borrow mut xs split at mid`.

---

### <a id="lang-scoped-projections"></a>9. Hylo-style scoped projection functions (single-source view return without lifetimes) (`R-0720`)

**Motivation.** With strictly downward views (`Borrow.lean`), functions cannot return a borrowed view derived from an input view—so std and user code cannot write `Map::get(&m, k) -> Option<&V>` or zero-copy header/chunk extractors, forcing raw `(handle, start, end)` integer offsets everywhere.

**Proposed design.** Allow a scoped projection function whose returned second-class view (or `Option`/`Result` directly wrapping one) is bound lexically to **one designated input view** at the call site inside a `borrow` block:
- Zero lifetime names, zero lifetime inference/algebra, zero stored references, and zero multi-source lifetime ambiguity.
- The caller's loan on the designated input view stays active for the exact lexical scope of the returned view and releases cleanly on block exit, preserving C5 loan-tree soundness.

**Philosophy alignment.** Delivers Hylo's mutable-value-semantics ergonomics while strictly preserving Concrete's no-lifetimes, second-class-reference rule ([What Concrete refuses](../PHILOSOPHY.md#what-concrete-refuses), rule 10, AT 4).

**Roadmap home.** `R-0720` (Phase C, Slice 7 borrows/arrays, feeding Slice 15 maps and zero-copy codecs).

**Owner questions required.**
1. Signature and call-site syntax for designated single-source projection functions.
2. Whether `Option<&V>` / `Result<&V, E>` (and `&mut V` equivalents) are admitted directly as lexical projection outcomes.
3. Interaction with `Borrow.lean` / `Own.lean` loan-tree rules and `R-0611`.

---

### <a id="lang-disarmable-defer"></a>10. Disarmable `defer` / `errdefer` and cleanup-destroy alignment (`R-0721`)

**Motivation.** Three concrete cleanup issues surface immediately in real linear code and the current spec:
1. With strict linearity, no `?`, and unconditional `defer` ([OD69](../decisions.md#od69), [OD140](../decisions.md#od140)), the ubiquitous fallible-constructor pattern (*allocate A, allocate B, on error clean up A, on success return `(A, B)`*) cannot use `defer` cleanly because moving `A` into `return Ok(...)` conflicts with deferred `destroy(A)`.
2. `Cleanup.lean` marks `defer` targets consumed *at registration*, whereas `Exec.lean` and `Own.lean` model *late reads at exit*—a live spec contradiction that must be resolved before Slice 9.
3. `Exec.lean`'s arity-1 `DestroyProtocol` does not match `Brand.lean`'s arity-2 branded release `free(h, a)`, and `Exec.lean`'s `mapDestroyOrder` hardcodes ascending key order, which does not fit `HashMap` (Slice 15).

**Proposed design.**
- Add an explicit linear `DeferGuard` with `disarm(g)` (or explicit `errdefer`) so rollback cleanup cancels visibly on the success path without `Option<Handle>` boilerplate or hidden control flow.
- Reconcile `Cleanup.lean` vs `Exec.lean`/`Own.lean` registration/exit semantics into one C1/C5 contract, align `DestroyProtocol` with branded allocator release (`R-0624`), and decouple `HashMap` destruction order from `OrderedMap`'s ascending key order.

**Philosophy alignment.** Keeps cleanup 100% explicit and linear (rule 3, rule 9, AT 1), removes spec contradictions (rule 5), and eliminates nested cleanup pyramids in fallible constructors (rule 17).

**Roadmap home.** `R-0721` (Phase C, Slice 9 traits/`Destroy`/`defer`, coordinated with `R-0607`, `R-0611`, `R-0624`).

**Owner questions required.**
1. Explicit linear `DeferGuard` + `disarm(g)` versus `errdefer` spelling and move-out rules.
2. Resolving `Cleanup.lean` (registration consumption) vs `Exec.lean`/`Own.lean` (late exit read).
3. `DestroyProtocol` arity / branded-allocator release alignment and `HashMap` destruction contract.

---

### <a id="lang-surface-scalars-const"></a>11. Surface literals, constant arrays, systems/crypto scalars and zero-heap std gaps (`R-0722`)

**Motivation.** Auditing `ConcreteSpec/Spec/Surface0.lean`, `Scalar.lean`, `Const.lean`, and the [Std per slice](../../ROADMAP.md#std-per-slice) table against the adopted showcases reveals four concrete cliffs:
1. **Unexpressible and audit-hostile literals (`Surface0.lean`):** Raw control/bidi characters are banned AND only 6 escapes (`\n\t\r\\\"\0`) exist—no `\xHH` or `\u{...}`—making ANSI escapes (`"\x1b[31m"` for `grep`) and PNG magic bytes (`"\x89PNG\r\n\x1a\n"`) literally unexpressible. Banning `_` digit separators (`1_000_000_000_000_000_000`), uppercase hex (`0xDEADBEEF`), and redundant matching suffixes (`let x: u8 = 200u8;`) hurts both human auditability and LLM code generation.
2. **Scalar-only `const` + shift-count mismatch (`Const.lean` vs `Scalar.lean`):** Restricting `const` to `IntTy`/`bool` blocks static lookup tables (SHA-256 `K[64]`, AES S-boxes, Keccak `RC[24]`, CRC32, EVM gas tables, UTF-8 DFAs). Meanwhile `Scalar.lean` allows mixed-type shift counts (`i8 << u8`), whereas `Const.lean` enforces `sameInt`, creating an accidental semantic mismatch.
3. **Missing systems/crypto scalar primitives (`Scalar.lean`):** EVM `u256`, X25519, Poly1305, and bit-twiddling codecs need `wrapping_neg`, `rotl`/`rotr`, `clz`/`ctz`/`popcnt`, `widening_mul`, and `carrying_add`/`borrowing_sub`. Separately, because the [2026-11-21 secrets decision](../decisions.md#decided-2026-11-21-secrets-after-1-0) moved wiping and its backend evidence to [after 1.0](../../ROADMAP.md#after-10), admitting a standalone `volatile_zero` primitive surviving backend dead-store elimination before 1.0 requires an explicit owner reopening of that 2026-11-21 deferral (otherwise it stays deferred with `R-0596` after 1.0).
4. **Zero-heap `args` / fixed-buffer `bytes` / wide-int std gaps:** `grep PATTERN FILE` at Slice 11 (0.1, zero heap) needs a non-allocating `args` contract, and EVM/crypto stages at Slices 7–11 need fixed-buffer `bytes` and wide-integer arithmetic contracts.

**Proposed design.** Resolve all four cleanly before Slice 2 locks the lexer and before Slices 7–11 reach the showcases:
- Extend `Surface0` escapes with `\xHH` and `\u{...}` (keeping raw control/bidi chars and raw `U+FEFF` BOM refused), admit `_` digit separators, uppercase hex, and matching redundant suffixes (and evaluate contextual literal typing without weakening overflow explicitness).
- Fix the `Const.lean` vs `Scalar.lean` shift-count mismatch and extend `const` to fixed arrays/tables (coordinating with `R-0716`).
- Add the missing systems/crypto scalar operations to C2, and—only if the owner explicitly reopens the [2026-11-21 wiping deferral](../decisions.md#decided-2026-11-21-secrets-after-1-0)—add one `volatile_zero` primitive to C4/C6 with backend DSE-survival evidence.
- Specify zero-heap `args`, fixed-buffer `bytes`, and wide-integer std contracts under `R-0071`.

**Philosophy alignment.** Directly improves human and LLM auditability (rule 3, AT 2), removes spec drift between `Const` and `Scalar` (rule 5), and unblocks every adopted zero-heap showcase (rule 17).

**Roadmap home.** `R-0722` (Phase C: literals before Slice 2; scalars/bytes at Slice 7; `const` arrays at Slice 8; zero-heap `args` at Slice 11; coordinated with `R-0608`, `R-0620`, `R-0071`).

**Owner questions required.**
1. Literal revisions to [decided-2026-11-08-language-literals](../decisions.md#decided-2026-11-08-language-literals) (`\xHH`, `\u{...}`, `_` separators, uppercase hex, redundant matching suffixes, contextual typing).
2. Constant arrays/tables under [decided-2026-11-08-constants](../decisions.md#decided-2026-11-08-constants) and shift-count alignment.
3. Exact systems/crypto scalar intrinsic set, and whether to reopen the [2026-11-21 decision](../decisions.md#decided-2026-11-21-secrets-after-1-0) moving wiping and its backend evidence after 1.0 to admit `volatile_zero` earlier.
4. Non-allocating `args` and wide-integer std module contracts.

---

### <a id="lang-mmap-gpu-layout"></a>12. Read-only `MappedFile` (`mmap`) linear grant and layout-verified C/Metal shader struct headers (`R-0725`)

**Motivation.** Real zero-copy data-intensive workloads—specifically the adopted **`concrete-llm` local LLM inference engine** (mapping 20–80 GB GGUF/Safetensors weight files) and **RocksDB SSTables**—cannot copy multi-gigabyte immutable files into heap `Vec`s. Meanwhile, GPU-accelerated engines written in a host systems language + Metal/CUDA shaders suffer from silent host/device ABI struct drift (size, alignment, or offset mismatches across languages) and manual GPU buffer lifecycle bugs.

**Proposed design.**
1. **Read-only `MappedFile` linear grant under `Fs`:** Narrowed to a granted directory (`with(Fs)`), producing a read-only byte view (`&[u8]`) backed by read-only `mmap` with explicit consuming `unmap` (zero mutable shared mappings initially, avoiding shared-memory aliasing/data-race UB). Because read-only `mmap` (even `MAP_PRIVATE`) does not prevent another process from overwriting or truncating the backing file—leaving visibility of external overwrites unspecified (`mmap(2)`) and risking stale validation witnesses while the view stays live—the immutable `&[u8]` view requires either an **explicit named content-stability premise** (alongside truncation/`SIGBUS`) or an **OS write-seal / immutable snapshot mechanism** (such as `F_SEAL_WRITE` or snapshot copy). Combined with `R-0714` (`ByteSafe`) and `R-0715` (linear typestate `GpuBuf<Idle>`/`GpuBuf<InFlight>` + `CmdBuf<Recording>` over Slice-17 `Kept<H>` Metal FFI), a Concrete+Metal inference engine proves zero-alloc decode, leak-free/race-free GPU scheduling, zero-`unsafe` GGUF parsing, and `Network: NONE` local privacy.
2. **Layout-verified C/Metal shader struct headers:** Emit C / Metal Shading Language struct definitions (with `static_assert` size/offset checks) directly from Concrete's single `C3` `Layout` + `ByteSafe` authority (`R-0609`, `R-0714`) so host and GPU shader buffer contracts cannot silently diverge.

**Philosophy alignment.** Single layout authority (rule 5), explicit linear authority and cleanup (rule 9, rule 11), zero hidden copies (rule 8), driven by real high-impact systems workloads (rule 17).

**Roadmap home.** `R-0725` (Phase D, Slices 17–18, enabling `concrete-llm` and RocksDB).

**Owner questions required.**
1. Exact `MappedFile` read-only grant operations, consuming `unmap` contract, and how concurrent external file modification and truncation are handled (explicit named content-stability + `SIGBUS` target premise versus OS write-seal such as `F_SEAL_WRITE` or snapshot copy so live validation witnesses cannot go stale).
2. CLI/build interface and supported target dialects (C / MSL / WGSL) for generated layout-verified struct headers.

---

### <a id="lang-f16-simd"></a>13. `f16`/`bf16` storage & conversions and explicit fixed-width SIMD vectors (`R-0726`)

**Motivation.** Moving `f32`/`f64` to Slice 18 unblocks basic float workloads and Metal GPU inference, but modern local LLM inference (GGUF/Safetensors weights, dequantization, RoPE, softmax), image codecs, and crypto also rely on 2-byte half-precision weights (`f16`/`bf16`) and explicit SIMD vectorization (matching Zig's `@Vector` and `f16` on NEON/AVX without gambling on fragile LLVM auto-vectorization).

**Proposed design.**
- **`f16` and `bf16` storage/conversion types:** 2-byte `ByteSafe` scalar types with explicit lossless widening to `f32` and explicit documented/rounded narrowing from `f32` (extending `R-0608`'s float observation contract), usable immediately for weight storage and dequantization.
- **Explicit fixed-width SIMD vectors (`Simd<T, N>`):** Portable, deterministic lane-wise operations with explicit width `N` and explicit horizontal reductions (no implicit target-dependent vector width, no hidden fast-math reassociation), lowered cleanly to LLVM vector IR and LCB SIMD instructions.

**Philosophy alignment.** Explicit predictable execution (rule 16, AT 7), bit-exact semantics matching `R-0608` (rule 12), and competitive CPU kernel performance without `unsafe` intrinsics soup (rule 17).

**Roadmap home.** `R-0726` (Phase D, Slice 18 alongside `f32`/`f64` and `concrete-llm`).

**Owner questions required.**
1. `f16`/`bf16` representation, NaN/subnormal handling and rounding rules (extending `R-0608`'s reopened float proposal).
2. Fixed-width SIMD type spelling, admitted `(T, N)` pairs, and deterministic reduction/mask semantics.

---

## Part 2: Compiler, Prover and Tooling Enhancements

### <a id="comp-verified-hints"></a>1. Verified optimizer hints from `CheckedCore` and `Facts` (`R-0711`)

**Motivation.** Today a discharged-evidence token in MIR → BIR simply omits a runtime overflow or bounds trap check. Because `CheckedCore` and `Facts` already establish mathematical proofs that C and C++ compilers only guess via undefined behavior, Concrete can safely emit stronger optimization metadata than C while keeping zero UB.

**Proposed design.**
- **LLVM backend (`R-0503`):** Emit `nuw`/`nsw` on integer `add`/`sub`/`mul`, `!range` metadata on bounded scalars/indices, and `noalias` on disjoint second-class `&`/`&mut` parameters (proved non-aliasing by C5), strictly gated by discharged-evidence tokens and proved `Facts` judgments, never inferred heuristically:
  - Emit `memory(none)` **only** when `Facts` prove empty `with()` **and zero observable memory reads and zero observable memory writes** (excluding not only `&T`, `&mut T`, and raw pointers, but also empty-`with()` functions taking an owned `Heap<T>` by value and reading or overwriting its payload).
  - Emit `memory(argmem: read)` / `readonly` **only** when `Facts` prove empty `with()`, zero observable writes (including no `&mut` and no owned-`Heap<T>` mutation), and reads restricted strictly to immutable `&T` argument memory.
  - Emit `willreturn` **only** when `Facts` prove **both** termination **and** `not may-abort` (since termination and `may-abort` are distinct facts in [decisions.md](../decisions.md#decided-project-no-hidden-traps)); emit `nounwind` strictly from Concrete's proved non-unwinding trap/ABI contract (`R-0616`/`R-0581`, plus foreign-call non-unwinding premises where applicable), not from termination alone.
- **LCB native backend (`R-0517`):** Consume the exact same proved facts for CSE, width narrowing under [Foundation Q7 option (c)](../decisions.md#foundation-q7), dead pure-call elimination, and loop-invariant code motion—gating dead pure-call elimination and speculative LICM strictly on proved zero observable reads and writes, `not may-abort`/trap-free, termination, and loop-invariance / safe-to-speculate evidence.
- **Soundness controls:** Negative controls verify that (1) unproved or `#[may_not_terminate]` sites never receive stronger hints than proved, (2) a pure function reading through `&T` never receives `memory(none)`, (3) an empty-`with()` function mutating an owned `Heap<T>` payload never receives `memory(none)` or `readonly`, and (4) a terminating-but-`may-abort` function never receives `willreturn`, dead-call elimination, or speculative LICM; differential tests (`R-0578`) check observation equivalence with hints enabled versus disabled.

**Philosophy alignment.** Every optimization hint carries a single proved owner in `CheckedCore`/`Facts` (rule 1, rule 5, rule 12) and never turns an unproved fact into an assumption.

**Roadmap home.** `R-0711` (Phase C, alongside `R-0503`, `R-0574`, `R-0575`, `R-0581`).

---

### <a id="comp-bitmask-rules"></a>2. Deterministic bitmask, modulo and shift transfer rules beside octagons (`R-0712`)

**Motivation.** Octagon inequalities (`±x ± y ≤ c`) handle linear bounds well, but systems code constantly relies on bitwise and modulo idioms that octagons alone cannot prove:
- Ring-buffer wraparound (`idx & (CAP - 1) < CAP` or `idx % len < len`)
- Hash-table bucket indexing (`hash & 255 < 256`)
- UTF-8, QOI, PNG, ChaCha20 and EVM bitfield extraction (`(byte >> 4) < 16`, `byte & 0x3F < 64`)
- Overflow-free midpoint (`lo + ((hi - lo) >> 1)`)

Without lightweight deterministic rules for these idioms, programmers would have to attach manual `#[proof(...)]` theorems to routine indexing and bit-packing code.

**Proposed design.** Extend `R-0574`'s decidable, Lean-proved checker (strictly **no SMT** in admission!) with a small closed table of kernel-checked transfer rules alongside octagons:
- Bitmask bounds: `mask ≥ 0 → 0 ≤ (x & mask) ≤ mask` (always satisfied for unsigned operands, and requiring proved `mask ≥ 0` for signed types under `Scalar.bitwise`), plus known-zero/known-one bit tracking.
- Modulo bounds: `x ≥ 0 ∧ d > 0 → 0 ≤ (x % d) < d` (unsigned or proved non-negative dividends only, matching `Scalar.checked .rem`'s `Int.tmod` under `OD114`, where negative dividends preserve sign).
- Right-shift bounds: `(x >> k) ≤ (max(x) >> k)` for unsigned `x` and `k < width`.
- Constant-factor / midpoint identities: `0 ≤ lo ≤ hi → lo ≤ lo + ((hi - lo) >> 1) ≤ hi` (unsigned or proved non-negative bounds, or with additional representability evidence proving `hi - lo` does not overflow signed machine width).

Each transfer rule is a single Lean theorem proved once against `Scalar` (`R-0608`), preserves decidability and determinism, and never introduces solver unpredictability.

**Philosophy alignment.** Preserves [no hidden traps](../decisions.md#decided-project-no-hidden-traps) and zero-SMT admission (rule 18), directly reduces proof noise in real systems workloads (rule 17), and keeps every rule small and kernel-checked (rule 6, rule 15).

**Roadmap home.** `R-0712` (Phase C, Slice 7 arrays/borrows, extends `R-0574`).

---

### <a id="comp-extract-lean"></a>3. Pure-functional Lean extraction (`concrete extract-lean`) pulled to Slice 10 (extends `R-0495`, `R-0177`)

**Motivation.** Proving functional correctness directly over a big-step/small-step operational machine configuration (`Core.eval` with heap, frame stack, fuel, and event trace) is painful for ordinary users and LLMs alike. Because Concrete enforces a clean separable core (pure functions, linear ownership, no hidden globals, second-class references), pure functions behave mathematically like ordinary Lean functions (the Aeneas insight—already noted in `README.md`—achieved natively because Concrete already compiles inside Lean 4).

**Proposed design.** Pull proof-order item 4's pure/separable-core slice forward from Phase E to **Slice 10** (the proof surface, once the owner approves the supported subset, representation, and residual assumptions as required by `R-0495` and `R-0574`):

- Add `concrete extract-lean`, emitting a clean, idiomatic pure-functional Lean model `f_model` for every eligible pure/separable-core Concrete function (starting with pure arithmetic, structs, enums, arrays and counted/variant loops).
- Automatically generate and kernel-check a refinement bridge theorem quantifying over admitted inputs, initial state, and sufficient fuel—showing that `Core.eval` terminates cleanly with an unchanged **erased/observable state projection** (projecting away C5 monotonic instrumentation counters such as `Own.State.nextPlace`, `nextLoan`, and `nextActivation` under C9 ghost erasure `R-0615`, since `Own.bind`, `Own.borrow`, and `Own.enter` advance those counters without resetting them on exit), empty event trace, and return value equal to `f_model` (for example `∀ args st fuel, SufficientFuel f args fuel → ∃ st', Core.eval f args st fuel = .ok (f_model args, st', []) ∧ eraseState st' = eraseState st`)—so users and LLMs write high-level functional correctness proofs (`simp`, `omega`, `grind`, induction) over `f_model` while the compiler composes them automatically into Core-level theorems.
- Keep stateful/open-module/raw-memory functional models in Phase E (`R-0495`, `R-0705`).

**Philosophy alignment.** Directly serves rule 7 (proofs attach to real Concrete code through the semantics, not a parallel hand-written reimplementation) and unlocks practical verification on 0.1+ showcases (`grep`, base64, ChaCha20, EVM opcodes, QOI/PNG).

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

Downstream artifacts that depend **only on `interface_hash`** (caller type/fact/contract checking, non-inlined separate-compilation caller codegen, and interface-only external proof lookup) stay valid across body-only edits, whereas **body-dependent artifacts** (downstream generic specializations, inlined caller codegen, and body-level proofs) record and invalidate on `body_hash` so incremental builds never execute stale specialized or inlined code. Every reused proof still passes `R-0697`'s mandatory Lean kernel check against its exact proposition and dependencies—caching accelerates lookup and avoids redundant re-elaboration/codegen, never bypassing kernel verification.

**Philosophy alignment.** Directly implements [AT 5](../PHILOSOPHY.md#admission-test) (declaration-level dependence) and [AT 12](../PHILOSOPHY.md#admission-test) (separate identities for independently changing surfaces).

**Roadmap home.** Extends `R-0556` (Slice 8 semantic interfaces) and `R-0149` (Phase G incremental builds).

---

### <a id="comp-assurance-diff"></a>7. Semantic assurance diffing (`concrete diff`), `Concrete.toml` ratchets and one-command audit dossier (`concrete build --audit`) (extends `R-0201`, `R-0353`)

**Motivation.** For teams and automated reviewers auditing PRs and releases—especially AI-written code—a textual `git diff` buries the questions an auditor actually cares about: Did a function gain a capability? Start allocating? Add `trusted`/`unsafe`/FFI? Increase its stack or heap bound? Drop a proved theorem to unproved? Meanwhile, Concrete already built world-class assurance machinery for itself (`Premises.lean`, `lean_site.py`, `external.sh` with declaration-fidelity DAG hashing + `con-leche` + `nanoda`) that should be exposed directly to Concrete users as a one-command deliverable.

**Proposed design.** Pull `concrete diff <base> <head>` and an initial `concrete build --audit` dossier forward from Phase F to **Slice 10/11** (keeping full package-scale release bundles and offline verification in Phase F under `R-0201`/`R-0353`):

- **Semantic diff & ratchets (`concrete diff`):** Compare the versioned `concrete facts` JSON outputs (`R-0575`, `R-0593`) of two commits by typed declaration identity (`R-0492`), producing a concise semantic diff of capability changes, allocation changes, trusted/foreign boundary changes, termination/may-abort changes, stack/heap/step bound regressions, and proof-status changes. Support declarative ratchets and explicit composable `[policy]` clauses in `Concrete.toml` (`R-0589`, `R-0576`, mapping 1-to-1 to `concrete facts` with no opaque `"strict"` mode word; `concrete init --strict` may emit explicit lines as a template, [decision](../decisions.md#decided-2026-11-22-explicit-policy-clauses)), including per-function step/complexity budgets so CI automatically refuses unapproved capability expansion, new `trusted` surfaces, or regressed stack/heap/step bounds (such as O(n) → O(n²) regressions in `grep` or `concrete-llm`) without blocking unrelated refactoring—keeping cost limits strictly in `Concrete.toml` policy/ratchets and never as source syntax ([2026-11-22 decision](../decisions.md#decided-2026-11-22-llm-inference-showcase-and-float-order)).
- **One-command user audit dossier (`concrete build --audit`):** Reuse `lean_site`, `Premises` and `external.sh` to emit a self-contained HTML/JSON audit bundle per compiled program: capability call graph, per-site trap-freedom and termination status, final-binary stack & heap bounds, trusted/FFI boundary inventory, linked user Lean theorems, exact trust-premise ledger, and a 1-command `nanoda`/`con-leche` independent replay script.

**Philosophy alignment.** Turns [artifact-first auditability](../PHILOSOPHY.md#the-rules) (rule 14) into both an everyday code-review superpower and a one-command user product.

**Roadmap home.** Extends `R-0201` (diffing, ratchets and initial single-program audit dossier pulled to Slices 10–11; full package audit bundle stays in Phase F) and `R-0353` (offline release-evidence bundle verification).

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

### <a id="comp-verified-map-elites"></a>9. Verified MAP-Elites kernel and schedule superoptimization (`R-0727`)

**Motivation.** Quality-Diversity local search (MAP-Elites + hill climbing, as used to optimize `nanoattention`'s Metal inference kernels) discovers high-performing schedules and kernel implementations across trade-off frontiers—but in unverified languages, a single subtle mutant bug silently corrupts outputs on edge shapes or inputs. Concrete uniquely provides both the **soundness filter** and the **exact behavioral descriptors** needed to make automated kernel/schedule search 100% trustworthy.

**Proposed design.** Build an offline Verified MAP-Elites search driver realizing [ROADMAP.md's "replace an implementation" experiment](../../ROADMAP.md#proposed-acceptance-experiment-replace-an-implementation) for hot kernels (crypto, codecs, `concrete-llm` dequant/routing) and LCB instruction/e-graph schedules (`R-0299`):
- **Hard admission filter:** every candidate variant must pass Concrete's checker plus either a Lean equivalence/refinement proof against the reference specification (`R-0495`) or a proved translation-validation / e-graph replay witness (`R-0299`, `R-0583`). Any unverified or non-equivalent candidate is rejected outright.
- **Diversity grid axes (derived automatically from `concrete facts`):** stack bytes (`R-0575`), peak arena footprint (`R-0595`), step bounds (`R-0574`), and register-spill / binary-size metrics (`R-0550`).
- **Fitness axis:** measured target throughput/latency (`R-0424`), enabling Pareto-optimal verified elites across latency, stack, and memory budgets.

**Philosophy alignment.** Exploits Concrete's artifact-first facts and kernel-checked equivalence (rule 7, rule 12, rule 14) with zero additions to the compiler's trusted computing base.

**Roadmap home.** `R-0727` (Phase E, extending `R-0299`, `R-0495`, `R-0575`, `R-0424`). No language-rule owner decision required.

---

## Part 3: Compiler/Spec Correctness & Scaling Fixes and Proof/CI Velocity Overhaul

### <a id="impl-compiler-spec-fixes"></a>1. Pre-Slice-0 and pre-Slice-3 compiler and spec correctness and scaling fixes (`R-0723`)

**Motivation.** Auditing the existing non-legacy Lean code in `Concrete/` and `ConcreteSpec/` surfaced concrete bugs, a mathematically false planned theorem, and algorithmic scaling cliffs that will block Slice 0 certificates or blow up once Slices 2–6 land unless fixed up front:

1. **Provably false `LLVM.unprint` injectivity plan & `LLVM.emit` defects (`Concrete/Backend/LLVM.lean`, `Concrete/README.md`, `B0a`):** `Concrete/README.md` plans `LLVM.unprint (LLVM.print b) = some b`, which is mathematically false against `LLVM.lean` (`LLVM.print` omits `p.sites` and prints `.add .i8` and `.add .u8` identically). Drop syntactic injectivity in favor of semantic/structured equivalence, make `LLVM.emit : Program → Except String (String × SiteTable)` check `WF` and emit `target triple`/`target datalayout`, and remove silent `args.getD k (.imm .u8 0)` fabrication.
2. **Front-end `decide` & $O(P^2 \cdot |T|^2)$ traps (`Concrete/Lex.lean`, `Concrete/LL1.lean`, `Concrete/Premises.lean`, `lakefile.toml`, `B0a`):**
   - Switch `Lex.decodeLoop` from `termination_by` (well-founded recursion, which breaks kernel `by decide` for Slice-0 certificates) to structural fuel like `skipTriviaAux`; make `scanIdentBody`/`stringBody`/`scanDigits` tail-recursive and preserve UTF-8 byte offsets for spans.
   - Replace `LL1.lean`'s $O(P^2 \cdot |T|^2)$ list-based `conflictFree` (which times out kernel `decide` on real grammars and misses unproductive `A → A` cycles) with a precomputed parse-table verification in $O(P \cdot |T|)$ that also directly supplies `Parse.lean`'s $O(1)$ parse table.
   - Register `Concrete.LL1.ll1_sound` in `Concrete/Premises.lean` and include `"Concrete"` in `lakefile.toml`'s `defaultTargets`.
3. **Two `Concrete/BIR.lean` scaling cliffs (`B0a` / before Slice 3):**
   - Drop `++ env` in `BIR.enter` (currently retains unreachable dead bindings in memory across loop iterations without bound—a memory-retention leak rather than quadratic lookup cost, since `++` traverses only the target block's parameter list and `Block.wf` ensures valid register lookups resolve within the current block's bindings without scanning historical environments; `Block.wf` already enforces block-local scope).
   - Before Slice 3, upgrade `BIR.wf` to validate a certificate-style immediate-dominator table (`idom`, matching `R-0583`), eliminating $O(V \times B)$ phi-parameter explosion against LCB's 488-value / 64-block verified cap.
4. **`Concrete/Backend/LCB.lean`, `Runtime.lean` and shared `Concrete.CFG` (`B0b` / before Slice 2):**
   - Always lower `div`/`rem`/`shl`/`shr` safety guards in `MIR → BIR`, never inside `LCB.lean`, preserving LCB's 1-to-1 block simulation.
   - Replace manual `set4`/`bin_steps` proofs with a generic straight-line `seq_sim` macro-op lemma, skip no-op `sub 0`/`add 0` on unsigned types, precompute `regMap`/`incomingMap` ($O(N^3) \to O(N)$), and fail closed on Linux `mulHigh*` until `Verified.reify` supports it.
   - Replace `Runtime.qbe`'s $O(S)$ linear branch chain with an $O(1)$ indexed `(ptr, len)` site table, and extract a shared `Concrete.CFG` module before writing `MIR.lean` and `Memory.lean` so evaluator/progress/behaviour proofs are not duplicated 3×.
5. **`ConcreteSpec/Spec/` unification traps (`A2i` / before consuming slices):**
   - `Hybrid.lean`: fix `Cell.Projected` and `State.Separated` (currently assuming 1 cell = 1 entire `Mem.Block` with distinct `prov`, which breaks struct fields and array elements), fix unbounded `State.set` list consing, and enable `.structured` reads of `bool`/`Unit` once `ISS-0069`/`ISS-0076` owner dispositions land.
   - `Ident.lean`: fix `KeyConstruct.childScope` inheriting `localPreceding` across unnamed blocks/ifs/loops so inserting one call early in a function does not shift downstream `StableKey`s (with owner sign-off if a registered rule changes).
   - `Exec.lean`: distinguish fresh `bind` from `assign` (currently both emit `Life.bind`), enforce `Fn.params` `Nodup`, merge unused `Ident.PlaceId` with `Hybrid.Place`, and extend `CallState` with borrow frames/view calls so `OwnErasure` covers calls.
   - Clear pre-freeze decision conflicts (`OD111`/`OD147` vs `recovered-59-23` packed structs, `HashMap` `Q2.2`/`recovered-59-19` vs Slice 15, `OD39` vs `recovered-59-22` interfaces, and `ISS-0069`/`0070`/`0073`/`0076` holds) with the owner before Core freeze (`R-0567`).

**Roadmap home.** `R-0723` (Phase B, `B0a`/`B0b` and pre-Slice-3 / `A2i`).

---

### <a id="impl-proof-ci-velocity"></a>2. Proof and CI velocity overhaul without soundness reduction (`R-0724`)

**Motivation.** Measured on `main` (2026-11-22), `ConcreteSpec/Proofs/` is already ~28k LOC (3.6×–4.9× Proof:Spec), nearly half of which (~12k LOC) consists of hand-written `Instance`/`Planted`/`Audit` mutants compiled inside `ConcreteSpec`, `axioms.golden` pins 7,209 lines including internal `_proof_N`/`_simp_N` compiler-generated names, `landing_inventory.lean` is a 2,052-line hand-maintained shadow manifest, and CI/local landings run 5× redundant `lake build`s and 3× pre-push root suites. While *new* governance meta-tooling (`R-0692`, `R-0693`, remaining `R-0710` items) is paused, **simplifying and cutting existing proof/CI drag** is urgent to keep Slices 0–21 fast without sacrificing a single kernel check:

1. **Split `ConcreteSpecEvidence` out of `ConcreteSpec`:** Move all `Instance`/`Planted`/`Audit` fixtures and hand-copied mutant functions out of `ConcreteSpec` into a separate Lake library (never imported by `ConcreteSpec`), and replace hand-copied mutant functions with `R-0699`'s automated mutation harness—cutting exported proof LOC and `con-leche` replay time in half.
2. **Zero-TCB proof tactics in `ConcreteSpec/Tactics/` + `Own.WellFormed` structure refactor (policy change owner-gated):** Keep `ConcreteSpec/Spec/` 100% plain readable Lean, and—subject to owner approval of narrowing `scripts/proofs/trust_ban.sh`'s lexical extension ban strictly to `Spec/` while Lean's kernel, `con-leche` and `nanoda` still check every elaborated proof term—permit a curated `ConcreteSpec/Tactics/` module (`invert_step`, `crush_option`, `StateTransitionPreservesWF`), and refactor `Own.WellFormed` from an unlabeled 7-tuple `∧` into a named `structure`.
3. **Implement provenance-backed `axioms.golden` (`R-0703`) & auto-derive `landing_inventory`:** Implement the owner-decided P/A/G contract in [`scripts/proofs/README.md`](../../scripts/proofs/README.md#target-contract) via `R-0703`'s persistent compiler-produced provenance (never name or regex filtering): retain full-population standard-axiom checking (`P` and `A`) across every constant in `scripts/proofs/axioms.sh`, and pin **all source-named theorems (both public and private)** and their transitive axiom sets in `axioms.golden` while excluding only compiler-generated helper rows; separately, auto-derive `landing_inventory` from Lean environment metadata instead of hand-editing 2,052 lines.
4. **Build once in CI, shard `con-leche`, and reuse SHA-bound pre-push attestations (hook reuse owner-gated):** Share `.lake/build` across proof CI jobs instead of rebuilding 5×, shard/modularize `con-leche` exports as the tree grows, and—subject to owner approval—let `train.sh check` emit a SHA-bound attestation so `train.sh ci` and `train.sh land` avoid repeating the full root suite 3× on the exact same tree while preserving Rule 8 exact-SHA validation.
5. **Proportion rule & tier-2 mechanical defaults (policy change owner-gated):** Clarify blocking review findings strictly as soundness bugs, wrong language semantics, false theorem statements, or broken builds/gates (treating mutant/fixture construction style and helper factoring as non-blocking notes even inside `Proofs/`), and separate tier-1 language decisions (always owner-decided) from tier-2 mechanical defaults with an owner veto list (resolving `ISS-0069`/`0070`/`0073`/`0076`/`0064` cleanly, keeping raw `U+FEFF` BOM refused).

**Philosophy alignment.** Eliminates accidental proof/CI friction (rule 15, rule 17) while preserving 100% of Lean kernel checking, external replay (`con-leche`/`nanoda`) and exact-SHA validation (rule 8, rule 12).

**Roadmap home.** `R-0724` (Phase T, active velocity track alongside `B0a`/`B0b`).

**Owner questions required (for policy items only).**
1. Narrowing `trust_ban.sh`'s lexical `macro`/`elab` ban to `ConcreteSpec/Spec/` while permitting zero-TCB proof tactics in `ConcreteSpec/Tactics/` (kernel + external replay unchanged).
2. Permitting SHA-bound local attestation reuse across `train.sh check` → `ci` → `land` (revising the current no-reuse hook rule in `AGENTS.md`).
3. Adopting the review proportion rule (blocking vs non-blocking notes in `Proofs/`) and tier-2 mechanical-defaults veto workflow (including the five held pre-freeze defaults with BOM kept refused).
