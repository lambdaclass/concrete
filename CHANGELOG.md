# Changelog

Status: changelog

This file tracks major completed milestones for Concrete.

It is intentionally milestone-oriented rather than release-oriented. The project is still evolving quickly, so the useful unit of history is “what architectural or language capability landed,” not tagged versions.

For current priorities and remaining work, see [ROADMAP.md](ROADMAP.md).

## Major Milestones

### Phase H cleanup: zero-alloc borrowed string literals, String.append method, example modernization

**Zero-alloc borrowed string literals:** `&"literal"` now points directly at the global constant — no malloc, no memcpy. Previously, every string literal materialization heap-allocated a copy even when only borrowed. The compiler adds a new `strConstRef` SSA variant that Lower emits for `borrow(strLit)` and EmitSSA translates to a stack-only `%struct.String` referencing the global, with `cap = 0` to signal non-owned.

**String.append and String.append_int methods:** `String` now has `append(&mut self, other: &String)` and `append_int(&mut self, n: Int)` methods in the stdlib. Combined with zero-alloc borrowed literals, string building no longer requires temporary owned strings:

```concrete
// Before: 3 lines per literal append
let mut suffix: String = " ok, ";
string_append(&mut sum, &suffix);
drop_string(suffix);

// After: 1 line, no temp, no drop
sum.append(&" ok, ");
```

**Example modernization:** integrity example went from 38 → 0 `drop_string` calls, verify from 15 → 0. All eliminated drops were cleaning up temporaries forced by the old API. Real owned-value drops remain explicit via `.drop()`.

**Stdlib additions:** `std.sha256` (SHA-256 hashing), `std.hex` (hex encode/decode), `std.ascii` (char classification). `String` gained `starts_with`, `ends_with`, `contains`, `to_lower`, `to_upper`, `clone`, `eq` methods.

**What changed strategically:** the main collection/string ergonomics pain point from Phase H is now addressed at the right level — by eliminating unnecessary ownership churn rather than hiding cleanup. Examples now reflect the intended Concrete style: explicit drops for real ownership, zero-cost borrows for literals.

### Phase H cleanup: match on integers and bools lands as validated language feature

**Integer and bool literal match patterns:** match on integer and bool values now has parser support for negative integer literals (`-1 => ...`) and bool literals (`true => ...`, `false => ...`). Integer match codegen already worked; this change adds the missing semantic validation layer.

**Non-enum exhaustiveness checking:** CoreCheck now requires a default `_` arm for non-enum matches (integer, char, etc.). Bool matches are exempt when both `true` and `false` arms are present. Previously, missing a default arm compiled silently and fell through to `unreachable` at runtime.

**Regression coverage:** added `match_int_basic.con`, `match_int_default.con`, `match_int_negative.con`, `match_bool.con` (positive), and `error_match_int_no_default.con`, `error_match_bool_no_false.con` (negative).

**What changed strategically:** "match on integers" moves from roadmap cleanup to landed feature. The remaining literal-pattern scope (char, float, string) stays out of scope until evidence demands it.

### Phase H cleanup: impl-method visibility hardened, paper workflow added, remaining cleanup narrowed

**Impl-method visibility fix:** private impl methods and private trait-impl methods are no longer callable across module boundaries via method syntax and no longer leak through imports. Four summary/resolution/elaboration paths now consistently respect `isPublic`: `buildFileSummary` public-name construction, `resolveImports` struct-method exposure, sibling impl-signature injection in Elab, and submodule impl-signature injection in Resolve.

**Regression coverage:** added `error_private_impl_method.con`, `error_private_trait_impl_method.con`, and `pub_impl_method.con` so the privacy boundary is exercised in both negative and positive form.

**Paper workflow:** added a nix-backed Typst paper flow under `paper/`, including `make paper`, dev-shell Typst support, generated-PDF ignore rules, and an initial serious project paper draft. This does not change compiler semantics, but it does turn the paper into a reproducible artifact in the repo instead of a side document.

**What changed strategically:** one more concrete Phase H cleanup bug is now closed, and the remaining cleanup tail is narrower and clearer. The next cheap semantic win is not “add match on integers from scratch,” but finish literal-pattern validation and exhaustiveness policy for the already partially working integer-match path.

### Phase H hardening: Bug 018 fixed, method-level generics land, project examples modernized

**Bug 018 fixed:** stack-array borrows used for writable FFI paths no longer create stale-copy behavior. Array borrows now retype directly instead of going through the cast path that triggered redundant alloca+store emission. `std.net` and the HTTP example can use stack arrays again instead of heap-buffer workarounds.

**Promoted alloca handling:** lowered array locals now skip the invalid load path in loop/promoted-allocation cases, which closes the remaining stack-array correctness path exposed during the HTTP/server work.

**Method-level generics:** generic methods now work end-to-end, including correct generic `Self<T...>` handling and distinct LLVM struct emission per concrete instantiation. This unlocked `fold<A>` on `Vec`, `HashMap`, and `HashSet` as ordinary stdlib methods rather than one-off APIs.

**Project example modernization:** the project-based examples (`integrity`, `verify`, `kvstore`, `lox`, `toml`) were migrated from the old free-function `vec_*` style to the `Vec` method API where the semantics are a clean fit. `kvstore` intentionally keeps `vec_get` / `vec_set` in the swap-remove path because it still relies on the current shallow-copy behavior there for linear `String` values.

**What changed strategically:** Bug 018 was the last remaining open correctness bug directly carried forward from Phase H. The remaining open work is no longer “fix the language enough for real programs,” but Phase J package/workspace maturity, testing/tooling refinement, and evidence-gated follow-up questions.

### Phase H findings closure: buffered I/O, std.args, collection iteration

**Buffered I/O for compiler print builtins:** Switched the compiler's print builtins (`print_string`, `print_int`, `print_char`, `print_bool`) from raw `write()` syscalls to buffered libc I/O (`printf`/`putchar`). Lower-level stdlib I/O helpers in `std.io` still expose direct/unbuffered `libc_write`-based behavior where appropriate. The grep case-insensitive benchmark gap dropped from 2.8x to 1.7x vs C. Remaining grep gap is per-character `to_lower` cost, not I/O overhead.

**`std.args` module:** Added `std.args` with `count() -> Int` and `get(idx: Int) -> String` for process argument access. `cgrep` and `conhash` examples converted from raw extern fn declarations to stdlib imports. Test-mode stubs emit `__concrete_get_argc` (returns 0) and `__concrete_get_argv` (returns null) so stdlib tests compile without the main wrapper.

**HashMap iteration:** Added `for_each(&self, f: fn(&K, &V))`, `keys(&self) -> Vec<K>`, and `values(&self) -> Vec<V>` methods to `HashMap` with stdlib test coverage (`test_map_for_each`).

**Text/interpolation decision:** Six Phase H programs written without string interpolation. `print`/`println` plus builder APIs are sufficient for current needs. Interpolation deferred until sustained evidence from larger programs justifies it.

**What changed strategically:** Three of the five remaining open Phase H items are now closed (grep I/O bottleneck, runtime argument surface, text/interpolation decision). Collection iteration is in progress. The center of gravity is now moving toward Phase J package/workspace maturity.

### Qualified module access fully closed

**Same-name collision fix:** Submodule function definitions are now renamed at elaboration time via `prefixModuleFnNames` (e.g., `add` in `math` → `math_add`), with cross-module call site rewriting via `crossModuleRenames`. This is consistent across all downstream passes (mono, lower, emit). Ambiguity detection ensures that two submodules defining the same leaf name require qualified access (`math::add` / `util::add`) rather than silently colliding.

**Inline sibling `::` access:** `mod A {} mod B {}` can now use `A::fn()` from `B` without explicit `import` statements. Sibling module function signatures are injected consistently through Resolve, Check, and Elab passes, with linker aliases bridging qualified call names to bare definitions.

**What changed strategically:** The two remaining qualified-access limitations (same-name collisions and inline sibling `::`) were the last namespace gaps blocking multi-module code from feeling natural. Both are now closed. The remaining open findings are narrower: text construction/interpolation, runtime-oriented collections, runtime argument surface, and runtime/stack-pressure classification.

### Phase J groundwork: package/project workflow hardening lands

**Builtin std resolution:** std is now resolved automatically relative to the compiler binary instead of requiring repo-relative path dependencies in user manifests. `CONCRETE_STD` provides an override for unusual setups. Example `Concrete.toml` files no longer need `std = { path = ... }`.

**`concrete run`:** Added a first clean project-facing run flow. It builds to a temporary binary, executes it with inherited stdio, forwards `-- args...`, and cleans up without extra “built” noise.

**`concrete test`:** Added a package-aware test flow with builtin std resolution, dependency loading, test binary execution, cleanup, and `--module` filtering.

**Workflow diagnostics:** Missing `Concrete.toml`, missing `src/main.con`, and bad dependency paths now produce more actionable package/workflow errors instead of vague failures. Missing builtin std now produces a clearer warning with a hint about `CONCRETE_STD` or an explicit dependency path.

**What changed strategically:** Package mode is now a real workflow, not just a roadmap intention. The next priorities move to text/output direction, verifier polish, testing-tooling refinement, workspace support, and incremental/package graph work.

### Phase H: vec builtin inlining removes the VM gap to C

**VM hot-path investigation:** The first bytecode VM benchmark initially showed Concrete about 3.4x slower than the comparable C heap-`Vec` implementation. Investigation found the main cause was not bounds checking or an inherent safe-collection tax, but missing inlining on tiny vec builtins in the hot dispatch loop.

**LLVM alwaysinline support:** Added `alwaysInline` to `LLVMFnDef`, taught `EmitLLVM.lean` to emit the `alwaysinline` attribute, and marked vec builtins accordingly. This lets LLVM optimize across repeated `vec_get`, `vec_set`, `vec_push`, `vec_pop`, and `vec_len` calls in runtime-heavy loops.

**Benchmark result:** On the VM `fib(35)` workload, Concrete improved from ~785ms to ~257ms after the inlining fix, matching the comparable C heap-`Vec` implementation at ~260ms and remaining far faster than Python.

**What changed strategically:** The VM result no longer supports the claim that Concrete currently pays a large unavoidable abstraction cost for safe collections. The first major runtime-heavy performance cliff was a backend/codegen policy issue, and fixing it materially strengthened the Phase H performance story.

### Phase H: cgrep proves JSON was not a one-off performance result

**`cgrep` example:** Added `examples/grep/main.con`, a ~220-line grep-like tool supporting `-n`, `-c`, `-v`, `-i`, multiple files, filename prefixes, and error reporting for missing files.

**Runtime argv support:** Added runtime access to process arguments through `__concrete_get_argc()` / `__concrete_get_argv(idx)` with globals populated from the generated C `main`. This required mutable LLVM globals support (`LLVMGlobal.mutable`) so user code can consume command-line arguments without handwritten C shims.

**Benchmark result:** On a 13MB, 200k-line text workload searching for `error`, Concrete `cgrep` is competitive with mainstream tools:

- Concrete count-only: ~35ms
- Python count-only: ~34ms
- macOS grep count-only: ~83ms
- Concrete with output: ~95ms
- macOS grep with output: ~88ms

The important result is not the exact ranking. It is that the earlier JSON parser win at `-O2` generalizes to a structurally different text/streaming workload rather than being a parser-specific accident.

Later rebuilds after the mixed-arg `print` / `println` work exposed a more nuanced picture:

- verify, JSON, VM, and policy-engine style workloads still sit roughly in the same performance class as the comparable C baselines on the current measurements
- `cgrep` is now slower than the native C comparison on rebuilt runs, pointing to string/output handling as the current bottleneck rather than a broad backend regression

That makes grep a useful continuing pressure test for text/output and string-I/O costs even though the overall Phase H performance story remains strong.

### Phase H: codegen fixes, -O2 default, JSON benchmark proves competitive performance

**Alloca hoisting (Bug 013):** All `alloca` instructions now emitted in function entry block via `entryAllocas` field in EmitSSAState. Previously, allocas inside loop bodies grew the stack every iteration, causing stack overflow at ~130k iterations in recursive parsers.

**String literal in loop (Bug 014):** Added `.strConst` case to `ensureValAsPtr` in EmitSSA.lean. String literal assignment inside loops previously generated invalid LLVM IR (global symbol used as struct value).

**-O2 default (Bug 015):** Clang invocations now pass `-O2` for both regular and test compilation. This is a 3.6x parse speedup on the JSON benchmark (145ms → 40ms) because LLVM inlines per-character builtins like `string_char_at` and `string_push_char`.

**`string_reserve` builtin:** Added `string_reserve(&mut String, cap)` across all compiler passes. Pre-allocates string capacity to avoid repeated reallocs during bulk construction.

**`Bytes.to_string()`:** Zero-copy ownership transfer from Bytes to String in std/src/bytes.con (identical struct layout, no copy needed).

**JSON benchmark result:** Concrete parses 9.3MB JSON in 40ms at -O2, matching or slightly beating Python's `json.loads` (46ms). Earlier 185ms measurements were -O0 artifacts. Full breakdown in `research/workloads/phase-h-findings.md`.

### Phase H: scope-aware defer semantics and control-flow cleanup coverage

**Scoped defer semantics:** `defer` now lowers as true scope-exit cleanup rather than a flat function-scoped approximation. Deferred calls run at block exit, loop-iteration exit, `break`, `continue`, early return, and implicit function end.

**Checker fix:** deferred calls now reserve consumed variables generally instead of handling only `destroy(...)`. This lets any consuming call used in a `defer` body reserve its arguments until the deferred execution point.

**Regression coverage:** added control-flow tests for block scope, loop iteration, `break`, `continue`, consuming-call LIFO behavior, and linear-reuse errors under `defer`.

**Current tradeoff:** cleanup code is emitted independently at exit sites. This is intentionally simple and correct; if real programs later show meaningful IR bloat, cleanup outlining into shared blocks can be revisited as a backend optimization.

### Phase H: builder builtins, JSON parser, cleanup ergonomics, future feature research

**Builder builtins:** Added `string_append_int(&mut String, Int)` and `string_append_bool(&mut String, bool)` builtins for zero-grammar-cost mixed-type string building. These avoid intermediate allocations and interpolation syntax while keeping capabilities explicit. Design rationale in `research/stdlib-runtime/text-and-output-design.md`.

**Bug 010 semantics fix:** `string_substr(s, start, len)` was previously aliased to `string_slice(s, start, end)` despite different contracts. Now has its own intrinsic ID and LLVM implementation that computes `end = start + len` before delegating to `string_slice`.

**JSON parser:** Added `examples/json/main.con` — ~450-line recursive-descent JSON parser + validator. Covers objects, arrays, strings (with escapes), integers, booleans, null. Includes comprehensive test harness: primitives, whitespace handling, nested structures, invalid input rejection. Passes at O0 and O2. First sustained pressure test of the builder builtins approach and linear string ownership at scale.

**Cleanup ergonomics design:** Documented 5 options for reducing linear ownership friction in `research/language/cleanup-ergonomics.md`. Immediate priorities: (1) `defer` statement for scope-end cleanup, (2) additional mutation-oriented string APIs. Deferred: Destroy trait, scoped helpers, borrowed slices. Roadmap updated with all items and revisit triggers.

**Future feature research:** Six research notes analyzing difficulty and design for features that multiply Concrete's value beyond capabilities:
- `research/stdlib-runtime/allocation-budgets.md` — NoAlloc/BoundedAlloc sub-capabilities; report-only classification (1-2 days) → enforcement (1-2 weeks) → byte-level budgets (3-4 weeks)
- `research/stdlib-runtime/arena-allocation.md` — bump-pointer arenas replacing manual Vec pools; ~1 week; simpler than Vec (no realloc)
- `research/stdlib-runtime/execution-cost-tracking.md` — structural boundedness reports (1-2 days) → abstract cost counting (2-3 weeks) → WCET (external tool)
- `research/language/typestate.md` — ownership-based two-state works today; phantom types for multi-state (2-3 weeks) deferred pending evidence
- `research/packages-tooling/authority-budgets.md` — updated with module-level `#[authority(...)]` path (~1 week); package-level deferred to Phase J
- `research/stdlib-runtime/layout-reports.md` — padding visualization, enum layout, ABI flags; 3-4 days; pure report formatting

### Phase H bug fixes: Bug 005, 008, 009 fixed; if-expression, const lowering, enum-in-struct

**Bug 008 — If-else expression:** If-else now works as an expression (`let x: i32 = if cond { 10 } else { 20 };`). Added `ifExpr` to AST/Core, `parseExprBlock` in parser, elaboration with hint propagation, and lowering using alloca+condBr+store+load with type casts. Changes across 10 files (AST, Core, Parser, Elab, Check, Lower, Format, Resolve, CoreCanonicalize, Mono, CoreCheck).

**Bug 009 — Const lowering:** Constants now inline correctly. Added `constants` field to `LowerState`, `collectAllConstants` helper, and constant lookup in `lowerExpr` `.ident` handler. `examples/constants.con` now compiles.

**Bug 005 — Enum-in-struct:** Confirmed fixed (layout engine handles enum fields in structs correctly).

**Bug 007 — Standalone print:** Added `print_string(&String)`, `print_int(Int)`, `print_char(Int)` as compiler builtins requiring `Console` capability. Uses `write(2)` syscall. User-defined functions with the same names take precedence.

**Bug 010 — Substring extraction:** `string_slice(s, start, end)` and `string_substr(s, start, len)` now exist as distinct operations with correct semantics. `string_substr` computes `end = start + len` and delegates to `string_slice`.

**Bug 011 — Loop string building:** Added `string_push_char(&mut String, Int)` and `string_append(&mut String, &String)` builtins with in-place mutation via `&mut`, analogous to `vec_push`. Works naturally in loops without fighting linearity.

**Bug 012 — Standalone timing:** Added `clock_monotonic_ns() -> Int` builtin requiring `Clock` capability. Returns nanoseconds from monotonic clock via `clock_gettime`.

**Builtin deduplication:** Builtin LLVM function definitions and declarations now skip names already defined by user code or extern declarations, preventing redefinition errors.

**MAL interpreter:** ~1150-line Make A Lisp interpreter (`examples/mal/main.con`) with linked-list environment (O(n) lookup), symbol interning, cons cell pool. Benchmarks show Concrete MAL is ~73x faster than Python MAL at -O2. Includes comparison benchmarks against Python native and C native.

### By-value repr(C) struct FFI and testing infrastructure: 891 tests, 0 failures

**Compiler fix — struct FFI ABI flattening:** `#[repr(C)]` struct parameters in extern fn calls are now flattened to integer registers per the ARM64 C ABI (≤8 bytes → i64, 9-16 bytes → two i64s), matching clang's calling convention. Target triple and datalayout emitted in LLVM IR. Previously, small structs were passed as LLVM aggregates, which didn't match the C register-passing convention across FFI boundaries.

**Bug 004 fixed:** `arr[i] = val` with runtime variable index used the value's type instead of the array element type for GEP/store, causing wrong offsets and store widths. One-line fix in Lower.lean.

**Testing infrastructure:**
- Cross-target IR verification: 25 programs verified to compile for x86_64 via `clang --target`
- Mutation testing (`test_mutation.sh`): 18 targeted mutations across 7 compiler files (Layout, Shared, Check, CoreCheck, Lower, EmitSSA, SSAVerify) — apply, rebuild, test, measure gap
- Fuzz testing expanded: 7 new generators (enum/match, nested struct, fn pointer, borrow, defer, non-exhaustive match, missing capability)
- Performance regression check integrated into `run_tests.sh --full`

### Phase 3 system-level testing

Added ~100 new tests (Phase 3) on top of Phase 2's 766.

**Wave 1 — Type system, codegen, capabilities, modules (44 tests):**
- Type system soundness: generic chains, recursive enums, nested match exhaustiveness, linearity branch agreement, trait multi-bound, defer linearity
- Codegen edge cases: integer overflow wrap, nested struct access, nested loops, large struct pass, cast chains, early return from loops, many locals, recursive fibonacci
- Capability/trusted: capability subset chains, capability polymorphism, trusted impl methods, trusted extern calls, and 5 error tests for capability violations
- Cross-module/parser: nested modules, struct methods across modules, enum match across modules, reexport types, deeply nested expressions

**Wave 2 — ABI/FFI, proof boundary, optimization (19 tests):**
- ABI/FFI: repr(C) nested structs, function pointer call chains, function pointers in structs, sizeof basic types, array bounds, pointer round-trips, and 2 error tests
- Proof boundary: 11 `check_report` assertions verifying exact `--report proof` output — eligible function marking, exclusion reasons (capabilities, trusted boundary), and totals
- Optimization: dead code after return, unused variables, constant folding, branch same value, loop invariant, deeply nested return, zero/single iteration loops
- O2 regression: 8 new `-O2` variants for optimization-sensitive tests

### Phase G complete: Language Discipline, Design Policy, and Provable Subset

All six Phase G items complete. Concrete now has explicit feature-admission criteria, recorded language decisions, documented long-term shape commitments, and a defined provable subset.

**Item 6 — Provable subset definition**: Created `docs/PROVABLE_SUBSET.md` as the standing reference. Defines the current ProofCore extraction boundary for proof-eligible functions (empty capability set, not trusted, not entry point, no trusted impl origin) and types (no repr(C)/packed, no builtin override), and distinguishes it from the stricter `--report proof` heuristic that also flags extern calls and raw-pointer operations. Documents pipeline position (extract from ValidatedCore after CoreCheck), current proof coverage (17 theorems over integers/booleans/arithmetic/conditionals), relationship to the high-integrity profile, and how permanent language decisions (no closures, no trait objects, static dispatch) make the subset boundary clean.

**Item 1 — Feature admission criteria**: Created `docs/DESIGN_POLICY.md` as standing policy. 10-point admission checklist (simple invariant, visibility, phase separation, declaration-level dependencies, static dispatch, predictable codegen, diagnostics ownership, single-pass ownership, proof story, benefit for audited code). Quick decision rule and one-line test. Promoted from `research/design-filters.md`.

**Item 2 — "No" and "not yet" decisions**: Created `docs/DECISIONS.md` as a decisions registry. Six permanent decisions: no closures, no trait objects, no source-generating macros, no hidden dynamic dispatch, no inference-heavy abstraction, trusted = pointer containment only. Six deferred decisions with explicit prerequisites: freestanding mode, capability hiding, concurrency, pre/post conditions, derived equality, package model.

**Item 5 — Long-term language shape**: Created `docs/LANGUAGE_SHAPE.md` documenting six structural commitments (static/explicit dispatch, capabilities in signatures, three-way trust split, linear ownership, whole-program monomorphization, phase separation), five "will not become" constraints, and a table of what may change with evidence. Synthesizes IDENTITY.md, DESIGN_POLICY.md, DECISIONS.md, and SAFETY.md.

### Phase G items 3–4: Language Surface Simplification and Trusted Narrowing

Two Phase G items landed, simplifying the language surface and tightening the trusted model.

**Item 3 — Syntax simplification**:
- Removed `main!()` / `fn name!()` bang sugar entirely: parser no longer accepts `!` after function names, `hasBang` field removed from `FnDef` in AST.lean, Format.lean no longer emits `!`. 70+ `.con` files migrated to explicit `with(Std)` or `with(Alloc)`.
- Fixed 5 pre-existing test failures (`complex_recursive_list`, `complex_recursive_tree`, `complex_recursive_mutual`, `heap_deref_recursive`, `option_heap`) that still used `fn name!()` syntax.
- Added `union_basic.con` test exercising union creation and trusted field access.

**Item 4 — Trusted narrowing**:
- Removed the loop-linear exception from `trusted`. Previously, `isTrustedFn` in Check.lean's TypeEnv let trusted functions consume linear variables inside loops, bypassing the loop-depth check. This was the only non-pointer-related privilege `trusted` granted, and it muddied the semantics.
- `trusted` now means exactly one thing: **audited pointer-level containment**. The four operations it permits (pointer arithmetic, raw pointer dereference, raw pointer assignment, pointer casts) all relate to pointer safety. No linearity, no capabilities, no other special treatment.
- The three-way model is now sharper: `with(Cap)` = semantic effects, `with(Unsafe)` = foreign boundary authority, `trusted` = reviewed pointer containment.

What changed:
- `Concrete/AST.lean`: removed `hasBang` field from `FnDef`
- `Concrete/Parser.lean`: removed `!` sugar parsing from `parseFnDef` and `parseFnDefOrDecl`
- `Concrete/Format.lean`: removed `bangStr` emission
- `Concrete/Check.lean`: removed `isTrustedFn` from TypeEnv, loop-depth check now applies uniformly

Test suite: 766 tests passing, 0 failures.

### Phase F items 1–3, 7 complete: Capability and Safety Productization

Four Phase F items landed, covering capability ergonomics, reporting, aliases, and error recovery.

**Item 1 — Capability error hints**: All capability-related errors in `Check.lean` and `CoreCheck.lean` now include actionable `hint:` text. `missingCapability` suggests `with(Cap)` on the calling function or a trusted wrapper. `insufficientCapabilities` suggests the same. `cannotInferCapVariable` explains explicit capability binding. Pointer/alloc operation errors suggest specific capabilities (`with(Unsafe)`, `with(Alloc)`).

**Item 2 — Authority and proof reports**: Two new `--report` modes implemented in `Report.lean`:
- `--report authority`: transitive authority analysis per capability with BFS call-chain traces through the call graph
- `--report proof`: ProofCore eligibility analysis — marks each function as eligible or excluded with specific reasons (capabilities, trusted, extern calls, raw pointers)
15 semantic assertions in `run_tests.sh`. Total report modes: 8 with 59 assertions.

**Item 3 — Capability aliases**: New `cap IO = File + Console;` syntax at module level. Parsed by the parser, expanded at parse time via `Module.expandCapAliases`, transparent to Check/Elab/CoreCheck. Validates cap names at definition time; supports `Std` macro and `pub cap`. Authority wrapper patterns documented in `docs/FFI.md` with stdlib examples.

**Item 7 — Bounded semantic error recovery**: `checkStmts` (Check.lean) and `elabStmts` (Elab.lean) now catch per-statement errors, restore the type environment on failure, and add placeholder types for failed let-declarations to prevent cascading errors. All accumulated diagnostics are thrown together. Statement-level granularity avoids guessing at expression-level placeholders while catching independent errors.

What changed:
- `Concrete/AST.lean`: `CapAlias` structure, `CapSet.expandAliases`, `Module.expandCapAliases`
- `Concrete/Parser.lean`: `cap Name = Cap1 + Cap2;` parsing at module level
- `Concrete/Pipeline.lean`: alias expansion in `Pipeline.parse`
- `Concrete/Check.lean`: per-statement error recovery in `checkStmts`; consumes `ResolvedProgram`; capability error hints
- `Concrete/Elab.lean`: per-statement error recovery in `elabStmts`; consumes `ResolvedProgram`
- `Concrete/CoreCheck.lean`: capability error hints
- `Concrete/Report.lean`: `authorityReport` and `proofReport` functions
- `Main.lean`: authority/proof report dispatch
- `docs/FFI.md`: authority wrapper patterns, capability aliases
- `docs/PASSES.md`: error accumulation, cap alias expansion, pipeline signature fixes
- `docs/DIAGNOSTICS.md`: statement-level accumulation policy
- `docs/ARCHITECTURE.md`: Parse cap alias expansion, Check error accumulation

Test suite: 685 tests passing (7 new: 4 error recovery, 3 capability alias).

### Phase F items 4–6 complete: Coherent Safety Story and High-Integrity Profile

**Item 4 — Safety usability**: Covered by the combination of capability aliases (item 3), error recovery (item 7), actionable error hints (item 1), and wrapper pattern documentation. Safety features are now easier to use correctly without weakening honesty.

**Item 5 — Coherent safety story**: Created `docs/SAFETY.md` as the central safety reference. Defines the three-way split (capabilities / trusted / `with(Unsafe)`), documents all 8 report modes with what each shows, explains the error model with accumulation policy, describes the proof boundary and ProofCore eligibility, and introduces the high-integrity profile direction. Cross-references added from all existing docs: VALUE_MODEL.md, STDLIB.md, IDENTITY.md, DIAGNOSTICS.md, EXECUTION_MODEL.md, ARCHITECTURE.md, FFI.md, PASSES.md. Stale `ABI_LAYOUT.md` references replaced with `ABI.md`.

**Item 6 — High-integrity safety profile**: `docs/SAFETY.md` defines the profile direction: same language under stricter restrictions (no Unsafe, no unrestricted FFI, no/bounded allocation, no ambient authority growth, analyzable concurrency, stronger evidence). Documents what the compiler must provide (profile-recognized restrictions, profile-aware reports, package visibility, proof relation). Connects profile restrictions to existing features (capabilities gate authority, trusted contains unsafety, linearity ensures resource safety, ProofCore extracts the provable fragment, reports make boundaries visible).

Phase F is now complete. All 7 items done.

### Phase E complete: Runtime and Execution Model

Phase E is done. All 11 items are complete. `docs/EXECUTION_MODEL.md` is the central reference.

**Items 6–11 (new this milestone):**

- **Item 6 — Target/platform support policy**: Three-tier support model (Tier 1: x86_64-linux, aarch64-darwin; Tier 2: x86_64-darwin; Experimental: everything else). Documents what "supported" means, what is target-dependent, and what is not yet validated empirically.
- **Item 7 — Stdlib execution model alignment**: Full module-to-layer mapping (Core/Alloc/Hosted) with capabilities and host dependencies for all 24 stdlib modules. `docs/STDLIB.md` updated with execution model alignment section.
- **Item 8 — Execution profiles**: Documents planned profiles (`no_alloc`, `bounded_alloc`, `no_unsafe`, `no_ffi`, `high_integrity`), how they map to the existing capability system, and their relationship to `ProofCore` eligibility.
- **Item 9 — Performance validation direction**: Documents principles (representative workloads, compilation time matters, observability over cleverness), metrics, regression thresholds, and future CI integration.
- **Item 10 — Verified FFI envelopes and structural boundedness**: Documents FFI envelope direction (mechanical checking of extern fn contracts), structural boundedness properties (allocation-free, stack-bounded, terminating), and how they connect to existing report infrastructure.
- **Item 11 — Concurrency direction**: Documents design principles (explicit, structured, threads-first, capability-gated), the first concurrency model (OS threads, spawn/join, channels, move ownership), 5-stage plan, and what to avoid (Rust-style async fragmentation, hidden executors).

### Phase E items 4–5: FFI ownership boundary and ABI calling convention

**Item 4 — FFI/runtime ownership boundary**: `docs/EXECUTION_MODEL.md` now documents how ownership, capabilities, and resource tracking interact at the FFI boundary. Extern functions require `Unsafe`; `trusted fn` wrappers hide `Unsafe` behind safe APIs. Linear types consumed by-value in extern calls; references borrow without consuming; raw pointers are Copy with no tracking. Known gaps documented: raw pointer leaks, no verified FFI envelopes, no cross-language ownership protocol.

**Item 5 — FFI/ABI calling convention fix**: `EmitSSA.lean` now distinguishes extern fn calls from internal calls. `#[repr(C)]` struct arguments in extern fn calls are passed by value per the C ABI instead of always by pointer. New helpers: `externParamTyToLLVMTy` and `isReprCStruct` detect repr(C) structs and emit by-value passing for extern calls while preserving pointer-based passing for internal calls.

### Phase E items 1–3: execution model and abort-on-OOM

`docs/EXECUTION_MODEL.md` defines Concrete's execution model covering three Phase E items:

**Item 1 — Hosted vs freestanding model**: Concrete targets hosted (POSIX + libc) only. The stdlib is classified into three layers by host dependency: core (pure computation, no libc), alloc (malloc/realloc/free only), and hosted (full POSIX libc). Freestanding mode is a future milestone — the hosted boundary is now explicit so the split is straightforward when needed.

**Item 2 — Runtime boundary**: There is no Concrete runtime. No global constructors, no GC, no module init, no thread-local setup. Programs start in a compiler-generated `main` that calls `user_main`, optionally print the result, and return 0. Failure is explicit through return types — no panic, no unwind. All external symbol dependencies are enumerated (always-required: malloc/free/printf/memcpy/etc.; conditionally-required: fs/net/process symbols from stdlib imports).

**Item 3 — Memory/allocation strategy**: All heap allocation goes through libc malloc/realloc/free. Allocation is capability-tracked via `Alloc`. Deallocation is explicit via linear ownership + `defer`. **Abort-on-OOM is implemented** at both layers: compiler builtins pipe all malloc/realloc through `__concrete_check_oom` (null-check + abort), and stdlib wrappers in `std/src/alloc.con` (`heap_new`, `grow`) null-check and call `abort()` on failure. Future directions: bounded allocation profiles, allocator parameters (Zig-style), no-alloc mode for freestanding.

### Runtime/concurrency roadmap split clarified

The roadmap now separates:

- **Phase E**: the first explicit runtime/execution model and initial thread/channel concurrency stance
- **Phase J**: the later long-term concurrency phase for structured concurrency, threads-plus-message-passing as the base model, and evented I/O as a specialized later runtime

Research notes now include:

- `research/stdlib-runtime/concurrency.md` for the near-term Phase E direction
- `research/stdlib-runtime/long-term-concurrency.md` for the long-horizon layered concurrency target

This makes the sequencing explicit: define the runtime boundary first, then broaden concurrency only after runtime, safety, package, and operational foundations are stable enough to support it well.

### Compiler improvement checklist items 4 & 5 complete

The final two partial checklist items are now done, completing the compiler improvement checklist (all 6 items except backend plurality, which is Phase E+ work).

**Item 4 — Post-cleanup SSA verification**: `Pipeline.lower` now runs `ssaVerifyProgram` both before and after `ssaCleanupProgram`. This mechanically guarantees that cleanup transformations (dead block elimination, trivial phi folding, empty block folding, constant folding, strength reduction, store-load forwarding) preserve all 8 SSA invariants (dominance, phi correctness, no aggregate phis, branch safety, unique defs, call arity, return coverage, type consistency). Previously verification ran only pre-cleanup — cleanup output was trusted by construction but not mechanically checked.

What changed:
- `Concrete/Pipeline.lean`: second `ssaVerifyProgram` call after cleanup
- `Concrete/SSAVerify.lean`: module docstring updated to document dual verification; `isAggregateType` comment explains why generic heap types (Vec, HashMap, etc.) are excluded from the aggregate check
- `docs/PASSES.md`: pipeline diagram, SSAVerify section, and invariant chain updated to reflect post-cleanup verification

**Item 5 — Builtin extraction from EmitSSA**: 568 lines of builtin LLVM IR generation extracted from `EmitSSA.lean` into `Concrete/EmitBuiltins.lean`. The new module exports `getBuiltinFns` (string ops, conversion ops) and `getVecBuiltinFns` (vec ops per element size) and imports only `Concrete.LLVM` and `Concrete.Layout` — no dependency on SSA IR, Core IR, or `EmitSSAState`. This proves the builtins are structurally decoupled from the SSA→LLVM translation. `EmitSSA.lean` shrinks from 1642 to 1099 lines.

Test suite: 663 tests passing, 0 failures.

### Compiler hardening pass complete (all 5 items)

- **Lower.lean hard errors**: 6 silent defaults converted to `throw` — `lookupStructFields`, `fieldIndex`, `variantIndex`, `variantFields`, `structNameFromTy` propagate errors through `LowerM`. `lowerModule` returns `Except String SModule` — failed function lowering is now a compile error, not silently dropped.
- **Layout.lean/EmitSSA.lean hard errors**: all 7 `dbg_trace` fallback defaults converted to `panic!` (6 in Layout, 1 in EmitSSA). Root cause fixed: generic struct/enum definitions survived monomorphization with unsubstituted type variables. Fix: `substStructTypeArgs` added to Layout (parallel to existing `substEnumTypeArgs`), applied in `tySize`, `tyAlign`, `fieldOffset`. `enumPayloadOffset` now accepts `typeArgs`; concrete args threaded from Lower.lean. `variantFields` substitutes type args before returning fields. EmitSSA scans function types for concrete instantiations and emits substituted type defs instead of skipping generic defs. Newtypes erased in imported function signatures at module boundaries.
- **Integer inference**: vec intrinsic hint propagation + SSAVerify `intBitWidth` check catches `i32 + i64` mismatches at the backend gate.
- **Borrow checker audit**: multiple shared borrows, sequential &mut, borrow-of-field all verified working.
- **Cross-module type aliases and newtypes**: fixed pre-existing bug — type alias names leaked through function signatures. `buildFileSummary` now resolves aliases in fn/extern/impl signatures. `resolveImports` resolves aliases and erases newtypes in imported signatures. `Elab.elabFn` resolves aliases in function parameter types.

5 hardening tests added. Test suite: 663 tests (184 stdlib). All hardening items complete — no remaining silent fallback defaults in the compiler pipeline.

### 3 compiler bugs fixed

Three bugs discovered during integration test writing, now fixed with regression tests and documentation in `docs/bugs/`:

- **Bug 001 — cross-module struct field offset** (`Elab.lean`): all fields of a struct defined in another module read as offset 0. Imported struct definitions were excluded from `CModule` output, so `Layout.fieldOffset` couldn't find them and silently returned 0. Fix: include imported structs in CModule.
- **Bug 002 — i32 literal type mismatch** (`Elab.lean`): `0 - a` where `a: i32` generated `sub i64 0, %i32_val`. Integer literals defaulted to i64 regardless of the other operand's type. Fix: when one operand is a default-typed literal and the other has a concrete smaller integer type, re-elaborate the literal with the concrete type.
- **Bug 003 — cross-module &mut borrow consumed as move** (`Check.lean`): passing `&mut Vec<T>` to a function consumed the variable, preventing reuse. The checker didn't distinguish owned from reference parameters when consuming arguments. Fix: skip consumption for `&T`/`&mut T` parameter types.

Test suite: 658 tests at time of fix (32 pass-level, 15 integration/regression, 44 report assertions).

### Phase D complete: all items done

Phase D (testing, backend, and trust multipliers) is fully complete. Final items landed:

- **Item 5 — real-program corpus growth**: 4 new integration programs (calculator 200 lines, type registry 248 lines, pipeline processor 223 lines, stress bytecode interpreter 280 lines). Integration corpus now 12 programs. Stress workload exercises 11-variant enum, multiple Vec instances, 21-instruction execution loop, cross-module types/functions.
- **Item 7 — deferred audit reports**: next report modes named in `docs/PASSES.md` (`--report authority`, `--report proof`, `--report high-integrity` deferred to Phase E). All 6 existing modes regression-tested with 44 stable semantic assertions.

### Phase D item 4 complete: FFI/ABI maturity

`docs/ABI.md` documents what's stable (FFI-safe scalars, repr(C)/packed/align layout, extern fn), what's intentionally unstable (non-repr struct layout, enum representation, pass-by-ptr convention, symbol naming), platform assumptions (64-bit only, hardcoded sizes), the FFI safety model, and a cross-platform verification matrix. 4 layout verification tests added to `PipelineTest.lean` (scalar sizes, builtin sizes, repr(C) layout, pass-by-ptr decisions). Test suite: 651 tests (32 pass-level).

### Phase D2 complete: backend contract, ValidatedCore, and proof workflow

Phase D2 is done. The compiler now has explicit artifact boundaries with a proof-oriented pipeline, formal evaluation semantics with proven properties, and a documented SSA backend contract.

What landed:
- **`ValidatedCore` artifact** (`Concrete/Pipeline.lean`): explicit pipeline type. `Pipeline.coreCheck` is the only constructor; `Pipeline.monomorphize` takes `ValidatedCore`. `Pipeline.elaborate` returns `ElaboratedProgram` (elab + canonicalize only), `Pipeline.coreCheck` validates it.
- **`ProofCore` extraction** (`Concrete/ProofCore.lean`): filters `ValidatedCore` into the pure, proof-eligible fragment — pure functions (empty capability set, not trusted), safe structs (no repr(C)/packed), safe enums (no builtin overrides). Reports inclusion/exclusion counts.
- **Formal proof workflow** (`Concrete/Proof.lean`): evaluation semantics for a pure Core fragment (integers, booleans, arithmetic, let bindings, conditionals, function calls). Embeds abs, max, clamp. 17 proven theorems: concrete correctness (9), structural lemmas (3), conditional reduction (2), arithmetic (3).
- **SSA backend contract** (`docs/PASSES.md`): documents SSAVerify guarantees (8 invariants), SSACleanup guarantees (8 postconditions), EmitSSA assumptions (5 preconditions), and the invariant chain.

### Phase D1 complete: testing infrastructure

Phase D1 is done — all "done means" criteria met. Testing is now a first-class compiler subsystem with dependency-aware selection, pass-level coverage for all compiler passes, and a documented coverage matrix.

What landed:
- **Pass-level Lean tests** (`PipelineTest.lean`, 28 tests): parse (4), frontend/check/elab (8), monomorphize (2), SSA lowering (2), SSA verify (3), SSA cleanup (2), SSA emit (2), full pipeline (5). Each pass tested in isolation on in-memory source strings — no clang, no file I/O, <1s total. Tests both success and error paths.
- **Test metadata**: `test_manifest.toml` provides per-test reference metadata (category, kind, passes, profile, owner_pass — not consumed by the runner, serves as documentation and future tooling source). `test_dep_map.toml` maps 27 compiler source files to affected test sections and categories (consumed by `run_tests.sh --affected`).
- **Dependency-aware selection**: `run_tests.sh --affected` auto-detects changed files via `git diff` and runs only affected test sections. Conservative mapping: `--affected Concrete/Report.lean` runs 72 tests (report + passlevel); `--affected Concrete/Lower.lean` runs 248 tests (positive + codegen + O2 + passlevel). Unknown files fall back to the full suite.
- **Coverage matrix and determinism policy** (`docs/TESTING.md`): full coverage matrix by failure mode (17 categories) and by compiler pass (12 passes), determinism rules (fixed seeds, no wall-clock dependence, 3 timeout tiers, network isolation by default, parallel safety, quarantine/repair policy), compile-time baselines, and failure isolation documentation.
- **Compiler output cache**: file-keyed cache, 26/57 hits per fast run, avoids redundant recompilation for multi-assertion report tests.
- **Failure artifact preservation**: `.test-failures/` with timestamped output and exact rerun commands.
- **Manifest listing**: `run_tests.sh --manifest` now emits the full runner-known test inventory with category/kind/file metadata, so the documented manifest view is a real tool instead of a missing feature.
- **Dependency gates**: `compile_gate()` skips downstream assertions when compilation fails.
- **Real-program corpus**: 8 integration tests including 5 multi-feature programs (150-250 lines each): generic pipeline (5-layer borrow chain, trait dispatch), state machine (4×5 nested match), compiler stress (deep generic dispatch, 5-variant enum, while-loop accumulation), multi-module (cross-module types/traits/enums with imports), recursive structures (expression evaluator + stack machine with 6-variant enum).
- **Failure-path stdlib tests**: fs (read past EOF, seek past end, read empty file), net (bind empty address, write to refused connection, read from unconnected socket, bind duplicate port), process (kill invalid signal, wait invalid PID, kill PID zero).

Test suite: 647 tests passing (189 stdlib), including 28 pass-level Lean tests, 44 report assertions, 8 integration tests, and 16 collections verified.

### Structured LLVM backend completed

The LLVM backend no longer relies on raw LLVM string emission. `LLVMModule` is now the single source of truth for backend construction, and all emitted LLVM IR flows through structured types before printing.

What landed:
- user function codegen emits through structured LLVM module fields
- extern declarations emit through structured LLVM module fields
- type definitions emit through structured LLVM module fields
- globals emit through structured LLVM module fields
- the main wrapper, test runner, and vec builtins were converted from prebuilt text blobs into structured module output
- string/conversion builtins were rewritten into structured `LLVMFnDef` / `LLVMGlobal` / `LLVMFnDecl` output
- the `rawSections` escape hatch was deleted
- the legacy `Concrete/Codegen/` backend path was deleted

This is a major Phase D milestone because the backend is now structurally unified: every emitted LLVM construct is represented in structured data before the final printer turns it into text.

### Phase C complete: tooling and stdlib hardening

Phase C is done with all 8 items complete. This phase turned syntax guardrails, diagnostics, stdlib testing, and audit reports into durable infrastructure.

What landed:
- **Module-targeted stdlib testing**: `--stdlib-module <name>` in `run_tests.sh` runs tests for a single stdlib module (e.g., `--stdlib-module map`, `--stdlib-module string`) using `--test --module std.<name>`. Developers can iterate on one module without bootstrapping the whole tree.
- **Diagnostics/formatter polish**: fixed empty `{}` edge case in formatter (enum literals need braces to avoid parser ambiguity), fixed `String.trimLeft` deprecation, eliminated compiler warnings in `Check.lean`.
- **Integration testing deepened**: added `report_integration.con` (exercises all 6 report modes with caps/unsafe/alloc/layout/interface/mono) and `integration_collection_pipeline.con` (multi-collection pipeline with Vec, generics, enums, structs, mixed allocation patterns).
- **Report assertions hardened**: 44 report tests with content checks across all 6 modes, replacing crash-only checks with assertions that verify specific output content (struct sizes, public API exports, capability traces, allocation patterns, specialization details).
- **Reports as audit product**: 6 report modes (`caps`, `unsafe`, `layout`, `interface`, `mono`, `alloc`) with:
  - capability "why" traces showing which callees contribute each capability with `(intrinsic)`/`(extern)` tags
  - trust boundary analysis showing what unsafe operations trusted functions wrap (extern calls, pointer dereference, memory management)
  - allocation/cleanup summaries tracking alloc/free/defer patterns with leak warnings for functions that allocate without cleanup
  - summary totals and aligned columns across all reports
- **Formatter golden tests**: 4 formatter-specific golden tests with idempotency checking
- **LL(1) grammar checker in CI** (completed earlier in Phase C)
- **Linearity checker fixes** (completed earlier in Phase C)
- **Builtin HashMap retirement** (completed earlier in Phase C)

Test suite: 600 tests passing (189 stdlib), including 44 report assertions, 46 golden tests, and 16 collections verified.

### Builtin HashMap interception retired

Deleted ~1,400 lines of compiler-internal HashMap machinery across 6 Lean files. HashMap is now an ordinary stdlib type compiled through the normal generic struct path — no compiler interception, no hardcoded layout, no hand-written LLVM IR runtime.

What was removed:
- 7 intrinsic IDs (`mapNew`..`mapFree`) and their resolution/capability mappings from `Intrinsic.lean`
- ~106 lines of type checking intercepts from `Check.lean`
- ~74 lines of elaboration intercepts from `Elab.lean`
- ~75 lines of LLVM wrapper functions from `EmitSSA.lean`
- ~636 lines of hand-written LLVM IR runtime from `Codegen/Builtins.lean` (hash, probe, insert, get, contains, remove, grow — for both int and string key variants)
- Hardcoded 5-field `%struct.HashMap` type definition from `Layout.lean`
- `HashMap` removed from `builtinTypeNames` (it is now resolved through normal imports)

What replaced it: the stdlib `HashMap<K, V>` in `std/src/map.con` (a 7-field struct with fn pointer fields for hash/eq) compiles natively through monomorphization, the same path as any user-defined generic struct. 6 new stdlib tests (4 HashMap, 2 HashSet) provide collection verification coverage.

This was enabled by the linearity checker fixes in the previous milestone.

### Linearity checker: generic types, self-consumption, and divergence

Four fixes to the type checker's linearity analysis (`Check.lean`) that together unblock user-defined generic collections with function pointer fields — the same pattern as `HashMap`:

1. **`isCopyType` for generic and type-variable types** — `.generic` types now look up the struct's `isCopy` flag instead of returning `false`; `.typeVar` types check whether their bounds include `Copy`. Previously all generic instantiations were treated as linear.
2. ~~**Trusted function loop-consumption relaxation**~~ — removed in Phase G. `trusted` no longer relaxes linearity rules; it is now strictly about pointer-level containment.
3. **Self-consuming method calls** — methods that take `self` by value (not `&self`/`&mut self`) now mark the receiver variable as consumed. Previously `f.drop()` left `f` unconsumed.
4. **If-without-else divergence** — consuming a linear variable inside an if-then that unconditionally returns is now allowed. The checker detects that the then-branch diverges and skips the branch-consumption check, enabling the common `if bad { x.drop(); return err; }` guard pattern.

Validated by four independent regression tests and a full IntMap (user-defined hash map with fn pointer fields for hash/eq) that compiles and runs end-to-end. 544 tests passing, 0 failures.

### Phase A completion: fast feedback and aggregate-lowering hardening

- Hardened mutable aggregate lowering so aggregate state no longer flows accidentally through whole-aggregate phi nodes:
  - loop-carried aggregate variables are promoted to stable entry-block allocas instead of being transported as aggregate phi values
  - aggregate merges in `if`/`else` and `match` now lower through alloca+store/load patterns instead of `phi %Struct`
  - void-typed match results are filtered out of phi/store paths
- Added mechanical SSA protection for this architecture:
  - `SSAVerify` now rejects aggregate phi nodes (`struct`, `enum`, `string`, `array`) with a hard error instead of relying only on regression coverage
  - lowering phi-emission sites were audited so aggregate transport is intentionally blocked rather than incidentally absent
- Strengthened regression coverage around the new lowering path:
  - added `-O2` regressions for struct-loop lowering patterns
  - added SSA-shape verification for aggregate-merge cases so aggregate phi nodes are caught close to the source
- Upgraded the main test runner into a practical fast-feedback workflow:
  - `run_tests.sh` now defaults to parallel execution on available cores
  - added `--fast` (default), `--full`, `--filter`, `--stdlib`, `--O2`, `--codegen`, and `--report` modes
  - partial runs now report mode/filter/skip information clearly
  - documented `--fast` as the standard developer loop and `--full` as the pre-merge check
- This completed Phase A well enough for the roadmap to shift primary attention to Phase B semantic cleanup while leaving deeper testing architecture work for later phases.

### Stdlib test-runner activation and compiler fixes

- The stdlib test corpus now runs through the real compiler path via `concrete std/src/lib.con --test`, so module-local `#[test]` coverage in `std/src` is active CI protection instead of latent coverage.
- Fixed parser precedence around unary `*` / `&` with postfix field access so expressions like `*self.data` and `&self.field` bind correctly.
- Fixed lowering/codegen issues exposed by stdlib execution:
  - built-in `String` field access now lowers through a synthetic `String` struct definition so field offsets are computed correctly
  - `&mut` field method chains now write back into the parent struct instead of mutating a temporary copy
  - reference/pointer-typed values are no longer incorrectly spilled to allocas by `ensurePtr`
  - `char` / `bool` integer casts now use the proper integer-extension path instead of the old alloca/store/load fallback
- This brought the project to 488 passing tests in the main suite, including active stdlib-module coverage.

### Stdlib API cleanup and new collections

- Unified get/set convention across `String`, `Vec`, `Bytes`:
  - `get` is now checked (returns `Option`), `get_unchecked` is the raw fast path
  - `set` is now checked (returns `bool`), `set_unchecked` is the raw fast path
  - `Vec::pop` now returns `Option<T>` instead of unchecked raw access
- Fixed `std.io` print semantics:
  - Removed type-suffixed names (`print_int`, `print_bool`, `print_char`, `print_string`, `eprint_string`)
  - `print` now uses `write(1, ...)` (no trailing newline), `println` writes + newline, added `eprintln` for stderr
  - `read_line` annotated with `with(Alloc)`
- Converted `Text` and `Slice` to use `impl` method syntax (from C-style free functions)
- Fixed `test.con` to properly drop owned `String` messages on all code paths
- Updated all internal callers (`parse.con`, `fmt.con`, `hash.con`, `test.con`) to use new accessor names
- Added 5 new collections to the stdlib (33 modules total):
  - `std.deque.Deque<T>` — ring buffer with power-of-2 masking, push/pop front/back, checked/unchecked access
  - `std.heap.BinaryHeap<T>` — binary heap with fn-pointer comparator (works as min-heap or max-heap)
  - `std.ordered_map.OrderedMap<K, V>` — sorted array with binary search, fn-pointer comparator
  - `std.ordered_set.OrderedSet<K>` — thin wrapper over `OrderedMap<K, u8>`
  - `std.bitset.BitSet` — u64-word-backed bitset with set/unset/test, popcount, union, intersect
- All new collections have inline `#[test]` functions covering basic operations, edge cases, and stress tests

### Testing strategy expansion

- Added parser fuzzing infrastructure (`test_parser_fuzz.sh`): generates random/malformed inputs and verifies the parser never crashes or hangs
- Added `fmt`/`parse` round-trip property tests (`fmt_parse_roundtrip.con`): verifies `parse(format(x)) == x` across ranges, powers, and edge values
- Added `Vec` trace tests (`vec_trace.con`): push/get/set/length invariants, growth preservation, interleaved operations
- Added `HashMap` trace tests (`hashmap_trace.con`): insert/get/remove/overwrite invariants, tombstone recovery, growth stress
- Added report consistency tests:
  - capability reports (`report_caps_check.con`)
  - unsafe / trusted-boundary reports (`report_unsafe_rawptr.con`)
  - layout reports with runtime cross-validation (`report_layout_check.con`)
  - interface visibility reports (`report_interface_check.con`)
  - monomorphization reports (`report_mono_check.con`)
- Added codegen differential tests (16 assertions across `--emit-ssa`, `--emit-llvm`, `--emit-core`):
  - SSA optimization verification: constant folding (`2+3→5`), strength reduction (`*8→shl 3`), absence of un-optimized ops
  - Codegen structure: struct GEP offsets, enum tag load/compare, monomorphization naming, LLVM struct type definitions, mutable borrow stores
  - Cross-representation consistency: packed struct syntax in LLVM matches `--report layout`, enum payload size agreement, Core→SSA function signature mapping
- This completed the first planned testing-strategy expansion: parser fuzzing, property tests, trace tests, report consistency tests, and selected differential tests are now permanent coverage in the main suite

### Builtin / stdlib boundary cleanup

- Introduced `IntrinsicId` as the compiler-internal identity for builtins, replacing raw string matching in compiler dispatch paths
- Removed the ad hoc builtin `abs` special case; `abs` now lives in the stdlib as a trait method (`Numeric::abs`) resolved through normal trait dispatch + monomorphization
- Migrated the remaining monomorphic math wrappers (`sqrt`, `sin`, `cos`, `tan`, `pow`, `log`, `exp`, `floor`, `ceil`) out of compiler intrinsics and into `std.math` as `trusted extern fn`
- Added `trusted extern fn` as an explicit audited foreign-binding category:
  - ordinary `extern fn` still requires `with(Unsafe)`
  - `trusted extern fn` exposes narrow trusted foreign symbols without leaking `Unsafe` to callers
  - audit reports now distinguish trusted extern functions from ordinary extern functions
- Removed dead intrinsic entries and then removed 17 I/O / File / Network / Process / Env intrinsics by migrating them to stdlib wrappers
- Deleted large hand-written LLVM builtin/codegen paths that were no longer needed after the stdlib/trusted-extern migration
- Tightened the public stdlib surface so more operations now route through ordinary stdlib APIs instead of compiler-known names

### LL(1) parser cleanup

- Removed all remaining parser save/restore backtracking sites
- Left-factored top-level `mod` parsing
- Removed retry-based parsing around `&self` / `&mut self`
- Tightened turbofish/type-position parsing so `::` commits instead of rewinding
- Moved enum-dot fallback handling into postfix parsing instead of speculative rewind
- Parser implementation now matches the language’s strict LL(1) design goal much more closely

### Lowering bug fixes (string dedup + variable scoping)

- Fixed string constant naming collision: multiple functions with string literals independently generated `str.0`, `str.1`, etc. The second lowering pass concatenated per-function lists, producing duplicate LLVM globals. Fix: `lowerFn` now returns string literals alongside the function definition; `lowerModule` collects, deduplicates by value, and renames references per-function. The redundant second lowering pass is removed.
- Fixed SSA domination error in if/else with while loops: restoring pre-if variable state before the else-branch only overwrote existing variables, leaving then-branch locals (loop body registers) visible in the else-branch scope. Fix: replace the per-variable restore with a full variable map replacement so the else-branch starts with exactly the pre-if variable set.
- Fixed the same variable leakage bug in while loop exits: body-local variables leaked into subsequent code after the loop. Fix: replace the per-variable phi restore at all four while-loop exit points with full variable map replacement.
- Added regression tests: `string_multi_fn.con`, `if_else_while.con`

### Trusted boundaries

- Added `trusted fn` and `trusted impl` to the language surface
- Propagated trusted boundaries through AST, Core, lowering metadata, and audit reporting
- `CoreCheck` now relaxes raw pointer dereference, raw pointer assignment, and pointer-cast checks inside trusted code
- Kept `extern fn` calls under `with(Unsafe)` even inside `trusted`
- Added tests for trusted functions, trusted impls, trusted pointer operations, and invalid trusted usage
- Added trusted trait-impl support and grouped trusted boundary reporting at the source level
- Migrated builtins, stdlib, and user code to one explicit trust/effect model with honest capability annotations
- Fixed trusted pointer arithmetic end-to-end by teaching SSA verification and codegen how to handle pointer + integer lowering

### Stdlib collections: HashMap and HashSet

- Added `std.map.HashMap<K, V>` — open-addressing hash map with linear probing, fn-pointer hash/eq (Zig-style)
- Added `std.set.HashSet<K>` — thin wrapper around `HashMap<K, u8>`
- Added hash/eq helper functions in `std.hash`: `hash_u64`, `hash_i32`, `hash_i64`, `hash_string`, `eq_u64`, `eq_i32`, `eq_i64`, `eq_string`
- Fixed compiler bug: function pointers loaded from struct fields were emitted as direct calls (`@name`) instead of indirect calls (`%name`), causing linker errors. Fix spans Lower, SSACleanup, and EmitSSA.

### Test framework

- Added `--test` CLI flag: `concrete file.con --test` compiles and runs all `#[test]` functions
- `#[test]` attribute tracked through the full IR pipeline (AST → Core → Mono → SSA)
- Generated test runner calls each test, prints `PASS: <name>` / `FAIL: <name>`, exits 0/1
- Test collection is recursive through submodules
- Validation: `#[test]` functions must have no parameters, not be generic, and return `i32`
- Validation: `#[test]` on non-function declarations is a parse error

### Compiler architecture

- Replaced the old direct AST backend with the full pipeline:
  `Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> clang`
- Added explicit Core IR, elaboration, monomorphization, SSA lowering, SSA verification, SSA cleanup, and SSA-consuming codegen
- Removed the legacy AST backend and `--compile-legacy`
- Added `Concrete/Pipeline.lean` with explicit artifact types:
  - `ParsedProgram`
  - `SummaryTable`
  - `ResolvedProgram`
  - `ElaboratedProgram`
  - `MonomorphizedProgram`
  - `SSAProgram`
- Introduced `IntrinsicId` so the remaining compiler-known operations are identified internally instead of by raw string names

### Frontend and semantic boundaries

- Established the summary-based frontend with `FileSummary` and `ResolvedImports`
- Split `Resolve` into shallow/interface resolution and body-level name resolution
- Moved most post-elaboration legality checks out of `Check.lean` and into `CoreCheck.lean`
- Made `CoreCheck` the main post-elaboration semantic authority
- Centralized `Self` type resolution via shared helpers

### Diagnostics

- Added structured diagnostic types across semantic passes:
  - `ResolveError`
  - `CheckError`
  - `ElabError`
  - `CoreCheckError`
  - `SSAVerifyError`
- Threaded source spans through the AST/parser
- Moved the main semantic pipeline to native `Diagnostics` transport instead of mostly string-based bridging
- Added range-capable spans, hint text, and broader error accumulation across functions/modules in `Check` and `Elab`
- Added report/inspection modes:
  - `--report caps`
  - `--report unsafe`
  - `--report layout`
  - `--report interface`
  - `--report mono`
- Added report consistency coverage so capability, unsafe/trusted, layout, interface, and monomorphization reports are now regression-tested against real semantics and emitted LLVM

### ABI / layout / low-level semantics

- Added `#[repr(C)]` for structs
- Added `#[repr(packed)]` and `#[repr(align(N))]`
- Added `sizeof::<T>()` and `alignof::<T>()`
- Centralized layout logic in `Concrete/Layout.lean`
- Unified FFI-safety checks and LLVM type-definition generation through `Layout`
- Fixed aligned struct/enum layout and enum payload offset handling
- Fixed builtin `Option` / `Result` layout to size payloads from actual instantiations instead of hardcoded `i64` assumptions

### Language capabilities

- Capabilities and capability polymorphism
- Function pointers (closures intentionally omitted)
- Borrow regions
- Linear ownership tracking
- `defer`, `Destroy`, and `Copy`
- Monomorphized trait dispatch
- Multi-file modules and `Self`
- `newtype`
- Raw-pointer `Unsafe` gating for dereference, assignment, and pointer-involving casts

### Runtime-facing builtins

- String builtins
- File I/O builtins
- Networking builtins
- `Vec<T>` and `HashMap<K, V>` builtin/runtime-backed support

### Standard library foundation

- Hardened the early `vec`, `string`, and `io` modules with correctness and completeness fixes
- Added:
  - `std.bytes`
  - `std.slice`
  - `std.text`
  - `std.path`
  - `std.fs`
- Expanded libc/math/test support to better support the growing stdlib surface

### Standard library systems layer

- Added `std.env` — environment variable access (get/set/unset)
- Added `std.process` — Unix process control (exit, getpid, fork, kill, Child with wait)
- Added `std.net` — TCP networking (TcpListener with bind/accept/close, TcpStream with connect/read/write/close)
- Extended `std.libc` with process (setenv, unsetenv, getpid, fork, execvp, waitpid, kill) and networking (socket, bind, listen, accept, connect, close, send, recv, htons, htonl, inet_pton, setsockopt) declarations
- Added module-level `#[test]` functions to `bytes` and `path`

### Stdlib hardening — typed error surfaces

- Added typed error enums to `std.fs`: `FsError`, `FileResult`, `ReadResult`, `WriteResult` — all `fopen` calls are now null-checked, `write_file` returns typed `WriteResult`
- Added typed error enums to `std.net`: `NetError` (including `SetsockoptFailed`, `AddressFailed`), `ListenResult`, `StreamResult` — all syscall returns checked including `setsockopt` and `inet_pton`
- Added typed wrappers to `std.process`: `ForkResult`, `KillResult`, `WaitResult`, `ExitStatus`, `ProcessError` — `fork`/`kill`/`wait` return typed results with POSIX wait-status interpretation
- Changed `std.env::get()` to return `Option<String>` — distinguishes absent vars from empty ones
- Made `Bytes` accessors explicit: `get`/`set` are now bounds-checked (returning `Option<u8>`/`bool`), `get_unchecked`/`set_unchecked` are the raw fast paths
- Made `Option<T>` pub for cross-module use
- Added failure-path `#[test]` functions across stdlib modules:
  - `bytes`: checked get/set in-bounds and out-of-bounds
  - `fs`: open/create/read/write on nonexistent paths
  - `env`: get absent var, set-then-get round-trip
  - `net`: connect and bind with invalid addresses
  - `process`: kill invalid pid, fork-wait typed round-trip

### Stdlib deepening — fmt, time, rand, hash + io hardening

- Added `std.fmt` — pure-Concrete formatting: `format_int`, `format_uint`, `format_hex`, `format_bin`, `format_oct`, `format_bool`, `pad_left`, `pad_right`
- Added `std.hash` — FNV-1a hash: `fnv1a_bytes`, `fnv1a_string` (pure Concrete, no libc dependency)
- Added `std.rand` — deterministic random: `seed`, `random_int`, `random_range` (wraps libc rand/srand)
- Added `std.time` — monotonic clock and sleep: `Duration` (from_secs/from_millis/from_nanos), `Instant` (now/elapsed), `sleep`, `unix_timestamp` (wraps clock_gettime/nanosleep/time)
- Hardened `std.io`: `File::create` and `File::open` now return `OpenResult` with null-checked fopen (added `IoError`, `OpenResult` enums)
- Extended `std.libc` with time (time, clock_gettime, nanosleep) and random (rand, srand) declarations
- Added failure-path tests: `fs::test_write_to_readonly`, `net::test_connect_refused`, `process::test_wait_invalid_pid`
- Added module-level `#[test]` functions across all four new modules

### Stdlib uniformity + deepening + parse

- Made `Result<T, E>` pub for cross-module use as a generic error container
- Patched `Check.lean` `?` operator to support generic enums (e.g. `Result<File, FsError>`) with type substitution, not just named enums
- Unified error/result types across stdlib: removed module-specific result enums (`OpenResult`, `FileResult`, `ReadResult`, `WriteResult`, `ListenResult`, `StreamResult`, `KillResult`, `WaitResult`), replaced with `Result<T, ModuleError>` everywhere
- `std.io`: `File::create`/`File::open` return `Result<File, IoError>`
- `std.fs`: `File::open`/`File::create` return `Result<File, FsError>`, `read_file` returns `Result<Bytes, FsError>`, `write_file` returns `Result<u64, FsError>` (now reports bytes written)
- `std.net`: `TcpListener::bind` returns `Result<TcpListener, NetError>`, `TcpStream::connect` returns `Result<TcpStream, NetError>`, `TcpListener::accept` returns `Result<TcpStream, NetError>`
- `std.process`: `kill` returns `Result<bool, ProcessError>`, `Child::wait` returns `Result<ExitStatus, ProcessError>`, `ForkResult` kept as 3-variant union
- Added `std.parse` — inverse of `fmt`: `parse_int`, `parse_uint`, `parse_hex`, `parse_bin`, `parse_oct`, `parse_bool` (all return `Option<T>`), plus `Cursor` struct for structured input parsing (`peek`, `advance`, `skip_whitespace`, `expect_char`)
- Added checked accessors: `String::get` returns `Option<char>`, `Vec::get` returns `Option<&T>` (reference-in-generic monomorphizes correctly)
- Systems deepening:
  - `std.fs`: `append_file`, `file_exists`, `read_to_string`, write-then-read roundtrip test
  - `std.net`: `TcpStream::write_all` (loop until all sent), `TcpStream::read_all` (read until EOF into Bytes)
  - `std.process`: signal constants (`sig_int`, `sig_kill`, `sig_term`), `spawn` (fork+execvp), `SpawnFailed` error variant

### Stdlib test deepening

- Deepened `std.test`: added `assert_gt`, `assert_lt`, `assert_ge`, `assert_le`, `str_eq`, `assert_str_eq`
- Added `std.net` integration tests: `test_tcp_roundtrip` (fork-based listener/client pair), `test_tcp_write_all_read_all` (write_all + read_all with Bytes)
- Added standalone `net_tcp_roundtrip.con` lean_test using builtins (`tcp_listen`, `tcp_accept`, `tcp_connect`, `socket_send`, `socket_recv`, `socket_close`)

### Testing / status milestones

- End-to-end main suite has continued to grow through the milestones above; current suite size and latest status live in `README.md` and `ROADMAP.md`, not here
- SSA-specific suite passing
- Golden SSA/IR testing integrated
- CI updated to exercise SSA-specific coverage as well as the main path
- Added stronger stdlib failure-path and integration coverage, including socket round-trip tests and parser/process edge cases
