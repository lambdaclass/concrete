# Changelog

Status: changelog

This file tracks major completed milestones for Concrete.

It is intentionally milestone-oriented rather than release-oriented. The project is still evolving quickly, so the useful unit of history is “what architectural or language capability landed,” not tagged versions.

For current priorities and remaining work, see [ROADMAP.md](ROADMAP.md).

## Major Milestones

### Phase E pass-by-pass verify gates

The four downstream pipeline boundaries — post-elab, post-mono,
post-lower, post-cleanup — are now wired through a single
`Pipeline.runVerifyGates` entry point and exposed both as a per-
program report (`concrete <file> --report verify`) and a corpus-wide
make target (`make test-verify-gates`). Roadmap reference: Phase
E.11.

- **Contract** (`docs/VERIFY_GATES.md`): named boundaries, structural
  invariant per boundary, severity, surface, the documented `try_`
  and `defer` placeholder exceptions for the post-elab gate, and the
  never-delete rule.
- **Plumbing** (`Concrete/Pipeline.lean`): new `VerifyReport`
  structure carrying per-gate diagnostic lists, with helpers
  `isClean` / `errorCount` / `warningCount` / `allDiagnostics`. New
  `runVerifyGates : ValidatedCore → VerifyReport` runs every gate
  and short-circuits cleanly when a downstream pass refuses to even
  produce its artifact (mono error → post-mono diags; lower error →
  post-lower diags; etc.).
- **Renderer** (`Concrete/Verify.lean`): `renderVerifyGates`
  produces a human-readable per-gate banner with `ok` / `warn` /
  `FAIL` status and indented detail per non-clean gate.
- **Single-program surface**: `concrete <file> --report verify`
  replaces the previous post-elab + post-mono-only output with the
  full four-gate report. Exits non-zero only on errors.
- **Corpus surface** (`make test-verify-gates`,
  `scripts/tests/test_verify_gates.sh`): runs the gates across the
  oracle vectors (`tests/oracle/vectors.txt`) and the wrong-code
  corpus's `kind = "runtime"` entries. Asserts zero errors. First
  landing: 78 PASS / 0 FAIL / 0 SKIP, 2 warnings — both the
  documented `try_` placeholder leak in `result_ok.con` and
  `result_err.con`.
- **Discipline**: WC-0004 is the canonical wrong-code case for this
  pipeline pattern (post-lower SSAVerify caught a Lower bug before
  it could miscompile). Future verify-gate failures across the
  corpora will register in the wrong-code corpus under the
  `verifier-trigger` category with bundle capture per
  `docs/BUG_BUNDLE.md`.

### Phase D bug bundle export

The reducer's natural companion. Closes the gap between "I have a
minimized repro" and "I have a manifest case landed in
`tests/wrong-code/`." Roadmap reference: Phase D.12.

- **Contract** (`docs/BUG_BUNDLE.md`): bundle purpose, required
  layout, what is always included, what is included only if
  available, and how bundles relate to
  `tests/wrong-code/manifest.toml`.
- **Bundler** (`scripts/tests/capture_wrong_code_bundle.sh`): wraps
  the existing `concrete debug-bundle` for the structural
  pipeline-state skeleton (manifest.json, source/, diagnostics.txt,
  IR dumps via `--emit-core` / `--emit-ssa` / `--emit-llvm`) and
  layers on corpus-specific overlay files: `command.txt`,
  `predicate.txt`, `compiler-version.txt`, separated `stdout.txt` /
  `stderr.txt` capture, and a `reports/` directory of any reports
  that ran cleanly. Adds a sibling `bundle.json` recording the case
  id, predicate, source basename, and line count. Also captures
  `ssa-unverified.txt` opportunistically (useful for verifier-
  rejection bugs like WC-0004).
- **Reducer integration** (`scripts/tests/minimize_wrong_code.sh
  --bundle <dir>`): after a successful reduction, captures a bundle
  of the reduced output. Case id is the bundle directory's
  basename; predicate is recorded verbatim. The full pipeline now
  flows: `new bug → reduce → bundle → manifest entry → permanent
  regression`.
- **Smoke test** (`make test-bundle-smoke`): 29 checks. Captures
  bundles on both a failing source (E0209 rejection) and a passing
  source (fib), asserts every always-included file is present plus
  the appropriate stage-conditional files (diagnostics.txt for the
  failing case; core/ssa/llvm + reports for the passing case).
  Validates argument plumbing rejects missing required flags. Does
  not run a long reduction.

### Phase D reducer / minimizer workflow

The wrong-code corpus pipeline now closes the loop from "I just hit a
compiler bug" to "the bug is a manifest entry." Roadmap reference:
Phase D.14.

- **Contract** (`docs/REDUCER_WORKFLOW.md`): when to reduce, what
  counts as a good minimized repro, when not to over-minimize, how
  to preserve expected behavior with a correctly-stated predicate,
  and how reduced cases enter `tests/wrong-code/manifest.toml`.
- **Predicate scripts** (`scripts/reduce/expect-*.sh`): four
  user-facing predicates — `expect-error-code.sh`,
  `expect-runtime-output.sh`, `expect-oracle-mismatch.sh`,
  `expect-report-contains.sh`. Each takes the candidate path as the
  final argument and returns 0 (still triggers) or 1 (no longer
  triggers). They're standalone tools — usable from a terminal, the
  reducer, or anywhere a predicate makes sense.
- **Wrapper** (`scripts/tests/minimize_wrong_code.sh`): translates a
  high-level predicate (`error-code:E####`, `runtime-output:V`,
  `oracle-mismatch`, `report-contains:KIND:SUB`) into a `concrete
  reduce --predicate external:<scripts/reduce/...>` invocation.
  Verifies the predicate holds on the original source before
  starting (refuses to reduce a passing program).
- **Reducer extension** (`Concrete/Reduce.lean`): new
  `Predicate.external (cmd : String)` constructor. Writes the
  candidate to a temp file, shells out to the command (split on
  whitespace, candidate path appended), returns true iff exit 0.
  All four high-level predicates now funnel through the same
  syntax-aware shrinker.
- **Smoke test** (`make test-reducer-smoke`): 13 checks covering
  predicate parsing, compiler invocation, pass/fail discrimination,
  and wrapper argument plumbing. Does NOT run a long minimization;
  the engine is exercised indirectly by `make test-wrong-code`.
- **Real-case demo**: ran on WC-0015 (`bug_int_match_disagree.con`,
  the int-match consumption-disagreement rejection). 27 lines → 20
  lines, predicate `error-code:E0209` preserved through the
  reduction. The case's notes file now carries a Reduction block
  with the replay command. Use this as the canonical example when
  writing new corpus entries.

The full pipeline is now: **new bug → reduce → manifest entry →
permanent wrong-code regression**.

### Wrong-code corpus bulk import (WC-0005 .. WC-0022)

The Phase D wrong-code corpus is bulk-registered with all existing
`tests/programs/bug_*.con` cases plus a curated selection of high-risk
adversarial cases. The corpus now sits at **22 PASS / 0 FAIL / 0 OPEN**.

- **bug_*.con bulk import**: 14 new entries (`WC-0005` through
  `WC-0018`), one per existing `bug_*.con` outside the four already
  registered. Per the contract, each entry references the existing
  repro path — no files were moved. Per-case notes capture the
  symptom, expected behavior, regression command, and known
  historical context (commit / bug-number when available).
- **Adversarial selective import**: 4 new entries (`WC-0019` through
  `WC-0022`) drawn from `tests/programs/adversarial/<area>/` — only
  cases that exercise high-risk compiler boundaries already known to
  produce real bugs. `newtype/heap_pattern.con` and
  `newtype/in_option_chain.con` cover the same enum-payload + heap
  layout surface as the older newtype-in-enum-payload codegen bug;
  `borrow/mut_borrow_through_match.con` is adjacent to WC-0004's
  match+borrow class; `trait_dispatch/trait_method_via_bound.con` is
  recently-shipped feature surface. Generic adversarial coverage
  stays under `tests/programs/adversarial/<area>/` without a manifest
  entry, per the curated-not-exhaustive rule.
- **Manifest schema**: handles both `kind = "runtime"` (21 cases) and
  `kind = "compile-error"` (1 case — `WC-0015`, the int-match
  consumption-disagreement rejection that must surface as `E0209`).
  Multi-line `expected` values now use TOML `\n` escapes; the harness
  parser unescapes them through TSV transport.
- **Categories**:
  - `miscompile` — 19 cases (the bulk: layout, codegen, intrinsic,
    field-assign, cross-module borrow / field bugs)
  - `error-regression` — 3 cases (WC-0013 if-expr, WC-0014 int-match
    propagate, WC-0015 int-match disagree)
- **Run state**: all 22 entries pass under `make test-wrong-code`.

### WC-0004 fixed — match arm-local bindings leaked into next match's var-table

The first wrong-code corpus case to be driven from `open` to `fixed`
through the corpus's own workflow. Confirmed the contract works on
real bugs, not just historical fixed ones.

- **Symptom**: two or more sequential `match` expressions over an enum
  with a payload-binding variant (e.g. `Check::Fail { code }`), plus a
  mutable accumulator shared across the matches, triggered
  `SSAVerify` E0708 (phi operand not dominated by def block).
  `parse_validate`'s phase-3 work hit this in accumulator style and
  was rewritten to early-return form to ship; the original symptom
  cited E0703 (a sibling dominator code that may have been the
  diagnostic before a verifier refactor).
- **Root cause**: `Concrete/Lower.lean`'s match lowering calls
  `setVar` for enum-payload arm bindings, mutating the global var-
  table. Before each arm the lowering correctly restores `vars :=
  preMatchVars`, but at the END of the match the var-table still
  held whatever bindings the last arm introduced. The next match's
  `snapshotVars` then captured those leaked bindings into its
  `preMatchVars`. The next match's merge iterated over those leaked
  names and emitted a phi across arms that didn't all bind them —
  the `Pass` arm carried over the previous match's `Fail`-arm
  register, producing a phi like `[%t9, %arm06], [%t21, %arm18]`
  where `%t9` was defined inside an arm that doesn't dominate
  `arm06`.
- **Fix**: in all three exit paths of `.match_` lowering (multi-arm
  merge, single-arm merge, all-arms-terminated), restrict the var-
  table back to the names that existed in `preMatchVars`. The merge
  has already written the phi registers for the merged variables;
  the leaked arm-local bindings just need to be dropped.
- **Diagnostic plumbing added**: `concrete <file>
  --emit-ssa-unverified` and `Pipeline.lowerUnverified` skip both
  the verifier and the cleanup pass to print raw Lower output.
  Debugging infrastructure for the wrong-code workflow per
  `docs/WRONG_CODE_CORPUS.md`; not on the production compile path.
- **Regression coverage**: `tests/wrong-code/cases/WC-0004/
  program.con` is now a permanent runtime regression in the corpus.
  WC-0004 in `tests/wrong-code/manifest.toml` flips from `open` to
  `fixed`. The full test suite (1572 positive tests, fast tier) is
  green.

### Phase D wrong-code corpus contract + seed

The Phase D.16 named regression corpus has a contract, a CI entry point, and the first four seeded cases.

- **Contract** (`docs/WRONG_CODE_CORPUS.md`): defines what qualifies as a wrong-code regression (miscompile, codegen-divergence, verifier-trigger, fact-report-mismatch, proof-evidence-regression, crash, error-regression), the per-case manifest schema (id, category, status, repro, kind, expected, discovered, fixed, notes), the minimization rule ("smallest input that triggers the bug"), the artifact bundle (the per-case directory under `tests/wrong-code/cases/<id>/`), and the lifecycle (cases never deleted; growth is monotonic).
- **Registry** (`tests/wrong-code/manifest.toml`): the corpus is a registry, not a relocation. Existing repros under `tests/programs/bug_*.con` and `tests/programs/adversarial/<area>/` keep their wiring; the manifest references them by path. New repros that have no other home land under `tests/wrong-code/cases/<id>/`.
- **CI** (`make test-wrong-code`, `scripts/tests/test_wrong_code.sh`): runs every `status = "fixed"` case and asserts expected behavior. `--include-open` additionally probes open cases and reports when an open bug becomes fixed (so a manifest update isn't missed). The script also enforces manifest invariants (every `repro` and `notes` path exists).
- **Seeded cases**:
  - WC-0001 — chained newtypes broke the cast-validity exemption (fixed in `66d8736`, repro `tests/programs/adversarial/newtype/chained.con`)
  - WC-0002 — `.0` on a borrowed newtype produced `&Newtype → Inner` (fixed in `66d8736`, repro `tests/programs/adversarial/newtype/borrow.con`)
  - WC-0003 — narrow-int field assign elaborated the RHS as `Int` (fixed in `66d8736`, repro `tests/programs/bug_field_assign_narrow_field.con`)
  - WC-0004 — accumulator-style match with mut-counter triggers SSA-verify dominator violation (status `open`; current code `E0708`, originally reported as `E0703`; the candidate minimized repro lives under `tests/wrong-code/cases/WC-0004/program.con` and confirms the bug is still reproducible)
- **First run**: 3 fixed cases pass; 1 open case still fails as expected. `make test-wrong-code` exits 0; `--include-open` adds the open-case report without changing exit status.

### Phase A close-out: value-return semantic oracle is effectively closed

The Phase A.1 differential harness has reached its useful steady state. 56 PASS / 0 FAIL / 1 PENDING across 57 vectors cover the predictable-core surface: int/bool/struct/enum/array, linearity + borrows + named borrow regions + deref-assign, generics, Result/Option/`?`, string literals plus `string_length` / `drop_string`, and the canonical `parse_validate` / `service_errors` examples — every supported program is now a live differential check between compiled output and `--interp` output.

- **Closed gaps**: borrow / `borrowIn` / `?` / string literals all flipped from PENDING to PASS via interpreter additions (`9de6ae0`, `f4a7539`, `764ddd4`).
- **Single remaining PENDING**: `examples/fixed_capacity/src/main.con` is blocked on print/IO intrinsics. The interpreter cannot reproduce stdout side effects, so any program that interleaves `print` / `println` with its return value is intentionally outside the harness contract. This is a *contract widening* concern (compare interleaved stdout, not just the int return), not a Phase A interpreter gap. Modeling print/IO is deferred until a flagship example forces it.
- **Trust boundary**: `docs/INTERPRETER_TRUST.md` documents the supported subset, the path-with-frame borrow model, the `?` lowering equivalence, the materialize-into-env handling for borrows of literals, and the explicit "interp: print/IO intrinsic ... not yet supported" diagnostic for I/O calls.

### Semantic-oracle differential harness lands as a regression surface

Phase A.1 + A.2 close: the source-level interpreter (`Concrete/Interp.lean`, `--interp`) is now driven by a corpus harness that compiles every vector to a native binary, runs `--interp`, and compares trimmed stdout. Mismatches are fail-the-build regressions; explicit `interp: ...` skips are recorded as PENDING entries pointing at named interpreter gaps. The companion trust-boundary doc enumerates the interpreter's supported subset and the assumptions that make it a defensible oracle.

- **Harness**: `scripts/tests/test_oracle.sh` reads `tests/oracle/vectors.txt` and produces `ORACLE: PASS=N FAIL=N PENDING=N TOTAL=N`. Compiled and `--interp` paths share the same `<value>\n` contract for `fn main() -> Int` because the SSA wrapper formats the int return as `%lld\n` and `Main.lean`'s `interpProgram` `IO.println`s the int return.
- **Corpus**: 39 vectors covering fib/arithmetic/recursion, structs, arrays, enums, match, linearity (non-borrow), generics, plus the canonical roadmap examples `parse_validate` and `service_errors`. Initial state: 32 PASS, 0 FAIL, 7 PENDING — borrow/try/string-literal cases (`fixed_capacity`, `result_ok`, `borrow_read`, `string_basic`, `impl_method`).
- **Wired into normal flow**: `make test-oracle` runs standalone; `make test-full` runs the harness as the tail of the existing `interp` section in `scripts/tests/run_tests.sh`. The `full` mode includes `interp` (it didn't before this change).
- **Trust-boundary doc**: `docs/INTERPRETER_TRUST.md` enumerates supported expressions/statements, the explicit `interp: ...` rejection contract for unsupported constructs, the value-passing-no-aliasing memory model, the arbitrary-precision-integer arithmetic stance (no wrapping — programs that need it are outside the predictable subset, not interpreter bugs), the 10M-iteration loop fuel, and the four reasons the interpreter is intentionally smaller and more trustworthy than the full compiler (small single file, no transformation passes, no platform dependence, no silent approximations).
- **Oracle README**: `tests/oracle/README.md` documents the harness contract, the PENDING-vs-FAIL split, and the rule that a PENDING is a Phase A.3 work item, not a permanent skip.

### Completed roadmap tasks moved out of the active roadmap

The active roadmap now lists remaining work only. Completed Phase A/B/C task lines were removed from `ROADMAP.md`, summarized here, and the remaining phase task numbers were renumbered.

- **Predictable-core foundation**: fixed-capacity examples, safe fixed-array Copy support, stack-depth reporting, predictable-boundary docs, failure strategy, failure-only discipline, parse/validate and service-style error-flow examples, the first predictable/core interpreter, diagnostic UX standards, trusted-boundary guide, freestanding split, standalone/project UX notes, project bootstrap design, example inventory/lifecycle/no-duplicate policy, and phase exit checklists are treated as completed history.
- **Pre-stdlib pressure workloads**: parser/decoder, ownership-heavy structure, borrow/aliasing, trusted-wrapper/FFI, fixed-capacity/no-alloc, and cleanup/leak-boundary pressure sets are complete and now live as historical evidence rather than active roadmap tasks.
- **Stdlib and syntax freeze history**: string/text/bytes contracts, stdlib target/audit/surface freeze, design principles, checked indexing, foundational modules, runtime collections, hosted/core split, anti-features, arithmetic policy, formatting/output, Result/error-flow ergonomics, module layout polish, docs/examples, workload-backed freeze evidence, LL(1) pattern cleanup, validated wrappers/newtypes, visibility/module hygiene, endian byte cursor APIs, layout contract, syntax review, and first-release surface freeze are now recorded as completed milestones instead of active tasks.

### Concurrency research direction and lettered roadmap land

The roadmap now uses lettered phases (`A`-`N`) with task numbering restarting inside each phase, and the concurrency/parallelism direction is documented as research rather than shipped behavior.

- **Evidence-bearing concurrency direction**: `research/stdlib-runtime/async-concurrency-evidence.md` defines the long-term target: capability-gated structured concurrency, optional-overlap versus required concurrent progress, linear task handles, bounded channels, deterministic simulation, and concurrency evidence reports. README links the direction while explicitly saying async/concurrency is not implemented.
- **Supporting research notes**: added focused notes for permission-set polymorphism (`research/language/capability-polymorphism.md`), bounded channels (`research/stdlib-runtime/channel-model.md`), FFI/cancellation boundaries (`research/stdlib-runtime/ffi-cancellation-boundary.md`), per-task stack analysis (`research/predictable-execution/concurrent-stack-analysis.md`), a concurrency formal model (`research/proof-evidence/concurrency-formal-model.md`), and a worked `--report concurrency` example (`research/proof-evidence/concurrency-evidence-example.md`).
- **Gated capability framing**: capability polymorphism is now framed as static permission-set polymorphism, not effect-row polymorphism. Capabilities are documented as permission gates checked at call sites, with no handlers, resume semantics, continuation capture, or runtime capability values.
- **Roadmap cleanup**: `ROADMAP.md` now has Phases `A`-`N`, per-phase task numbering, and a linear Phase L concurrency track. The active concurrency work starts with a pressure-test suite and v1 design freeze before any compiler implementation.

### Adversarial test sweep across language features finds three compiler bugs

A breadth-first set of adversarial tests organised under `tests/programs/adversarial/<area>/` (newtype, enum_match, borrow, defer, generic, linear, trait_dispatch) exposed three unrelated compiler bugs. Each bug ships with a named regression test under either the area folder or `tests/programs/bug_*.con`, all wired into `make test`.

- **Bug 1 — chained newtypes broke the cast-validity exemption.** `Outer = Middle = Inner = i32` rejected `Middle(Inner(...))` at E0553 because the exemption used `Layout.resolveNewtype` (recurses to the primitive) instead of one-step unwrapping. The constructor wraps one step at a time, so the inner-vs-resolved comparison must be one-step too. Fix in `Concrete/CoreCheck.lean`: replaced the recursing check with a `oneStepInner` helper that finds the immediate inner type only, and try both wrap and unwrap directions independently. `Layout.ntSubstTy` was also promoted from `private` so CoreCheck can reuse it for generic newtypes. Regression: `tests/programs/adversarial/newtype/chained.con`.
- **Bug 2 — `.0` on a borrowed newtype produced `&Newtype → Inner`.** `&self.0` (and `&mut self.0`) in an inherent impl method emitted a `.cast` from `.ref Newtype` to the inner primitive. CoreCheck's exemption (matching only direct `Newtype ↔ Inner` pairs) rejected it, and codegen would have mishandled the ref→value transition. Fix in `Concrete/Elab.lean`'s field-access path: when the receiver is a ref/refMut, emit `.deref cObj newtypeTy` first so the rebrand cast is `Newtype → Inner` as expected. Regression: `tests/programs/adversarial/newtype/borrow.con`.
- **Bug 3 — narrow-int field assign elaborated the RHS as `Int`.** `c.n = 100` where `n: i32` elaborated `100` as `Int` (i64) because `elabExpr value` was called with no type hint. Codegen emitted `store i64 100` to a 4-byte field — undefined behaviour that the LLVM optimiser deletes at `-O2`. The bug masked itself under `lli` (which honours the low 4 bytes) and only surfaced when reading back via the native binary. Fix in `Concrete/Elab.lean`'s `.fieldAssign` path: look up the field's declared type from the struct definition and pass it as the value hint, so `100` is elaborated as `i32` directly. Regression: `tests/programs/bug_field_assign_narrow_field.con`.

Coverage added: 15 new adversarial tests across newtype (9), enum_match, borrow, defer, generic, linear, trait_dispatch (1 each), plus the dedicated bug regression. `make test` 771 → 787 pass / 0 fail.

### Phase 3 stdlib and syntax freeze closes

The canonical Phase 3 exit checklist is now 19/19 complete. The first-release stdlib/syntax surface is treated as freeze-ready on current evidence rather than on aspirational follow-up rewrites.

- **Runtime-collection close-out**: `docs/RUNTIME_COLLECTIONS.md` and `docs/STDLIB_FREEZE_LEDGER.md` now treat the shipped map/deque surface as sufficient for freeze. `HashMap::get_mut`, displaced-value `insert`, and `OrderedMap::get_mut` are all landed. `lox` runs end-to-end against the frozen surface; rewriting it onto the canonical `HashMap<String, Value>` + `Vec<Frame>` shape remains useful follow-up evidence, not a blocker.
- **Validated-wrapper close-out**: the local wrapper docs now match the actual compiler state. The freeze-ready surface is the shipped stdlib wrappers (`NonZeroU32`, `NonZeroU64`, `Port`, `AsciiText`) plus the four resolved compiler gaps: native/SSA layout on enum-payload newtypes, cross-module identity, instance-method dispatch on wrappers, and narrowed wrap/unwrap-only cast exemption.
- **Roadmap/docs alignment**: ROADMAP items 57, 67, 72, and 79 now agree with the changelog and local freeze ledgers; `docs/STDLIB_VALIDATION_PLAN.md` and `docs/STDLIB_SURFACE_FREEZE.md` were also updated to stop advertising stale Phase 3 item metadata.
- **Post-freeze cleanup**: the remaining Phase 3 roadmap entries were either closed with explicit rationale or reworded as post-freeze follow-up polish, the stale layout freeze checklist was converted into a close-out note, and the last golden `#` syntax files were migrated to `::` so the frozen syntax record matches the corpus again.

### Newtype-cast exemption narrowed to wrap/unwrap pairs

The first cut of the instance-method-dispatch fix exempted *any* `.cast` where either side named a newtype from CoreCheck's cast-validity table. Reviewer caught the regression: `let x: bool = p as bool` and `let p: Port = b as Port` both compiled, bypassing the validated-wrapper contract in `docs/VALIDATED_WRAPPERS.md §2`. This narrows the exemption to the exact pattern Elab actually inserts: one side is a newtype `N`, and the other side equals `N`'s resolved inner type (after generic-arg substitution). Anything else falls through to the standard validity table and gets E0553 if it's not a legitimate cast on its own.

- **Precise wrapper-pair check**: `Concrete/CoreCheck.lean` builds a `Layout.Ctx` from its newtypes list and uses `Layout.resolveNewtype` to compute the inner type for either side; the cast is exempt only when one resolved side equals the other side. Direction-symmetric (covers both wrap and unwrap).
- **`hasTypeVar` loophole closed**: the cast-validity skip on type variables also matched any `.named _`, including newtypes. Now `.named n` is treated as a type parameter only when `n` is *not* a known struct, enum, or newtype. This was a pre-existing overbroad skip that the dispatch fix surfaced.
- **Negative regressions**: `tests/programs/error_newtype_cast_to_unrelated.con` (Port→bool) and `tests/programs/error_newtype_cast_from_unrelated.con` (bool→Port) now reject at E0553. All 11 positive newtype programs still compile and run; all 5 newtype error tests reject; std/pipeline-test/full-suite baselines unchanged.

### Newtype instance-method dispatch resolves against the wrapper

Calling an instance method on a newtype now resolves against the newtype's inherent impl, not the inner type. This closes the last documented gap in `docs/VALIDATED_WRAPPERS.md §8` and means the static-only `T::try_new(...)` convention is now a stylistic preference rather than a workaround.

- **Type identity flows through elaboration**: `resolveTypeE` no longer erases newtype names. A `let p: Port = ...` binding now keeps `p` typed as `.named "Port"` through the rest of Elab, so `p.value()` mangles to `Port_value` and finds the inherent impl. Layout already resolves named types through `Layout.Ctx.newtypes`, so codegen still sees the right size/alignment.
- **Constructor and `.0` carry the wrapper**: `Port(8080)` now produces a CExpr with type `.named "Port"` (via a representation no-op `.cast`); `p.0` unwraps back to the inner type. For generic newtypes, type args flow from explicit `::<T>` first, otherwise from the call hint (`let w: Wrapper<Int> = Wrapper(100);`).
- **CoreCheck cast policy newtype-aware**: the cast-validity check in `Concrete/CoreCheck.lean` previously rejected `Int → Wrapper<Int>` at E0553. It now skips validation when either side names a newtype — Layout makes the cast a representation no-op, so the cast-policy table doesn't need to enumerate newtype-vs-inner combinations.
- **Aggregate same-type cast lowering**: surfaced as a regression once newtype-over-`String` (`AsciiText`) started flowing through casts. EmitSSA's same-LLVM-type alias path was emitting `add %struct.X, 0` (invalid LLVM) for first-class aggregates. The same-type case now matches on LLVM kind and round-trips structs/arrays/enums through a stack slot.
- **Test**: `tests/programs/newtype_method_dispatch.con` exercises both `p.value()` and `p.is_privileged()` on a `Port`. All 11 newtype regression programs (basic, copy, enum_payload, generic, linear, struct_copy_field, validated, adversarial_consume, module_across, summary_import, method_dispatch) compile and run with correct output. All 3 newtype error tests still reject as expected.

### Cross-module newtype import preserves identity end-to-end

Newtypes now cross module boundaries cleanly, with no inner-type erasure and no special-casing required from callers. This closes the second of the two boundary gaps tracked against `docs/VALIDATED_WRAPPERS.md` (only the inherent-method-dispatch gap remains).

- **Public newtypes are importable**: `pub newtype Port = u16;` now lands in the exporting module's `publicNames` (`Concrete/FileSummary.lean`); `import Wrap.{Port};` resolves and brings inherent impl methods along, mirroring the struct path.
- **No erasure at the boundary**: `resolveImports` previously fed newtypes into the alias map alongside type aliases, which substituted the inner type into every imported signature — so a downstream `Port::try_new` returned `Option<u16>` instead of `Option<Port>`. The alias map is now type-aliases-only; newtype identity is preserved across the boundary, and Layout resolves through `Layout.Ctx.newtypes` natively (the 2026-04-24 fix).
- **Imported newtypes reach Layout**: `ResolvedImports` gains a `newtypes` field; Check, Elab, and the elaborated `CModule.newtypes` all consume it. EmitSSA's `tyToLLVMTy` (a separate path from `Layout.tyToLLVM`) also resolves through newtypes now, fixing a panic that surfaced once the boundary stopped erasing.
- **Test identity preserved**: `tests/programs/adversarial_module_newtype_across.con` exercises the full path (cross-module `Port::try_new`, `Option<Port>` pattern match, `.0` extraction); `tests/programs/summary_import_pub_newtype.con` (previously dead) now runs and returns 42; a new negative repro confirms passing a bare `u16` where `Port` is expected still fails type-check across the boundary.

### Validated wrappers land in stdlib; newtype layout threads through codegen

The newtype surface designed in `docs/VALIDATED_WRAPPERS.md` is now usable on the native/SSA path, and the first canonical wrappers ship in stdlib.

- **Stdlib wrappers**: `std.numeric.NonZeroU32`, `std.numeric.NonZeroU64`, `std.numeric.Port`, and `std.text.AsciiText` land as canonical validated wrappers. Each exposes static-only `try_new` / `try_from_<src>` constructors returning `Option<T>` and preserves the zero-cost `.0` extraction contract. `AsciiText` wraps `String` with the invariant that every byte is in `0..=127`, validated once in `try_new`. 3 new `AsciiText` tests (happy path, empty-OK, reject non-ASCII).
- **Newtype layout fix (native/SSA)**: `Layout.tySize` / `Layout.tyAlign` / `Layout.isPassByPtr` / `Layout.tyToLLVM` previously panicked with `unknown named type 'Port'` whenever a newtype survived Elab's struct/enum-field erasure — for example as the payload of `Option<Port>` or `Option<AsciiText>`. `Layout.Ctx` now carries a `newtypes` list and resolves named/generic types through `resolveNewtype` before querying layout; recurses to handle newtype-of-newtype and substitutes generic args. `CModule` gains a `newtypes : List NewtypeDef` field populated by Elab; `SModule` carries the same, collected across submodules in Lower; EmitSSA state threads newtypes into the Layout context used by `scanBuiltinEnumArgs` and the rest of codegen. `CoreCheck` and `Report` build Layout.Ctx with newtypes too. Check.lean and FileSummary.lean are intentionally untouched — the type-check layer still rejects `Port` vs `u16` without implicit coercion (`tests/programs/error_newtype_no_implicit.con` still rejects).
- **Inherent-impl path now native-clean**: the note in `tests/programs/newtype_validated.con` flagged `impl Port { fn try_new ... }` returning `Option<Port>` as a known layout-bug trigger. That path now lowers, links, and runs; it is the shape the stdlib wrappers use.
- **`std.ordered_map` gains `get_mut`**: paired with the wrapper work because both are part of the Phase 3 stdlib-freeze checklist for runtime collections.
- **Regression coverage**: `std/src/lib.con --test` runs 248/248 including the new `AsciiText` tests; `pipeline-test` 32/32 layout/ABI cases still green; 8 newtype `.con` test programs compile and run; 3 error-path tests (`error_newtype_no_implicit`, `error_newtype_double_unwrap`, `error_newtype_wrong_inner`) still reject as expected.
- **Freeze-ledger update**: `docs/STDLIB_FREEZE_LEDGER.md` validated-wrapper row flips from "Partial — newtype mechanism works at interp; native/SSA layout bug on enum-payload newtypes" to "Complete"; `docs/VALIDATED_WRAPPERS.md §8` drops the `Layout.tySize`/`Layout.tyAlign` gap (the instance-method-dispatch gap remains, by design).

### Canonical Result/Option surface and `Type::Variant` qualification

Concrete now has one public `Result`/`Option` story and one enum/static qualification syntax:

- **Canonical builtins**: `Result<T, E>` and `Option<T>` are the language-level builtins; `std.result` and `std.option` now provide helper impls on those builtins instead of redefining duplicate public enums
- **Canonical `Result` shape**: builtin `Result` now uses `Ok { value }` and `Err { error }` everywhere, matching the stdlib helper surface and the `?` operator
- **Canonical qualification syntax**: enum construction, enum patterns, and static method calls now use `Type::Variant` / `Type::method(...)`; the compiler, formatter, interpreter pretty-printer, examples, stdlib, tests, and docs were migrated off `#`
- **Canonical examples updated**: `parse_validate` and `service_errors` now use builtin `Result` directly; the old `ParseResult`, `ValidateResult`, `AuthResult`, `RateResult`, and `ServiceResult` surfaces are gone from the shipped examples
- **Follow-on roadmap honesty**: the roadmap and phase checklist now track only the remaining syntax work honestly — field punning, ignore/rest patterns, and destructuring forms are still pending

### Source-level interpreter / semantic oracle (Phase 1, item 31)

`Concrete/Interp.lean` — source-level interpreter operating on validated Core IR. CLI: `concrete <file.con> --interp`. Evaluates the predictable/core subset without codegen (no LLVM, no clang).

- **Supported**: integer/bool, let/assign, if/else, function calls, structs (creation + field access + field assign), enums (creation + match with field bindings), arrays (literal + indexing + index assign), bounded for/while loops, cast, binary ops (arithmetic + comparison + bitwise XOR/AND/OR), unary ops, break/continue
- **Unsupported (explicit diagnostics)**: borrow, deref, float, string, char, defer, try, alloc, whileExpr, fnRef
- **First target**: `examples/parse_validate/` — all 8 tests pass, exit code matches compiled binary
- **Trust-gate**: 12 interp tests (parse_validate, function calls, array loops, structs, enum match, XOR bitwise, unsupported diagnostic, compiled-vs-interpreted comparison, scope isolation, negative index, observable contract, match-arm mutation). Total trust-gate: 1404 checks
- **Key design**: fuel-based evaluation with explicit control flow propagation (return/break/continue as `Flow` inductive). For-loop step field only runs on `continue` (step already appended to body by desugaring)

### Interpreter semantic fixes (3 bugs)

- **Scope isolation**: if-branches and else-branches now restore outer scope after evaluation — block-local `let` bindings no longer leak. Fix: trim env back to outer length after branch (`Interp.lean:376`).
- **Negative array index**: `arrayIndexAssign` now checks `i < 0` before `toNat` conversion, producing "negative array index" error instead of silently writing element 0 (`Interp.lean:404`).
- **Observable contract**: `--interp` now prints the return value to stdout and exits 0, matching compiled binary behavior. Previously it returned the value as a process exit code and printed nothing (`Main.lean:203`).
- **Match-arm mutation**: statement-level match now propagates outer-variable mutations via `evalMatchStmt`. Block-local and arm bindings are still trimmed. Previously `evalMatchBody` discarded the env entirely (`Interp.lean:337`).

### No-std split, standalone UX, project bootstrap (Phase 1, items 34-36)

Three design documents defining developer-facing UX for different project shapes:

- **`docs/FREESTANDING_SPLIT.md`** (item 34): freestanding vs hosted execution targets. Freestanding = no stdlib, no capabilities, no libc, pure computation. Hosted = full stdlib, main entry point, all capabilities. Feature table (18 items), concrete examples from fixed_capacity (freestanding-ready) and parse_validate (hosted). Freestanding is a strict subset of predictable.
- **`docs/STANDALONE_VS_PROJECT.md`** (item 35): standalone file (`concrete myfile.con`) vs project (`concrete build` with Concrete.toml). Standalone for scripts/snippets, project for stdlib/policy/modules. Proposed `--stdlib` flag bridge. Migration path from standalone to project.
- **`docs/PROJECT_BOOTSTRAP.md`** (item 36): `concrete new` command design with 3 templates (predictable, library, ffi), standard layout conventions, Concrete.toml field reference, first outsider workflow.

Phase 1 (Predictable Core) now has 17/18 items done. Only item 31 (interpreter) remains.

### Per-phase exit checklists (Phase 1, item 40)

`docs/PHASE_EXIT_CHECKLISTS.md` — exit criteria for phases 1-7:

- **Phase 1 (Predictable Core)**: 10 criteria — canonical examples trust-gate tested, boundaries documented, stack-depth working, error propagation patterned, example governance, diagnostic UX designed, trusted boundary guide, no-std split, interpreter. Current: 14/18 items done
- **Phases 2-7**: concrete exit criteria tied to specific outputs (pressure examples, stdlib freeze, tooling, benchmarks, showcase, proof expansion)
- **Phases 8-14**: deferred until earlier phases approach completion
- **Update protocol**: check boxes as criteria are met, add commit hash, close phase when all checked

### Diagnostic UX and trusted boundary guide (Phase 1, items 32-33)

Two design documents establishing quality standards:

- **`docs/DIAGNOSTIC_UX.md`** (item 32): diagnostic quality tiers (good / missing "why" / bare), target format (rule + location + why + hint), 4 priority categories (policy violations, extraction blockers with per-construct explanations, ownership/linearity, stale proof repair). No new Lean types needed — existing Diagnostic fields sufficient
- **`docs/TRUSTED_BOUNDARY_GUIDE.md`** (item 33): 4 canonical wrapper patterns (raw pointer reads, FFI shell, safe alternative, multi-layer orchestration), audit checklists for trusted fn (7 items) and trusted extern fn (4 items), report/evidence material references

### Example inventory, lifecycle, and no-duplicate rule (Phase 1, items 37-39)

Three documentation items establishing governance for the example set:

- **`docs/EXAMPLE_INVENTORY.md`** (item 37): canonical inventory of all 20 named examples — 3 flagship (crypto_verify, elf_header, proof_pressure), 5 canonical (fixed_capacity, parse_validate, service_errors, thesis_demo, packet), 10 pressure (grep, http, integrity, json, kvstore, lox, mal, policy_engine, toml, verify), 2 supporting (project, snippets). Each entry records path, owning phase, claim, oracle strategy, test gates, and promotion status. Multi-phase ownership table. Promotion log.
- **`docs/EXAMPLE_LIFECYCLE.md`** (item 38): 4 promotion levels (pressure → canonical → flagship → permanent regression target) with explicit bars, promotion path, anti-patterns (unnamed workload, test-free canonical, duplicate workload, phantom flagship), and new-example checklist.
- **`docs/EXAMPLE_NO_DUPLICATES.md`** (item 39): rule against creating near-duplicate examples; reuse with multi-phase ownership. When-to-reuse vs when-to-create criteria. Current near-duplicates to watch.

### Service-style error propagation example (Phase 1, item 30)

`examples/service_errors/` — 4-stage service-style request handler for the predictable subset:

- **4-stage pipeline**: validate → authorize → rate-limit → process, each with its own error enum
- **3 stage-specific error enums**: `ValidateError` (BadUserId, BadAction, PayloadTooLarge), `AuthError` (InvalidToken, InsufficientPermission), `RateLimitError` (QuotaExceeded), plus unified `ServiceError` with deterministic error codes (101-103, 201-202, 301)
- **Public surface**: `Request` struct, `Response` struct, stage-specific error enums, unified `ServiceError`, and builtin `Result<T, E>` for every stage/pipeline return
- **Pure functions**: 12 functions, all `caps: (pure)`, `evidence: enforced`, zero trusted, zero allocation
- **Policy enforced**: `predictable = true` in Concrete.toml, all functions pass `--check predictable`
- **9 runtime tests**: success path, 3 validation failures, 2 auth failures, 1 rate limit, admin action, action-2 path
- **4 pipeline adversarial programs**: stage conversion with unified AppError, severity classification with is_fatal/is_retriable, partial success with intermediate state preservation, fan-in first-failure reporting
- **Known lli bug**: `adversarial_pipeline_partial_success.con` crashes lli (LLVM 21.1.8 JIT) on complex enum variant with struct+enum payload; native compilation works correctly — interpreter-only issue, not a Concrete codegen bug
- **10 trust-gate serviceerrors tests**: build, run, predictable check, effects, trusted count, allocation, error code functions, pipeline handlers, policy, purity
- **Trust-gate total**: 1358 checks

### Canonical parse/validate error-flow example (Phase 1, item 29)

`examples/parse_validate/` — canonical error-flow example for the predictable subset:

- **Public surface**: `enum Copy ParseError` (6 categories: TooShort, BadVersion, BadType, PayloadTooBig, Truncated, BadChecksum), `struct Copy Header`, and builtin `Result<Header, ParseError>`
- **Explicit propagation**: match-based error handling, every error path visible in source, no hidden control flow
- **Pure functions**: 9 functions, all `caps: (pure)`, `evidence: enforced`, zero trusted, zero allocation
- **Bounded**: `compute_checksum` XOR fold over fixed-size array with bounded loop; all others loop-free
- **Policy enforced**: `predictable = true` in Concrete.toml, all functions pass `--check predictable`
- **8 runtime tests**: valid header, bad version, bad type, too short, payload overflow, bad checksum, valid with payload, truncated — all pass (exit 0)
- **SSA-verify bug discovered**: accumulator-style match (many sequential `match` + mutable counter) triggers E0703 dominator violation; early-return style works correctly
- **10 trust-gate parsevalidate tests**: build, run, predictable check, effects, trusted count, allocation, enum compilation, bounded loop, policy, purity

### Predictable failure discipline (Phase 3, item 28)

`docs/PREDICTABLE_FAILURE_DISCIPLINE.md` — failure-only discipline for predictable-profile code:

- **Allowed**: explicit `Result<T, E>` return, error codes (`i32`, enum), sentinel values, `?` propagation
- **Excluded**: abort, OOM, panic, unwinding, exceptions, blocking I/O failure, FFI failure, `longjmp` — all blocked by the five predictable gates
- **Remaining gaps**: integer overflow (silent wrap) and array OOB (UB) — neither is hidden control flow
- **Verification**: `--check predictable` + `--report effects` + `--report stack-depth`
- **PROFILES.md updated**: stack-depth and failure discipline rows now show "Working"/"Defined"

### Failure strategy (Phase 3, item 27)

`docs/FAILURE_STRATEGY.md` — definitive failure/panic strategy:

- **Abort-only**: no panic, no unwinding, no catch — permanent design commitment
- **Cleanup guarantees**: defer runs LIFO on all normal exits (return, `?`, break, scope exit); skipped on abort/signal
- **No-leak on normal paths**: linear ownership + defer; leak-on-abort acceptable (OS reclaims)
- **Failure taxonomy**: explicit errors (`Result`), abort (OOM/user/stdlib), hardware traps (SIGSEGV/SIGFPE), undefined behavior (array OOB, integer overflow)
- **FFI consequences**: trust-based contracts, `longjmp` is UB, no cleanup for extern-triggered abort
- **Proof assumptions**: proved code avoids all failure sources by construction; integer overflow and array OOB are known gaps between proof model and runtime
- **8 summary commitments** for the failure model

### Predictable/proved boundary classification (Phase 3, item 26)

`docs/PREDICTABLE_BOUNDARIES.md` — comprehensive classification of runtime boundaries for predictable and proved code:

- **Host calls**: only `write(2)` reachable from predictable code (Console); all heap, string, Vec, FFI, and blocking operations excluded by five gates
- **Cleanup paths**: defer runs LIFO on normal return/scope exit; abort/OOM/signals not reachable; all data stack-only
- **Determinism**: source-level shape deterministic (bounded loops, acyclic calls); timing, binary layout, cache behavior explicitly not claimed
- **Failure paths**: integer overflow wraps silently (LLVM default), array OOB is UB (no runtime check), stack overflow via OS guard page; OOM and abort not reachable
- **Memory/UB boundaries**: ownership enforcement prevents UAF/double-free/null-deref/dangling-ref in safe code; array OOB, overflow, division by zero remain as UB sources
- **Proved functions**: Lean theorem over PExpr, not binary; unbounded integer gap; per-function scope; no composition theorem
- **Profile interaction**: summary table showing safe × predictable × proved property matrix
- **Verification commands**: 8 CLI commands mapped to specific audit questions

### Stack-depth reporting (Phase 3, item 25)

`--report stack-depth` for functions that pass the no-recursion profile:

- **Per-function analysis**: frame size (bytes) from params + locals via `Layout.tySize`, max call depth from acyclic call graph, worst-case stack bound (sum of frame sizes along deepest chain)
- **Recursive handling**: recursive functions shown as "unbounded" — only non-recursive functions get numeric bounds
- **Implementation**: `stackDepthReport` in Report.lean, `collectLocalTys` collects all local variable types from function bodies (recursive into nested scopes), `computeCallDepths` does memoized DFS on the call graph
- **Wired**: `--report stack-depth` dispatch in Main.lean, added to usage string
- **12 trust-gate stackdepth tests**: report structure, frame bytes, call depth, stack bounds, leaf vs caller depth, bound >= frame invariant, totals, max stack, fixed_capacity 0 recursive, source locations

### Copy struct array fix + safe-indexing rewrite (Phase 3, item 24)

Compiler bug fix + example rewrite eliminating most trusted code from `examples/fixed_capacity/`:

- **Compiler fix**: `isCopyTy` (CoreCheck.lean) and `isCopyTyPostMono` (Verify.lean) both had missing `.array` case — Copy structs with fixed-array fields were rejected. Added recursive array element Copy checking to both functions.
- **Safe-indexing rewrite**: example now uses `struct Copy MsgBuf { data: [u8; 256], len: i32 }` and `struct Copy RingBuf { data: [i32; 16], head: i32, count: i32 }` with safe `arr[i]` syntax — no pointer arithmetic needed
- **Trust boundary shrunk**: 16 functions now `trusted: no, evidence: enforced` (was: 13 trusted); only 4 test-packet builders remain trusted
- **20 functions** total, predictable profile passes, zero allocation, 8 runtime tests pass
- **Trust-gate tests updated** for new function signatures (validation core not trusted, only builders trusted)

### Fixed-capacity validation (Phase 3, item 23)

`examples/fixed_capacity/` — bounded message validator with ring buffer for replay detection:

- **20 functions**, all pass `--check predictable`, `predictable = true` policy enforced
- **Ring buffer**: `struct Copy RingBuf { data: [i32; 16], head: i32, count: i32 }` with safe array indexing — replay detection for recent sequence numbers
- **Message validator**: 8-byte fixed header (version, type, seq, payload_len, XOR tag), 5 pure validation functions, bounded tag computation
- **Trust boundary**: only 4 test-packet builders are trusted; 16 functions (validators, ring buffer, byte readers) are fully safe with `evidence: enforced`
- **8 runtime tests**: valid data/heartbeat/reset, replay rejection, too_short, bad_version, bad_type, payload_overflow
- **Validated findings**: fixed arrays + safe `arr[i]` indexing + Copy structs + bounded for loops = practical no-alloc pattern for real bounded-state processing
- **12 trust-gate fixedcap tests**: build, run, predictable pass, evidence levels, trust classification, bounded loops, zero allocation, proof eligibility, extraction gaps, policy declaration, capability-free validation core

### Active roadmap reset after former phases 1-2

Completed former roadmap Phases 1 and 2 are now treated as historical milestones rather than active plan items:

- Former **Phase 1** compiler-integrity work and former **Phase 2** proof-workflow work remain recorded in this changelog
- The active roadmap now starts at former Phase 3, renumbered as active **Phase 1**
- Task numbering did not restart; active work still begins at item `23`
- This is a roadmap cleanup, not a semantic change in project scope
- The remaining roadmap now has **14 active phases**

### Trusted-computing-base accounting (Phase 2, item 22)

`docs/TRUSTED_COMPUTING_BASE.md` rewritten as canonical TCB reference:

- **6 TCB layers**: Concrete checker/compiler, Lean kernel, proof registry/fingerprint machinery, backend/toolchain, runtime/target/OS, trusted/foreign code boundaries
- Each layer specifies: what is trusted, what is actively verified, what is not claimed
- **Profile-specific TCB**: new section maps each profile (safe, predictable, provable, high-integrity) to its TCB dependencies with explicit weights (critical/assumed/gap/boundary)
- **4 strongest-claim breakdowns**: safe-code guarantees, theorem claims, predictable claims, workflow determinism — each with per-layer status
- References proof-bundle assumptions, CI proof gate, adversarial test coverage, fingerprint determinism, registry validation
- Remaining trust surfaces explicit: checker soundness, PExpr extraction fidelity, eval soundness, integer-representation gap
- Docs-only change — no new code, no test changes

### Named user-facing profiles (Phase 2, item 21)

`docs/PROFILES.md` rewritten as canonical profile reference:

- **4 profiles** with per-profile gate/report/evidence tables
- **Safe**: 7 enforced gates, 5 report commands, evidence artifacts; explicit non-coverage (bounds, overflow, stack, termination, checker soundness)
- **Predictable**: 5 gates (recursion, loops, allocation, blocking, FFI) with advisory and policy-enforced modes; JSON violation facts; explicit today-vs-remaining table
- **Provable**: 10 eligibility gates, 6 obligation states, 3 theorem shapes, 10 report commands; evidence bundle + CI gate; precise "proved" semantics and non-claims
- **High-integrity**: named direction with intended restrictions; nothing profile-specific implemented yet
- **Relationship diagram** showing profiles overlap without being strict subsets
- Each profile carries explicit status (real/partial/future) and claim class
- Docs-only change — no new code, no test changes

### Canonical "claims today" surface (Phase 2, item 20)

`docs/CLAIMS_TODAY.md` rewritten as the canonical short public statement of current guarantees:

- **9 sections**: compiler-enforced guarantees, proof-backed guarantees, proof workflow and evidence pipeline, compiler-reported analysis, profile status, explicit non-claims, trusted computing base, vocabulary rules, machine-readable evidence
- **Compiler-enforced**: 12 properties (ownership, linearity, borrows, capabilities, cleanup, control flow) with mechanisms; explicit non-coverage (bounds, overflow, stack, termination, checker soundness)
- **Proof-backed**: eligibility gates, 3 theorem shapes (concrete test, boundary, full contract), 6 obligation states, precise meaning of "proved" (PExpr + unbounded Int + fingerprint match), 5 explicit non-claims (binary correctness, checker soundness, composition, coverage, all-properties)
- **Proof workflow**: 10 working capabilities (extraction, lean-stubs, registry, kernel verification, proof-status, diagnostics with E0800–E0807, dependency graph, evidence bundle, `concrete check`, CI proof gate); rename and stale detection
- **Profiles**: safe (real, enforced), predictable (partially real), provable (real for current subset), high-integrity (named direction)
- **TCB**: 6 layers (compiler, Lean kernel, registry/fingerprint, backend/toolchain, runtime/target/OS, trusted/foreign boundaries)
- **Machine-readable**: JSON facts for proof_status, obligation, evidence, eligibility, proof-bundle, diagnostics-json
- Every claim carries its taxonomy class; cross-references 8 detailed documents
- Docs-only change — no new code, no test changes

### Proof evidence CI gate (Phase 2, item 19)

`scripts/ci/proof_gate.sh` runs the complete proof workflow as one CI gate:

- **20 checks across 8 sections**: extraction (4), registry validation (2), proof-status consistency (3), proof diagnostics (3), proof dependencies (2), evidence bundle (2), determinism (2), Lean theorem checking (2)
- Runs against the proof pressure set (`examples/proof_pressure/`)
- One exit code: 0 if all pass, 1 if any fail
- Wired as `proof-gate` job in GitHub Actions CI (`.github/workflows/lean_action_ci.yml`), runs in parallel with trust-gate
- `make test-proof-gate` Makefile target for local use
- 7 trust-gate tests verify: script exists/executable, passes on pressure set, runs all 20 checks, covers all 8 sections, shows compiler identity, CI workflow wired, Makefile target present

### Proof evidence bundle (Phase 2, item 18)

`--report proof-bundle` produces a single JSON evidence bundle for review, CI, and release evidence:

- **Metadata**: source path, timestamp, compiler identity (version + git commit + Lean toolchain)
- **Summary**: obligation counts by status (proved/stale/missing/blocked/ineligible/trusted), extraction counts, diagnostic severity breakdown, dependency stats
- **Assumptions**: explicit section documenting proof model (PExpr + unbounded Int), unverified compilation chain, integer model gap, per-function composition limitation, checker soundness assumption, fingerprint stability
- **Registry**: full proof-registry.json entries with function, fingerprint, proof name, spec name
- **Dependency graph**: proved and stale dependency edges between functions
- **Facts**: all proof-related facts (proof_status, obligation, extraction, proof_diagnostic, eligibility) — non-proof facts excluded
- 15 trust-gate tests verify: schema version/kind, source/compiler/timestamp metadata, summary obligation counts, extraction counts, diagnostic severity counts, assumptions fields, registry entries, dependency graph edges (proved and stale), all 5 fact kinds present, fact_count consistency, clean-file baseline, exit codes, non-proof fact exclusion

### "How to Prove a Concrete Function" tutorial (Phase 2, item 17)

Canonical outsider-facing tutorial added to the book (`docs/book/src/proving.md`):

- Step-by-step walkthrough proving `clamp_value` from the proof pressure set
- Real compiler output at every step: extraction, proof-status, lean-stubs, check-proofs, proof-diagnostics, proof-deps
- Explicit claim scope section: what is proved (Lean-kernel-checked PExpr property) and what is NOT (binary correctness, overflow, safety, composition)
- Recovery guide: stale proofs, ineligible functions, blocked extraction, detailed diagnostics
- Maintenance: dependency tracking, refactor survival table, rename detection, daily `concrete check` workflow
- Quick-reference command table for all proof-related commands
- Added to book SUMMARY.md table of contents

### Proof-authoring and maintenance workflow (Phase 2, item 16)

Complete user-facing proof workflow defined and tested:

- `docs/PROOF_WORKFLOW.md` — 12-step canonical workflow document covering: function eligibility, stub generation, proof writing, registry attachment, verification (`concrete check`, `check-proofs`), failure diagnosis (8 diagnostic kinds), stale repair, dependency fallout review, and proof-preserving refactors
- Command reference table for all 9 proof-related CLI commands
- End-to-end example: proving `clamp_value` from the pressure set
- Rename detection workflow: orphaned registry entries matched by fingerprint
- Refactor checklist: which changes preserve proofs, which invalidate, and how to recover
- 15 trust-gate workflow tests: extraction reports, lean-stubs generation, proof-status states, proof-diagnostics classes, proof-deps edges, fingerprint consistency, stale repair cycles, rename detection, obligation totals, and diagnostic-codes coverage

### Refined proof failure taxonomy (Phase 2, item 15)

Proof diagnostics now carry fine-grained failure and repair classifications covering the full proof pipeline:

- **8 diagnostic kinds**: staleProof (E0800), missingProof (E0801), ineligible (E0802), unsupportedConstruct (E0803), trusted (E0804), attachmentIntegrity (E0805), theoremLookup (E0806), leanCheckFailure (E0807)
- **10 failure classes**: `stale_proof`, `missing_proof`, `unsupported_construct`, `effect_boundary`, `structural_gate`, `entry_point`, `trusted_boundary`, `attachment_integrity`, `theorem_lookup`, `lean_check_failure`
- **6 repair classes**: `theorem_update`, `add_proof`, `code_rewrite`, `policy_change`, `registry_update`, `none`
- `IneligibleCategory` enum for typed sub-classification (replaces brittle substring checks)
- `registryIssuesToDiagnostics` converts all RegistryIssue variants into attachment_integrity diagnostics, wired into `extractProofCore`
- `checkProofResultsToDiagnostics` converts check-proofs failures into theorem_lookup/lean_check_failure diagnostics
- `concrete diff` disambiguates multiple diagnostics per function using `diagnostic_kind` as suffix key
- Text rendering shows `failure:` and `repair:` lines; JSON includes `failure_class` and `repair_class`; schema updated
- 15 adversarial taxonomy tests verify all failure/repair classes in text and JSON, attachment_integrity generation from stale registry, E0805-E0807 in diagnostic-codes, and diff deduplication

### Proof dependency and composition semantics (Phase 2, item 14)

Proof dependencies are now explicit and propagated:

- `staleDeps` field on obligations tracks which proved callees have gone stale
- `--report proof-deps` shows the full dependency graph:
  ```
  main.validate_header [proved]
    → main.check_nonce (proved)

  main.main [ineligible]
    → main.validate_header (proved)
    → main.compute_checksum (stale)
  ```
- Obligations report renders `stale deps:` inline when present
- JSON `diagnostics-json` includes `stale_deps` array on obligation facts
- INV-14 consistency check validates staleDeps correctness
- 11 adversarial tests cover dependency graph output, stale propagation, JSON fields

### Proof attachment stability under refactors (Phase 2, item 13)

Proof attachment rules are now explicit and tested. The stability semantics:

- **Proof survives**: qualified name unchanged + body fingerprint unchanged (comments, whitespace, adding sibling functions don't affect it)
- **Proof goes stale**: body changes (operator change, variable rename, helper extraction, added/removed statements)
- **Proof lost (rename detected)**: function renamed but body unchanged — registry validation now detects this via fingerprint matching and reports "appears renamed to 'new.name'" with an update hint
- **Proof lost (removed)**: function deleted entirely — reported as "unknown function (removed or renamed)"

10 adversarial stability tests cover all scenarios.

### User-package proof scoping (Phase 2, item 12)

Proof reporting and policy enforcement now scope to user/package modules only. Stdlib and dependency obligations are filtered out of all output surfaces:

- `concrete build` summary counts only user functions: `Proofs: 2 proved, 1 stale, 1 missing, 1 blocked`
- `concrete check` report shows only user functions with a footer: `(324 dependency obligations hidden)`
- `require-proofs = true` enforces only on user-package obligations
- Exit codes based on user obligations only — stdlib blocked/missing won't fail your build
- `ProofCore.scopeToUser` utility filters by qualified-name prefix against `depNames`

### Coherent build / check / prove workflow (Phase 2, item 11)

`concrete build` now shows a proof summary line after every successful build:
```
Built myproject
Proofs: 2 proved, 1 stale, 7 missing, 53 blocked
```

`concrete check` is a new project-level command that runs the frontend and proof pipeline without codegen:
- Prints the full proof-status report (same as `--report proof-status`)
- Shows prioritized "Next steps" (stale first, then missing, then blocked — at most 3 items)
- Exits 0 if all eligible functions proved, exits 1 if any stale/missing/blocked
- Build exit code is NOT affected by proof status (only policy violations cause non-zero)

Internal refactor: `loadProject` shared function eliminates 60-line duplication between `compileBuild`, `compileTestBuild`, and the new `compileCheck`.

10 new workflow tests: build summary presence/content, build exit code (0 despite stale), check report/next-steps/exit-code/totals/error-handling.

Trust-gate: 1189 pass, 0 fail.

### Blocked/ineligible proof pressure tests (Phase 2, item 10)

Blocked and ineligible proof diagnostics now explain exactly why a function cannot be proved:

- **Blocked diagnostics** list specific unsupported constructs: struct literal, match expression, mutable assignment, string literal, if-without-else. Previously said only "unsupported constructs" without detail.
- **Ineligible diagnostics** list specific reasons: capabilities (File, Network, Process, Unsafe, Alloc), recursion (direct, mutual), FFI, allocation, blocking I/O, entry point, trusted. Previously suggested "Remove is entry point (main)" — now says "Address these constraints."
- **JSON output** (`diagnostics-json`) includes `unsupported` array for blocked and `profile_gates` array for ineligible functions.
- **Registry validation** rejects entries targeting blocked functions (extraction-blocked error) and ineligible functions (ineligible function error).

29 new adversarial pressure tests: 14 ineligible-reason coverage (each capability, recursion type, FFI, allocation, combo, entry point, trusted, boundary), 8 blocked-construct coverage (each unsupported construct type, boundary), 7 boundary tests (registry-targeting errors, combined reasons, consistency invariants, JSON fields).

Trust-gate: 1179 pass, 0 fail.

### Lean kernel checking and stale-proof repair (Phase 2, items 7-9)

`--report check-proofs` invokes the Lean kernel to verify that proof theorems referenced in the registry or hardcoded list actually exist and type-check:
- Generates a temporary Lean file with `#check @TheoremName` for each proved/stale obligation
- Invokes `lake env lean` using the project's pinned toolchain
- Reports per-theorem results: verified or failed (with exit code 1 on any failure)
- Shows Lean toolchain version for reproducibility

`provedFunctions` extended from `(name, fingerprint)` pairs to `(name, fingerprint, theoremName)` triples — hardcoded proofs now carry explicit Lean theorem references instead of generated `_correct` suffixes.

End-to-end Lean attachment tests verify consistency across proof-status, obligations, extraction, and check-proofs reports. Stale-proof repair tests cover the full cycle: proved → mutate → stale (with fingerprint drift) → update registry → proved again → kernel check passes.

Trust-gate: 1147 pass, 0 fail.

### Registry integrity validation (Phase 2, item 6)

`validateRegistry` checks proof-registry.json entries against ProofCore state:
- **Unknown function**: registry names a function not in the compiled module (error)
- **Ineligible target**: registry targets a function that fails the predictable profile (error)
- **Extraction blocked**: registry targets a function with unsupported constructs (error)
- **Empty proof/spec name**: registry entry has blank proof or spec identifier (error)
- **Duplicate/conflicting entries**: multiple entries for the same function (warning/error)
- **Stale fingerprint**: body changed since proof was registered (warning)

`RegistryIssue.isError` distinguishes errors from warnings. Proof-sensitive reports (`proof-status`, `obligations`) return exit code 1 when registry errors are present. Hardcoded proofs (from `Proof.provedFunctions`) bypass extraction gate — proved status is correct even when extraction fails for hardcoded entries.

Adversarial test files in `tests/programs/adversarial_registry/` with 6 trust-gate assertions. Trust-gate: 1131 pass, 0 fail.

### Canonical theorem shapes (Phase 2, items 3 and 5)

`docs/PROOF_THEOREM_SHAPES.md` defines the canonical proof-spec and theorem shapes:
- 3 theorem categories: concrete tests (`native_decide`), universal boundary, full contract
- Naming rules: `<fn>Expr`, `<fn>Fn`, `eval_<fn>`, `<fn>_correct`, `<fn>_rejects_<X>`
- Allowed property forms: total input-output, branch coverage, compositional
- Explicit non-goals: loop invariants, effect proofs, refinement types, temporal/liveness, float/string
- Fuel convention, proof tactics, readability standards

### Lean theorem stub generation (Phase 2, item 4)

`--report lean-stubs` generates ready-to-use Lean source from ProofCore extraction:

- **PExpr definitions**: Each extracted function gets a `fooExpr : PExpr` definition with Lean constructors matching the extracted form
- **PFnDef entries**: Name, parameter list, and body reference
- **Function table**: `generatedFns : FnTable` with all extracted functions
- **Eval helpers**: `eval_foo` wrappers binding parameters to the function table
- **Theorem stubs**: `theorem foo_correct ... = sorry` with correct parameter signatures, env bindings, and ProofCore docstring

Only extractable functions appear — excluded (capabilities) and blocked (unsupported constructs) are omitted. 10 new trust-gate assertions. Trust-gate: 1125 pass, 0 fail.

### Proof pressure set inspectable end-to-end (Phase 2, item 2)

Fixed effects report evidence discrepancy: `--report effects` was computing evidence from hardcoded `Proof.provedFunctions` instead of using `pc.obligations` (the single source of truth that includes registry proofs). `check_nonce` and `validate_header` now correctly show `evidence: proved` instead of `evidence: enforced (proof stale: body changed)`.

24 trust-gate assertions verify the pressure set across all report modes:
- **Extraction** (6): ProofCore forms for `check_nonce`/`clamp_value`, extraction failure for `classify_range` with field-access blocker, Console exclusion for `format_result`, totals (4/1/2)
- **Obligations** (4): dependency tracking (`validate_header` → `check_nonce`), registry source, missing spec, totals match proof-status
- **Eligibility** (2): totals (5/2), entry-point exclusion
- **Effects/evidence** (3): proved evidence for registry-backed proofs, stale annotation, totals (2 proved)
- **Determinism** (2): fingerprints and extraction stable across runs

Trust-gate: 1115 pass, 0 fail.

### Proof pressure set defined (Phase 2, item 1)

6-function proof pressure set in `examples/proof_pressure/` exercising all proof obligation states:

- **`check_nonce`** (proved) — conditional-heavy validator with correct registry fingerprint
- **`validate_header`** (proved) — helper-composition, calls `check_nonce`, correct fingerprint
- **`compute_checksum`** (stale) — registry has old fingerprint from before `+ 1` body edit
- **`format_result`** (ineligible) — has `Console` capability, outside proof boundary by design
- **`clamp_value`** (missing) — pure, extractable, no registry entry yet
- **`classify_range`** (blocked) — pure, eligible, but uses struct field access (unsupported in extraction)

Design doc: `docs/PROOF_PRESSURE_SET.md`. Registry: `examples/proof_pressure/src/proof-registry.json`. 7 new trust-gate regression assertions verify all states + totals. Trust-gate: 1098 pass, 0 fail.

### Stable diagnostic and error-code taxonomy

All compiler diagnostics now carry stable error codes, documented with machine-readable listings.

- **172 error codes** assigned across all 10 compiler passes: parse (E0001), resolve (E0100–E0111), check (E0200–E0285), elab (E0400–E0419), core-check (E0500–E0582), verify (E0600–E0601), lower (E0602), policy (E0610–E0612), ssa-verify (E0700–E0715), proof (E0800–E0804)
- **`code` field** added to `Diagnostic` structure; rendered in human-readable output as `error[pass]: (E0XXX) message`
- **Proof diagnostic JSON** facts include `code` field for machine consumers
- **`--report diagnostic-codes`** produces machine-readable JSON listing all codes with pass, severity, description, severity meanings, and compatibility rules
- **Compatibility contract**: codes are stable identifiers; never reassigned; new codes may be added; retired codes never reused; consumers should tolerate unknown codes
- 7 new error-code trust-gate tests; full suite: 2575 pass, 0 fail; trust-gate: 1074 pass, 0 fail

### Fact/query JSON API versioning

All machine-readable JSON output is now versioned with stable schema contracts.

- **Versioned envelope**: `diagnostics-json` and all fact-filter queries wrap results in `{schema_version, schema_kind, fact_kinds, fact_count, facts}` instead of bare arrays
- **Query answers versioned**: all `query_answer` objects include `schema_version` field
- **Snapshot versioned**: `concrete snapshot` output includes `schema_version` alongside existing `version` field
- **Schema report**: `--report schema` produces a machine-readable JSON schema definition documenting all 11 fact kinds, 7 query kinds, required/optional fields per kind, location encoding, envelope shapes, and compatibility rules
- **Policies documented**: empty-result returns envelope with `fact_count: 0` (not an error); malformed queries return plain-text error on stderr; consumers should ignore unknown fields for forward-compatibility; adding optional fields does not bump `schema_version`
- 9 new api-versioning trust-gate tests; full suite: 2568 pass, 0 fail; trust-gate: 1067 pass, 0 fail

### Canonical qualified function identity across all fact families

All machine-readable fact kinds now use consistent qualified function names (e.g. `"main.parse_byte"` instead of `"parse_byte"`).

- **Previously inconsistent**: `effects`, `capability`, `unsafe`, `alloc`, `predictable_violation` used bare names while `proof_status`, `obligation`, `extraction`, `traceability`, `eligibility`, `proof_diagnostic` used qualified names
- **Now**: every `"function"` field in every fact kind uses the fully qualified `module.name` form
- **FnEffects** and **ProfileViolation** structures now carry both `name` (bare, for display) and `qualName` (qualified, for facts)
- **Query matching**: `fn:FUNCTION` and per-kind queries accept both bare and qualified names via suffix matching
- **Fact generators updated**: `effectsForModule`, `capToFact`, `collectUnsafeFactsModule`, `allocToFact`, `violationToFact` all thread module path for qualification
- Full test suite: 2559 pass, 0 fail

### Phase 1 infrastructure closure: miscompile workflow and regression promotion

The initial compiler-integrity workflow that used to live at the top of Phase 1 is now historical rather than active roadmap work:

- **Miscompile-hunting workflow infrastructure**: the repo now has the basic machinery for reducing, capturing, and preserving compiler failures as named artifacts instead of tribal memory. The key pieces are `concrete reduce`, `concrete debug-bundle`, the numbered bug ledger in `docs/bugs/`, and the adversarial/wrong-code regression corpus.
- **Crash/miscompile-to-regression promotion policy**: every numbered bug is required to have named regression coverage, enforced by CI via the bug-corpus audit gate.

This closes the workflow/promotion half of the original roadmap item. Full oracle-based wrong-code discovery remains deferred to the later reference-interpreter / semantic-oracle work.

### Compiler-as-a-contract cleanup

Systematic removal of silent fallbacks, implicit downgrades, and "best effort" behavior across the report, proof, and codegen layers.

- **Report.lean fact comparison**: missing JSON fields now show as `<missing>` instead of empty string; `compareFacts` and `classifyNewFact` distinguish missing data from real values
- **Report.lean drift classification**: unknown fact kinds in new facts classified as `weakened` (not silently `neutral`); unknown extraction statuses ranked at 0 (not collapsed into `excluded`)
- **Report.lean proof state**: missing proof entries now report `missing` instead of `unknown`; missing fingerprints report `<missing>` instead of empty string
- **ProofCore.lean registry parsing**: replaced `head!` crash with safe extraction; `extractStr` returns `Option` — missing JSON fields produce per-field warnings instead of silent empty strings; empty function values explicitly warned
- **ProofCore.lean recursion classification**: functions missing from SCC analysis classified as `unclassified` (not silently `none`)
- **Main.lean snapshot**: mono/lower pipeline failures now emit explicit `warning: snapshot incomplete` diagnostic instead of silently dropping traceability facts
- **EmitSSA.lean string constants**: missing string-length lookups now `panic!` with internal error instead of silently producing zero-length buffers
- Trust-gate: 1058 checks, 4 new compiler-contract regression tests

### Invalid-query diagnostics

`queryFacts` now returns `Except String String` — malformed/unknown queries produce explicit errors instead of silent empty arrays.

- **Empty query**: explicit error with usage hint and known-kinds list
- **Unknown kind** (single-word or two-part): error naming the unknown kind and listing all valid semantic and fact-filter kinds
- **Empty segments** (leading/trailing/double colons): error identifying the malformed segment
- **Unknown three-part kind**: error explaining only `why-capability:FN:CAP` uses three parts
- **Too many separators**: error with max-separator guidance
- **Kind validation**: single-word and `KIND:FUNCTION` filters validate against the 11 known fact kinds; unknown kinds produce errors instead of empty arrays

`compileAndQuery` surfaces errors to stderr with nonzero exit. 11 regression tests wired into trust-gate. Trust-gate: 1054 pass, 0 fail.

### State-desynchronization attack tests and cross-artifact validation

13 adversarial tests that try to force disagreement between artifact families:

- **Registry vs fingerprint**: stale fingerprint detected, proved status never granted
- **Registry vs function identity**: unknown function warning surfaced via `validateRegistry`
- **Registry conflicting specs**: duplicate/conflicting entries produce explicit warnings
- **Obligations vs diagnostics**: stale obligation status matches stale diagnostic kind; all 13 selfCheck invariants hold under desync scenarios
- **Snapshot drift**: doctored fact values detected as drift; snapshot-then-edit catches body changes via fingerprint diff
- **Traceability vs proof identity**: traceability report covers all obligation function identities
- **Trusted/ineligible bypass**: registry entry for trusted function never grants proved status

Wired `validateRegistry` into `compileAndReport` so registry-level issues (unknown functions, stale fingerprints, conflicting specs) surface as stderr warnings on every report command. Trust-gate: 1039 pass, 0 fail.

### Artifact validation hardening (bundle types, empty registry, diff diagnostics, warning dedup)

Four follow-up fixes to close shallow-validation gaps found in review:

- **Bundle type validation**: `validateBundle` now checks manifest field types — `version` must be numeric, `source_path` must be a string, `failed_at` must be a string or null, artifact flags must be booleans. Previously only checked substring presence.
- **Valid empty registry**: `parseRegistryJson` no longer warns on `[]` or `{"version":1,"proofs":[]}` — an empty registry is a legitimate "no proofs yet" state, not malformed.
- **Warning dedup**: removed duplicate `loadRegistryWarn` calls in `compileAndReport` — report branches now reuse the registry already loaded before ProofCore construction.
- **Diff missing-file diagnostic**: `concrete diff` now checks file existence before reading, producing `error: file not found: <path>` instead of an uncaught IO exception.

4 new regression tests (malformed tests 19-22). Trust-gate: 1043 pass, 0 fail.

### Malformed-artifact attack tests and explicit diagnostics

Eliminated silent fallback/empty-success paths for all artifact families:

- **Proof-registry**: `parseRegistryJson` returns warnings for malformed JSON, empty files, duplicate entries, and empty fingerprints. `loadRegistry` surfaces warnings to stderr instead of silently returning an empty list.
- **Snapshot/diff**: `parseFactsWarn` validates fact schema — flags missing `kind`/`function` fields. Non-array JSON, duplicate fact keys, and truncated JSON all produce explicit errors.
- **Concrete.toml**: `parseDependencies` warns on unparseable dependency lines. `parsePolicy` warns on unrecognized keys. `validateToml` warns on missing `[package]` section, missing `name` field, and unknown top-level sections.
- **Debug bundles**: `concrete validate-bundle` command validates manifest.json structure: required fields (`version`, `source_path`, `failed_at`, `artifacts`), artifact sub-fields, and source directory presence. Corrupted/partial/missing manifests produce explicit errors or warnings.

19 malformed-artifact attack tests wired into `--trust-gate` and `--full` modes. Trust-gate: 1026 pass, 0 fail.

### Bug-to-regression audit gate and failure triage

CI-gated corpus audit (`audit_bug_corpus.sh`) enforces every numbered bug in `docs/bugs/` has a mapped regression test. Failure triage script (`triage_failure.sh`) wires `concrete reduce` and `concrete debug-bundle` into a single command. 21 numbered bugs, 16 mapped, 5 skipped with justification.

### Fix: LLVM IR function name mangling collision

Same-name functions in different modules produced duplicate LLVM definitions. Fixed in `EmitSSA.lean`: collision detection uses `f.modulePath` (not just `m.name`) to handle flattened submodules. Colliding names are qualified with module path in emitted IR. Call resolution chains through local aliases after linker alias resolution, and skips aliases when the call target already matches a defined function. 4 regression tests.

### Fix: Generic Copy struct core-check failure

`struct Copy Box<T> { val: T }` was rejected because field type `.named "T"` wasn't recognized as a type parameter. Fixed in two places: `CoreCheck.lean` skips field Copy check when the type name matches a struct `typeParam`; `Verify.lean` adds post-mono Copy validation (`verifyCopyFieldsPostMono`) that rejects `Box<NonCopy>` instantiations as a compile error. 5 positive regression tests + 1 negative test (`error_copy_generic_non_copy_instantiation`).

### Fix: Top-level main alongside inline modules

`pub fn main()` after `mod` blocks was silently dropped by the parser. `parseProgram` returned after collecting inline modules without parsing remaining items. Fixed in `Parser.lean`: after the mod-parsing loop, remaining tokens are parsed as a sibling "main" module. 2 regression tests.

### Adversarial compiler-hardening corpus

45 hostile test programs across 6 categories (parser, lowering, monomorphization, module, proof/report, scaling). All 47 registered in `run_tests.sh` (45 original + 2 previously missing).

### Attacker-style drift demo and threat model

**Thesis threat/accident model** defined in `docs/THREAT_MODEL.md`, covering six threat categories: proof semantic drift, authority escalation, validation weakening, resource drift, trust boundary erosion, and specification mismatch. Each threat maps to the compiler mechanism that catches it.

Three end-to-end drift demos with drifted variants:
- **`crypto_verify`**: `compute_tag` changes `+` to `-` (proof drift), `check_nonce` changes `>` to `>=` (validation weakening)
- **`elf_header`**: `check_magic` first byte `127` → `0` (proof drift), `check_version` accepts `0` (validation weakening)
- **`thesis_demo`**: `parse_byte` changes `+` to `-` (proof drift), `validate` gains `with(File)` + unbounded `while` (authority escalation + resource drift)

All three demos verified via `concrete snapshot` + `concrete diff` pipeline, producing exit code 1 on trust weakening. 8 new drift-detection gates added to CI evidence section. Trust-gate now runs 960 checks (up from 952).

### Package-level policy enforcement

**`[policy]` section in Concrete.toml** makes thesis properties enforceable at the package level as compile errors, not just report-side analysis.

Three policy constraints:
- **`predictable = true`**: enforces the predictable-execution profile (no recursion, alloc, FFI, blocking I/O) — violations are compile errors
- **`deny = ["Unsafe", ...]`**: forbids specific capabilities — any function declaring a denied capability fails compilation
- **`require-proofs = true`**: all proof-eligible functions must have registered proofs (missing, stale, or blocked = compile error)

Policy runs after CoreCheck, before monomorphization. Only project modules are checked (dependencies are excluded). `crypto_verify` example now uses `[policy] predictable = true, deny = ["Unsafe"]` as a demonstration.

Implemented in `Concrete/Policy.lean` with `parsePolicy` (TOML parsing) and `enforcePolicy` (constraint checking). Wired into both `compileBuild` and `compileTests` in Main.lean.

### Error context chains

Compiler diagnostics now carry a `context : List String` field that accumulates human-readable frames as errors propagate up the call stack. Example output:

```
test.con:2:5: error[check]: type mismatch in let binding 'x': expected i64, got String
 2 |     let x: Int = "hello";
   |     ^
  = while checking module 'main'
  = while checking function 'foo'
```

**Infrastructure** (`Concrete/Diagnostic.lean`):
- `Diagnostic.addContext` / `Diagnostics.addContext` — prepend a context frame
- `withContext` — monadic combinator for `ExceptT Diagnostics m`
- `Except.addContext` — pure combinator for `Except Diagnostics`
- Rendering shows context as `= while ...` lines after the snippet

**Annotated sites**: Check (per-module, per-function), Elab (per-module, per-function), Lower (per-function). Replaces the ad-hoc message-prepending in `lowerModule`.

### CI/CD evidence gates

**10 new evidence gates** in the trust-gate CI job verify proof, predictable, and report correctness:

- **Predictable check**: `crypto_verify` must pass, `thesis_demo` must fail (has I/O)
- **Stale-proof check**: no proof-bearing example has stale proofs (runs `--report proof-status`)
- **Proof-obligation status**: `thesis_demo` has at least one proved obligation
- **Report artifact generation**: all 18 `--report` modes produce non-empty output on `thesis_demo`
- **Trust-drift check**: consistency and fingerprints pass on all proof-bearing examples

Trust-gate now runs 5 contract sections: determinism, consistency, terminology, verify, evidence.

### Uniform diagnostic engine

The core compiler pipeline now emits the same structured `Diagnostic` shape (`severity`, `message`, `pass`, `span`, `hint`, `file`) end-to-end. Previously, Parser returned plain strings, Mono/Lower used `ExceptT String`, and Pipeline wrapped them with `liftStringError`. Now:

- **Parser**: `ParseM` uses `ExceptT Diagnostics`, `throwParse` creates structured diagnostics with source spans
- **Mono**: `MonoM` uses `ExceptT Diagnostics`, `monoProgram` returns `Except Diagnostics`
- **Lower**: `LowerM` uses `ExceptT Diagnostics`, `throwLower` creates structured diagnostics, `lowerModule`/`lowerFn` return `Except Diagnostics`
- **Pipeline**: removed all `liftStringError` wrappers
- **FileSummary**: `resolveImports` returns `Except Diagnostics` directly with a `pass` parameter
- **Check/Elab**: import resolution errors now structured diagnostics without `liftStringError`

`liftStringError` is removed entirely — it no longer exists in the codebase. All core phases (Parse, Resolve, Check, Elab, CoreCheck, Mono, Lower, SSAVerify) now emit structured `Diagnostics` natively.

### Testcase reducer

**New CLI command:** `concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]` shrinks failing programs while preserving the failure, using syntax-aware shrinking in a fixpoint loop.

**Shrinking passes** (coarsest to finest):
1. Remove entire top-level items (functions, structs, enums, impl blocks, traits, constants, etc.)
2. Remove statements from function bodies
3. Remove match arms (keeping at least one)
4. Remove else branches from if statements

**Predicates:** `parse-error`, `resolve-error`, `check-error`, `elab-error`, `core-check-error`, `mono-error`, `lower-error`, `consistency-violation`, `verify-warning`, `crash`. Substring matching via colon: `check-error:expected Int`.

For programs that don't parse (parse-error predicate), falls back to line-based reduction. Implemented in `Concrete/Reduce.lean`.

### Compiler identity in debug bundles

Debug bundle manifests now record real compiler identity instead of `concrete-dev`: version (`0.1.0`), git commit hash (with `-dirty` suffix for uncommitted changes), and Lean toolchain version. Example: `concrete 0.1.0 (abc1234) [leanprover/lean4:v4.28.0]`. Also adds `concrete --version` CLI flag.

### Minimal CI trust gate

**Four correctness contracts now run automatically in CI** via a dedicated `trust-gate` job:

- **Determinism regression** (`test_determinism.sh --quick`): verifies identical output across two compilations for 18 report modes, 6 query kinds, 3 IR emit modes, and snapshot comparison (excluding timestamp).
- **Self-consistency** (`--report consistency`): runs ProofCore self-check (15 invariants) on all 468 compilable programs.
- **Terminology gate** (`test_terminology_gate.sh`): enforces canonical proof/obligation status terms (`proved|stale|missing|blocked|ineligible|trusted`), rejects non-canonical variants.
- **Verifier passes** (`--report verify`): runs post-Elab placeholder check and post-Mono typeVar check on all 385 non-error programs.

**New CLI mode:** `./scripts/tests/run_tests.sh --trust-gate` runs only these four sections. Also available as `make test-trust-gate`.

### Debug bundle for compiler failure reproduction

**New CLI command:** `concrete debug-bundle <file.con> [-o dir]` runs the full pipeline, capturing every available artifact at the point of failure (or completion), and writes a stable directory layout for reproduction.

**Bundle layout:**
- `manifest.json` — compiler version, source path, failure stage, artifact availability
- `source/` — original source files (main + submodules)
- `diagnostics.txt` — rendered diagnostics at failure point
- `core.txt` — Core IR dump (if elaboration succeeded)
- `ssa.txt` — SSA IR dump (if lowering succeeded)
- `llvm.ll` — LLVM IR (if emission succeeded)
- `consistency.txt` — ProofCore self-check results (if available)
- `verify.txt` — post-Elab verifier results (if available)

The capture pipeline tracks 9 stages (parse, resolve, check, elaborate, coreCheck, mono, lower, emit, complete) and records which stage failed. Returns exit code 1 on pipeline failure, 0 on success.

### Determinism contract and nondeterminism audit closed

**Determinism is now the default compiler contract.** Same source + same compiler binary + same toolchain = identical output for all report modes, query modes, IR emission, and snapshot content (excluding the documented `timestamp` metadata field).

**Nondeterminism source audit:** the compiler uses no HashMap/HashSet data structures (all collections are `List`-based), no random values, no PID/hostname access, stable `mergeSort` for all sorting, and deterministic register/fingerprint naming. The only nondeterministic sources are: (1) snapshot `timestamp` field (intentional wall-clock metadata), (2) temp file paths in `--test` mode (internal, not in output artifacts), (3) `CONCRETE_STD` environment variable (configuration, not output).

**Verification:** `test_determinism.sh` runs in full test suite, covering all 20 report modes, query modes, IR emission, and snapshot comparison (excluding timestamp). Compiled binary reproducibility is explicitly not tested (depends on LLVM/clang, outside compiler scope).

### Verifier passes for compiler boundaries

**Three verifier passes added** that catch internal compiler invariant violations before bad state leaks downstream:

- **Post-Elab verifier** (`verifyNoPlaceholders`): detects `Ty.placeholder` surviving elaboration. Available only via `--report verify` (not wired into the pipeline gate). 14 programs with try/defer expressions retain placeholder types — documented intentional exception, resolved during lowering.
- **Post-Mono verifier** (`verifyNoTypeVars`): hard gate — detects `Ty.typeVar` surviving monomorphization. Blocks compilation if generic type variables leak into lowering. Skips generic definitions (only checks monomorphized copies).
- **LLVM IR validation** (`validateLLVMIR`): runs `llvm-as` on emitted `.ll` files before clang. Gracefully skips if llvm-as not on PATH. Wired into all four compilation paths (compileSSA, compileTest, compileBuild, compileTestBuild).

**New CLI mode:** `--report verify` runs both post-Elab (warnings) and post-Mono (errors) verifiers.

**New file:** `Concrete/Verify.lean` — recursive IR walker that checks type predicates across all CExpr/CStmt/CMatchArm nodes, struct/enum fields, extern fn signatures, and constants.

### Pass invariants and contracts completed

**PASSES.md now covers all 12 pipeline stages** with explicit signatures, preconditions, postconditions, error conditions, and invariants. Three sections added:

- **ProofCore extraction (§7b):** full contract with the 13 selfCheck invariants mapped in a table, failure modes, and non-guarantees (extraction faithfulness, fingerprint stability, proof soundness).
- **Verifier-Pass Readiness table:** maps each of the 11 pass boundaries to what is machine-checked today (SSAVerify, ProofCore.selfCheck, ValidatedCore opaque constructor) vs what a future verifier pass would add (Core IR well-formedness, post-Mono type-variable absence, LLVM IR validation, parse span coverage).
- **Failure Modes table:** documents what happens when each pass receives input violating its preconditions — identifying the weak boundaries (Check→Elab, Elab→CoreCheck) where violations propagate silently vs strong boundaries with type-system enforcement.

Cross-pass invariant table updated with ProofCore self-consistency and canonical terminology rows.

### Proof/guarantee terminology gate

**Canonical status terminology unified across all output surfaces.** The six proof/obligation status terms (`proved`, `stale`, `missing`, `blocked`, `ineligible`, `trusted`) are now defined once in `ObligationStatus.canonical` and used consistently across JSON facts, CLI reports, human-readable summaries, and documentation.

**Three terminology drifts fixed:**
- no\_proof / missing\_proof / not\_proved → `"missing"` (obligation and proof\_status JSON)
- not\_eligible → `"ineligible"` (obligation and proof\_status JSON, summary text)
- CLAIM_TAXONOMY.md table corrected to match actual emitted values

**Terminology gate:** `scripts/tests/test_terminology_gate.sh` greps the codebase for banned non-canonical terms in quoted string contexts. Integrated into `--full` test mode. Prevents future drift by failing CI if old terms reappear.

**`ProofState.canonical` and `ProofDiagnosticKind.canonical`** added alongside `ObligationStatus.canonical` — all renderers delegate to these functions instead of inline match expressions.

See [docs/CLAIM_TAXONOMY.md](docs/CLAIM_TAXONOMY.md) §7.6 for the canonical terminology table.

### ProofCore self-consistency checks and `--report consistency`

**13 cross-family invariants** verify that proof obligations, diagnostics, extraction results, fingerprints, and eligibility data agree with each other inside the `ProofCore` artifact. The checks are wired into the compiler as `--report consistency` and run over all 468 compilable test programs with zero violations.

**Invariants checked:** OBL-KNOWN (obligations reference known functions), OBL-STATUS (obligation status agrees with re-derivation), PROVED-EXTRACTED/FP/SPEC (proved status requires extraction + matching fingerprint + spec), STALE-FP/SPEC (stale status requires spec with mismatched fingerprint), ENTRY-FP (entry/obligation fingerprint agreement), EXTRACT-UNSUP (extraction ↔ unsupported list consistency), BLOCKED-UNSUP (eligible + no extraction → non-empty unsupported list), DEP-PROVED (dependencies only reference proved obligations), DUP-NAME (no duplicate function names), DIAG-STATUS (diagnostic kinds agree with obligation status).

**Extraction pipeline fix:** `identifyUnsupported` now recurses into binOp/call/ifExpr children and detects empty-body, void-return, and no-return-statement patterns. Previously, functions with field access inside arithmetic (e.g., `p.first + p.second`) would fail extraction without recording the unsupported construct.

**Test integration:** consistency checks run over all programs in `--full` mode. Determinism suite updated to include the `consistency` report mode.

### Compiler determinism verified and regression suite added

**All compiler output paths are deterministic:** a comprehensive audit verified that all 17 report modes, all query modes, all IR emit modes (LLVM, SSA, Core), compiled binaries, and snapshot JSON (excluding the intentional timestamp field) produce byte-for-byte identical output across consecutive runs.

**Why it works:** the compiler uses `List`-based collections throughout (no HashMap iteration in output paths), explicit sorting where needed (`CapSet.normalize`), deterministic register naming via monotonic counters, and no environment-dependent data in artifacts.

**Deterministic artifact regression suite:** `scripts/tests/test_determinism.sh` verifies reproducibility across 12 representative programs × 27 output modes = 324 checks. Integrated into `run_tests.sh --full` as a quick 81-check gate.

**Single known non-deterministic field:** the `timestamp` in snapshot JSON, which is intentional metadata. All tooling (`concrete diff`, the regression suite) strips it before comparison.

See [docs/DETERMINISM.md](docs/DETERMINISM.md) for the full guarantee, verification methodology, and cross-version caveats.

### Unreachable checker error kinds audited and removed

**Six dead error kinds pruned from Check.lean:** a checker audit determined that `breakInDefer`, `continueInDefer`, `assignToBorrowed`, `variableAlreadyMutBorrowed`, `cannotMutBorrowAlreadyMutBorrowed`, and `cannotImmBorrowMutBorrowed` were all structurally unreachable under the current language model:

- `breakInDefer` / `continueInDefer`: `defer` takes a single call expression (not a block), so `break`/`continue` cannot appear syntactically. The `inDeferBody` flag was never set to `true`.
- `assignToBorrowed`: shadowed by `assignToFrozen` — borrow refs only exist while the owner is frozen.
- `variableAlreadyMutBorrowed`, `cannotMutBorrowAlreadyMutBorrowed`, `cannotImmBorrowMutBorrowed`: `mutBorrowed` was never set to `true` anywhere.

Also removed: `inDeferBody` field from `TypeEnv`, `mutBorrowed` field from `VarInfo`.

**Why this matters:** dead checker branches make the language claim less precise. The checker surface now matches reality — every remaining error kind is reachable and tested.

### Memory-model, borrow/aliasing, and cleanup/leak-boundary pressure test sets land

**19 dedicated pressure tests covering three categories:** the project now has focused pressure tests that exercise the checker and docs against hard cases:

- **Memory-model (5 tests):** fresh borrow per loop iteration, interleaved linear variables, nested linear struct consumption, branch-scoped linear creation/consumption, match arms consuming pre-existing linear values
- **Borrow/aliasing (3 tests):** sequential deref read/write then call on borrow-block `&mut T`, parameter `&mut T` ref across multiple function calls, borrow-for-inspection then consume after unfreeze
- **Cleanup/leak-boundary (11 tests, 6 positive + 5 negative):** nested defer LIFO ordering, defer in loop-called functions, defer combined with borrow blocks, Destroy-trait consumption, multi-hop helper consumption chains, heap alloc with deferred free, plus error cases for move-after-defer, heap leak, linear leak, use-after-destroy, and branch-disagree leak

**Both positive and negative tests:** the cleanup/leak-boundary set includes error cases that prove the checker catches cleanup/leak mistakes, not just happy-path programs.

**MEMORY_REGRESSION_CHECKLIST.md updated:** all eight checklist categories now reference the new pressure tests in their test inventories.

### Memory/reference semantics, `&mut T` closure, and the first explicit no-leak boundary land

**The safe-memory claim is now centralized and checker-matching:** Concrete now has a canonical memory/reference semantics document in [docs/MEMORY_SEMANTICS.md](docs/MEMORY_SEMANTICS.md), an explicit public guarantee statement in [docs/MEMORY_GUARANTEES.md](docs/MEMORY_GUARANTEES.md), and a dedicated `&mut T` closure tracker in [docs/MUT_REF_CLOSURE.md](docs/MUT_REF_CLOSURE.md). This is the point where the memory story stopped being scattered invariants and became one checked claim surface.

**`&mut T` soundness-critical closure landed:** the compiler now distinguishes two real `&mut T` modes:

- borrow-block `&mut T` refs are scoped linear views and are consumed on function call / return
- function-parameter `&mut T` refs are reborrowable and not consumed on ordinary call use

This closes the earlier checker/codegen mismatch where borrow-block `&mut T` refs could be reused in ways that later broke lowering/codegen.

**Checker/codegen agreement was hardened, not just documented:** this arc included:

- checker enforcement for borrow-block `&mut T` consumption
- lowering fixes for reference-typed borrow variables instead of value-typed temporaries
- the early-return-inside-borrow-block codegen fix
- adversarial `&mut T` coverage for call-use, branch agreement, loop behavior, deref-vs-consume behavior, nested borrow blocks, method-call behavior, and return-in-borrow paths

This is the point where the compiler can honestly say safe-subset ownership mistakes must fail early with diagnostics rather than survive checking and crash later in codegen.

**The first explicit strong no-leak boundary is now stated:** Concrete now makes a real compile-time no-leak claim for the strong safe subset:

- linear values must be consumed or reserved by scope exit
- `defer` reserves cleanup in a way the checker tracks explicitly
- the docs now separate this strong guarantee from weaker future audit/reporting around trusted, FFI, arena, and raw-pointer boundaries

**What changed strategically:** the next work is no longer “finish the basic safe-memory story.” It is:

- make effect/trust proof boundaries explicit
- write the precise theorem/guarantee statement
- define the language-semantics vs proof-semantics boundary
- define the user-facing proof contract

### First proof-foundation arc lands end-to-end

**Two real proof-backed flagship examples:** the project now has two honest proof-backed examples that exercise different parts of the thesis:

- a crypto-adjacent authenticated-tag core used as a second proof-backed domain
- an ELF header validator with a real file-I/O shell, trusted POSIX boundary, and proved pure validation core

These examples are not just demos. They carry snapshots, drift detection, registry-backed proof names, and adversarial tests, so they function as regression targets for the proof story.

**Eligibility-first proof pipeline:** proof processing no longer starts from ad hoc report/extraction logic. The compiler now:

1. determines proof eligibility first
2. explains source and profile exclusions explicitly
3. lowers only the resulting proof-facing fragment into `ProofCore`

That closes the gap between “report what looks extractable” and “have an explicit provable subset.”

**Explicit `Core -> ProofCore` compiler boundary:** `ProofCore` is now a real pass and the architectural owner of:

- proof eligibility
- extracted and normalized proof terms
- unsupported-construct classification
- raw body fingerprints
- call graph and recursion analysis
- spec attachment
- proof obligations

`Report.lean` is now a consumer of those artifacts rather than a shadow owner of proof semantics.

**Normalized proof target + typed proof identity:** the proof pipeline now has:

- normalized `PExpr` proof targets
- typed `FunctionIdentity`
- typed `SpecIdentity`
- typed `SpecAttachment`
- registry ownership and validation inside the proof pipeline

This separates proof attachment *identity* from proof *status*, which is the right foundation for later diagnostics, compatibility guarantees, and proof maintenance tooling.

**Mechanical obligations land as first-class artifacts:** proof obligations are now generated inside `ProofCore` rather than being mostly reconstructed in report code. The pipeline now carries:

- stable obligation identity
- mechanically derived obligation status
- dependency tracking from the qualified call graph
- read-only report/fact/query consumption of obligation artifacts

This is the point where the proof system stops being “proof names plus reports” and becomes a real artifact-producing compiler workflow.

**What changed strategically:** Concrete now has a serious Lean 4 proof architecture for a narrow but real subset. The next work is not “invent the proof pipeline” anymore. It is:

- close the remaining first-slice obligation-model correctness gaps
- make proof-oriented diagnostics first-class
- make memory/reference and effect/trust proof boundaries explicit enough to broaden the provable subset honestly

### Stable attached spec identities land as a real proof-pipeline layer

**Typed proof attachment model:** proof/spec attachment is no longer just free strings reconstructed in multiple report paths. The proof pipeline now has explicit identity types for:

- function identity
- spec identity
- spec attachment source

This separates *identity* from *status*: the compiler can now carry what proof/spec is attached to a function without collapsing that into `proved` / `stale` / `unproved` too early.

**Centralized spec resolution:** the old scattered lookup logic has been replaced by one proof-pipeline path. Registry ownership moved out of report code and into the proof pipeline, so report/fact surfaces consume resolved attachments instead of re-deriving them independently.

**Registry validation:** registry loading now has real consistency checks instead of silently accepting arbitrary entries. Validation distinguishes:

- unknown function
- duplicate entry
- conflicting entry
- stale fingerprint

That makes the registry more honest as a proof artifact and prepares the transition from raw fingerprint-only attachment toward richer proof-target-aware identities.

**What changed strategically:** the next step is no longer "attach strings to functions." Concrete now has a real identity layer between ProofCore and obligations. That is the right foundation for mechanical obligation generation and proof-oriented diagnostics.

### ProofCore normalization lands as a canonical proof-target layer

**Normalized proof target:** extracted `PExpr` terms are now normalized before they are exposed as the proof target. The normalization pass applies a deliberately small rewrite set:

- algebraic identities (`x + 0`, `x * 1`, `x * 0`, `x - 0`)
- commutative operand ordering for proof-facing expressions
- dead-let elimination
- let flattening
- boolean short-circuiting for literal conditions

This gives the proof pipeline a cleaner canonical target without pretending every source refactor should collapse to the same proof identity.

**Architectural split preserved:** raw Core fingerprints remain the current proof attachment identity, while normalized `PExpr` is the canonical proof-facing term. That means:

- proof maintenance and stale detection still use raw body fingerprints
- proof extraction, traceability, and `proof_core` output now show a more canonical form
- the compiler is ready for a later identity-model upgrade without forcing it into the normalization landing

This is an important constraint because it keeps the current registry/proof workflow stable while improving the proof target itself.

**Regression coverage:** adversarial tests now pin both the normalization rewrites and the qualified call-graph path, so the canonical proof target is not just an implementation detail. The proof pipeline now has explicit coverage for normalization behavior rather than only indirect extraction tests.

**What changed strategically:** ProofCore is now not only an explicit pass, but also a more canonical proof target. The next architectural step is no longer “how do we normalize proof terms?” It is “what is the stable typed identity model for functions/specs/proofs that will sit on top of this normalized target?”

### ProofCore becomes the real proof-pipeline boundary

**Explicit `Core -> ProofCore` pass:** `ProofCore.lean` now owns the proof-oriented compiler boundary instead of leaving proof extraction as mostly report-side logic. The pass computes and carries:

- proof eligibility
- extracted `PExpr` forms
- unsupported-construct classification
- body fingerprints
- call graph
- recursion classification
- loop analysis
- extern-name context

This turns ProofCore from a reporting helper into a real compiler artifact that the rest of the proof/evidence pipeline can consume.

**Report consolidation:** the remaining shadow extraction pipeline in `Report.lean` is gone. Extraction report, extraction JSON facts, and traceability now read from `ProofCore` entries directly instead of recomputing extraction/eligibility from raw `CModule` functions. That makes report code presentation-only again and removes a serious risk of proof-pipeline drift.

**Qualified proof identity:** proof-pipeline analysis now uses canonical qualified function names across the call graph and recursion machinery, so same-name functions in different modules no longer collide. This closes a real correctness gap that would have weakened the proof story in multi-module programs.

**Compile-time result:** the new pass adds negligible overhead in practice. On current examples, compile-only versus compile-plus-proof-reporting remains within noise, and even larger examples remain frontend-dominated rather than ProofCore-dominated. That matters because the proof/evidence architecture is now getting stronger without making the compiler obviously heavier.

**What changed strategically:** the proof architecture is no longer just "real enough to demo". The compiler now has a much cleaner proof boundary:

1. validated Core
2. explicit ProofCore artifact
3. reports/facts/queries consuming that artifact

That clears the way for the next real architectural step: normalize ProofCore before Lean attachment so proofs and fingerprints target a canonical proof form rather than incidental extracted structure.

### ELF file-I/O flagship closes cleanly; proof pipeline becomes eligibility-first

**ELF file-I/O flagship cleanup:** the ELF example now fully closes the gap between "systems-shaped validator" and "real project-mode example". Runtime coverage exercises all shipped binary fixtures, including the bad-class path, and generated example binaries are kept out of the worktree so the flagship stays clean as a regression target.

**Trusted shell + proved core split:** the example now demonstrates the intended Concrete evidence layering cleanly:

- proved pure validator core
- trusted low-level file/pointer boundary
- reported entrypoint shell

That makes the flagship a stronger thesis example than the earlier field-only validator, because it now has real file I/O while keeping the Lean-backed core unchanged and honest.

**Eligibility-first proof pipeline:** proof processing no longer starts from extraction/report surfaces alone. The compiler now computes proof eligibility first and threads that result through the rest of the proof/evidence workflow.

What is now real:

- `--report eligibility`
- eligibility facts in snapshots
- eligibility-first proof-status / obligation accounting
- updated fact counts and summary expectations across the examples

**What changed strategically:** Concrete's proof story is no longer "report what seems extractable" first. It now has a clearer front door:

1. determine whether a function is in the provable subset
2. explain exclusions explicitly
3. only then lower eligible functions into the proof pipeline

That closes the next architectural gap after the ELF flagship and makes the following roadmap step much sharper: promote `Core -> ProofCore` into an explicit compiler phase rather than leaving proof extraction as a mostly report-side boundary.

### Fact artifact snapshots land as first-class workflow output

**`concrete snapshot`:** Added a project-facing fact artifact command:

```bash
concrete snapshot <file.con> [-o output.json]
```

It writes a versioned JSON snapshot containing:

- source path
- timestamp
- fact count
- summary counts
- the full fact array

The snapshot bundles the existing evidence workflow into one machine-readable artifact surface:

- effects
- capability
- unsafe / trust
- allocation
- proof status
- obligations
- extraction
- traceability
- predictable violations

**Default artifact path:** when `-o` is omitted, the compiler writes `<file>.facts.json` next to the source file.

**Diff interop:** `concrete diff` now accepts both raw fact arrays and snapshot objects by reading the `"facts"` field from the snapshot object. That means the intended workflow is now real:

1. generate a baseline snapshot
2. commit it or archive it in CI
3. diff later snapshots against it

**What changed strategically:** the evidence story is no longer spread across many reports only. Concrete now has a stable, versioned artifact that CI, tooling, and future MCP/AI integrations can consume directly. This closes the first item in the current thesis-validation roadmap and makes the next priorities clearer: a stronger second flagship example, ProofCore pressure from that example, policy enforcement, and CI evidence gates.

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

`docs/ABI.md` documents what's stable (FFI-safe scalars, repr(C)/packed/align layout, extern fn), what's intentionally unstable (non-repr struct layout, enum representation, pass-by-ptr convention, symbol naming), platform assumptions (64-bit only, hardcoded sizes), the FFI safety model, and a cross-platform verification matrix. 4 layout verification tests added to `Concrete/PipelineTest.lean` (scalar sizes, builtin sizes, repr(C) layout, pass-by-ptr decisions). Test suite: 651 tests (32 pass-level).

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
- **Pass-level Lean tests** (`Concrete/PipelineTest.lean`, 28 tests): parse (4), frontend/check/elab (8), monomorphize (2), SSA lowering (2), SSA verify (3), SSA cleanup (2), SSA emit (2), full pipeline (5). Each pass tested in isolation on in-memory source strings — no clang, no file I/O, <1s total. Tests both success and error paths.
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
