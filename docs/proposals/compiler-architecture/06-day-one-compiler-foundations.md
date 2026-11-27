**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to Stage-0 (`B0b`) provisional tasks [R-0736](../../../ROADMAP.md#r-0736)–[R-0742](../../../ROADMAP.md#r-0742) (and extends `R-0528` / [R-0730](../../../ROADMAP.md#r-0730)).**

# 06 — Stage-0 (`B0b`) Day-One Compiler Foundations & Engineering Multipliers

**Scope.** Beyond the six frontier architectural upgrades ([Docs 01–05](README.md)), eight pragmatic compiler-engineering foundations pay compounding returns on every single PR from Slice 0 through 1.0—and cost **10× more to retrofit** once Slices 2–11 land or Core freezes (`A2c`). All eight start in **Stage 0 (`B0b`)** (running alongside `B0b` / Slice 1 without blocking `B0a`), require **zero `ConcreteSpec` language-rule changes**, and decide no language rules.

---

## <a id="1-round-trip-ir-text-format"></a>1. Canonical Round-Trip Text + Binary Format for Every IR (`R-0736`)

### Why Now (Stage 0)
Today, [Concrete/Artifact/Encoding.lean](../../../Concrete/Artifact/Encoding.lean) frames `certificate | receipt | facts`, while `BIR` is only printed as LLVM IR text or QBE debug dumps, and `Core` / `MIR` / `Memory` have no canonical round-trip text syntax. Every mature compiler (LLVM `.ll`, MLIR, Cranelift `.clif`, QBE) relies on a **human-readable, round-trip-parseable IR format** as its primary development multiplier.

At Slice 0, each IR (`Core`, `MIR`, `Memory`, `BIR`) has only a handful of constructors—writing `print : IR → String` and `parse : String → Except ParseError IR` (plus canonical `Artifact.encode`) takes ~100–200 lines total. Every future slice then adds 3–5 lines per new constructor instead of retrofitting a parser over 11 slices of IR.

### Design
- For each IR `X ∈ {Core, MIR, Memory, BIR}`:
  - Total deterministic `X.print : X.Program → String` (preserving typed IDs, spans/origins, and `FactTable` annotations).
  - Total `X.parse : String → Except Refusal X.Program`.
  - Kernel-checked (or tested-plus-proved) **round-trip law**: `X.WF p → X.parse (X.print p) = .ok p`.
- CLI entry points (`--emit-ir=core|mir|memory|bir`, `--from-ir=bir`):
  - **Pass-isolated unit & golden tests:** test `MIR → Memory`, `SSA`, `BIR → LCB`, and `ValidatedPass` ([Doc 01](01-validated-passes-and-ub-free-opt.md)) directly without going through the frontend.
  - **Direct middle/backend fuzzing:** generate valid `BIR` / `MIR` directly to stress lowering, LCB, and validators (`R-0272`, `R-0732`).
  - **LLM & human inspectability:** every intermediate IR is readable, diffable, and copy-pasteable into minimal regression files.

---

## <a id="2-dense-arrays-and-proof-stability"></a>2. Dense `Array`-Indexed Tables & Modular Proof-Stability Discipline (`R-0737`)

### Problem 1 — The `AList` / `List.lookup` Scaling Cliff
On `main`, [Concrete/MIR.lean](../../../Concrete/MIR.lean), [Concrete/Memory.lean](../../../Concrete/Memory.lean), and [Concrete/BIR.lean](../../../Concrete/BIR.lean) store pass environments and tables in association lists (`AList`, `List.lookup`, `List.find?`). Every lookup inside a pass loop is $O(n)$, making passes $O(n^2)$—already surfacing in `R-0723` (`BIR.enter`, `LCB`, `LL1`) and the deterministic span scaling gate (`R-0550`).

Migrating `List` → dense `Array` indexed by typed IDs (`BlockId`, `RegId`, `PlaceId`, `SiteId`) takes days over Slice 0/1's small proof base and *months* once Slices 2–11 add 50,000 lines of proofs.

### Problem 2 — Cross-Slice Proof Churn
In verified compilers, adding one field to a `State` struct or one constructor to `Expr` in Slice 4 or Slice 7 routinely breaks hundreds of lines of brittle `cases`/`simp` proofs across earlier slices.

### Stage-0 Standing Engineering Discipline
1. **Dense `Array` tables in all compiler hot paths:**
   - Provide one tiny shared verified table module (`Concrete/Data/IdTable.lean`) wrapping `Array` indexed by typed `Nat` IDs with O(1) `get?`/`set` and pre-proved `get_set_eq` / `get_set_ne` / `size_set` simp lemmas.
   - Adopt a standing rule from Slice 1 onward: **no `List.lookup` or `List.find?` inside per-instruction/per-block pass loops** (enforced naturally by `R-0550`'s `8n / n` scaling gate).
2. **Modular state lenses & reusable simulation tactics (`Concrete/ProofLib.lean`):**
   - Never destructure multi-field `State` records (`cases st`) inside pass proofs; state every step lemma in terms of field-update frame lemmas (`updateField_other`) so adding a new field to `State` in a later slice breaks **zero** existing proofs.
   - Provide small reusable Lean 4 tactics/combinators for `CheckpointSimulation` and `Refines` lockstep/stuttering steps so each new slice's lowering proof stays compact and uniform (`R-0560`, `R-0724`).

---

## <a id="3-bug-minimizer-concrete-reduce"></a>3. Syntax/Core-Directed Bug Minimizer (`concrete reduce`) (`R-0738`)

### Why Now (Stage 0)
As soon as `B0b` turns on differential fuzzing (`R-0272`) and cross-backend parity (`R-0578`: LLVM vs. LCB vs. Lean reference evaluator), failing generated programs will routinely be 50–300 lines long. Manually shrinking them wastes hours every week.

### Design
Exploit Concrete's existing lossless LL(1) AST (`TokenStream` / `SyntaxTree`) and typed `Core` IR to ship a small (~150-line) deterministic delta-minimizer (`concrete reduce <prog.con> --check <predicate.sh>`):
- **Structured AST/Core passes:** remove unused `let` bindings, replace subexpressions with constants of the same type, collapse trivial branches/blocks, delete unused statements/functions—re-checking the interestingness predicate (`<predicate.sh>`) after each reduction step.
- **Zero TCB:** Pure developer/CI tooling feeding `R-0272` counterexample-to-regression persistence so every fuzzer hit, differential bug, or proof failure shrinks automatically into a 5-line regression fixture.

---

## <a id="4-early-kernel-sketches"></a>4. Early Real-World Kernel Sketches + Paired Negative Twins Against the 0.1 Grammar (`R-0739`)

### Why Now (Stage 0, Before Slice 2–6 Lock Shapes)
The combined 12-slice LL(1) grammar and token ballot just landed (`R-0728`). Core does not freeze until **Slice 11 (`A2c`)**. Waiting until Slices 7–11 to discover syntactic or ownership ergonomics cliffs forces costly late changes.

### Design
Write 5 compact (50–100 line) real-world Concrete kernel sketches right now under `tests/sketches/` (non-normative until their target slices execute) paired with **"bug Concrete refuses" negative twins**:
1. **Zero-copy ring buffer** (bitmask wraparound `R-0712`, `borrow mut`, linear ownership).
2. **Open-addressed hash-table probe loop** (counted loop `#[variant]`, modulo/mask indexing, `Option` matching).
3. **QOI or PNG chunk decoder** (byte-slice indexing, explicit-endian/wire parsing `R-0714`, typed errors).
4. **Bump-arena tokenizer / parser** (scoped borrows, cleanup/`defer`, fixed-capacity output).
5. **`grep` inner search loop** (UTF-8 views, zero-heap guarantee, `Fs` grant narrowing).

Parse every positive sketch and negative twin through the combined 12-slice LL(1) grammar checker (`R-0728`) in Stage 0 so any grammar ambiguity, awkward token spelling, or ownership/borrow friction surfaces immediately for Batch 2 before `A2c` (`R-0567`, `R-0573`).

---

## <a id="5-trust-and-speed-scorecard"></a>5. Single Live Trust & Speed Scorecard (`R-0740`)

### Why Now (Stage 0)
Concrete already tracks an extraordinary assurance suite (`Premises.lean`, `axioms.sh`, `nonvacuity.sh`, `external.sh`, `R-0550` scaling gate, `R-0424` benchmarks), but the numbers live across separate scripts and CI logs.

### Design
Add one lightweight generator script that aggregates existing gate outputs (zero duplicate definitions — [Rule 5](../../PHILOSOPHY.md#the-rules)) into a single per-commit scorecard artifact (Markdown + JSON):
- **Open trust premises count** (from [Concrete/Premises.lean](../../../Concrete/Premises.lean) — visibly shrinking as proofs retire premises).
- **Stage proof coverage** (`directProof` vs. `provedValidator` vs. `namedPremise` from `Interfaces/Pipeline.lean`).
- **Verification cost:** clean `lake build` wall time + external checker (`con-leche` / `nanoda`, `R-0702`) replay time.
- **Compiler scaling ratio:** `8n / n` work/time ratio from `R-0550`.
- **Runtime speed & binary size vs. `clang -O2`:** report-only from Slice 2 (`R-0424`, `R-0623`).

---

## <a id="6-synthetic-target-profile"></a>6. Synthetic 32-Bit / Big-Endian `TargetProfile` Portability Check Before `A2c` (`R-0741`)

### Why Now (Before Core Freezes at Slice 11)
Both active native targets (`x86_64-linux` and `aarch64-macos`) are **64-bit little-endian**. Without exercising at least one non-64-bit and one big-endian `TargetProfile` in `ConcreteSpec` (`C3`, `R-0609`) and `Concrete/Memory.lean` (`R-0580`) before Core freezes (`A2c`), accidental `ptr_width == 64`, `usize == u64`, or little-endian assumptions silently leak into layout, encoding, and BIR lowering.

### Design
- Define two synthetic test-only target profiles in Lean (e.g. `synthetic32LE` with 32-bit pointers/lengths and `synthetic32BE` with big-endian byte order) implementing `C3`'s `Target` profile interface.
- Run `ConcreteSpec` layout/representation laws (`rep_roundtrip`, `C3` alignment/size laws) and `Memory`/`BIR` lowering tests across both real 64-bit targets **and** the synthetic 32-bit/big-endian profiles in CI.
- **Zero native toolchain required:** executes purely inside Lean, guaranteeing embedded (`ARM32`, `RISC-V`), `wasm32`, and network big-endian portability remain structurally intact when Core freezes at `A2c`.

---

## <a id="7-deterministic-work-budgets"></a>7. Deterministic Work-Count Budgets for Admission & Validators (Extends `R-0528` & `R-0730`)

### Why Now
The single biggest usability failure in SMT-based verification languages (Dafny, Verus) is *"passes on my laptop, times out on CI."* Because Concrete forbids SMT in admission and already enforces deterministic work-count scaling (`R-0550`), every admission checker, parser, and `ValidatedPass` validator ([Doc 01](01-validated-passes-and-ub-free-opt.md)) can be bounded by **exact deterministic step/work counts**, never wall-clock timers.

### Design
- Every admission checker and `ValidatedPass` validator takes an explicit deterministic work budget (extending `R-0528`'s resource caps) and fails closed with a typed `workLimitExceeded (used, limit)` refusal when exceeded.
- **Guarantee:** *"Accepted on a developer laptop ⇔ accepted on CI and independent replay"* becomes a machine-independent mathematical invariant, eliminating flaky builds across fast and slow runners.

---

## <a id="8-proof-backed-inspector"></a>8. Proof-Backed Source ↔ IR ↔ Assembly Inspector (`concrete inspect <fn>`) (`R-0742`)

### Why Now
Mainstream compilers rely on lossy DWARF debug info that optimizations routinely mangle, forcing systems engineers to guess why a loop spilled registers or whether a bounds check actually disappeared. Concrete already threads typed `Origin`s, `FactTable` entries, and `EliminationLedger` records ([Doc 01](01-validated-passes-and-ub-free-opt.md)) across `Core → MIR → Memory → BIR`, and `R-0736` gives every IR a canonical readable text view.

### Design
- **Stage-0 / Slice-0 increment:** `concrete inspect <fn>` renders source lines side-by-side with `Core`, `MIR`, `Memory`, and `BIR` using the exact `Origin` / `FactTable` / `EliminationLedger` graph—showing per source line which checks were discharged and eliminated (and why), which checks remain residual, and each register/slot assignment.
- **`LP0` / `R-0517` native increment:** Carry `Origin` one final hop into LCB machine instructions so `concrete inspect <fn> --asm` acts as a **built-in, proof-backed Godbolt** mapping source lines directly to verified native assembly instructions, stack slots, and register spills (zero reliance on lossy DWARF).

---

## Acceptance Gates (Stage 0 / `B0b`)

| Task | Deliverable | Gate |
|---|---|---|
| `R-0736` | `print`/`parse` round-trip for `Core`, `MIR`, `Memory`, `BIR` + `--emit-ir` / `--from-ir` CLI flags | Round-trip law (`parse (print p) = .ok p`) on all Slice-0 fixtures + pass-isolated BIR→LCB test + malformed-IR refusal control |
| `R-0737` | `Concrete/Data/IdTable.lean` + `Concrete/ProofLib.lean` + remove `AList`/`List.lookup` from Slice-0 MIR/Memory/BIR hot paths | `IdTable` simp laws kernel-checked, `R-0550` scaling gate passes on $8n$, zero `sorry`/extra axioms |
| `R-0738` | `concrete reduce` AST/Core delta-minimizer | Deterministic minimization control shrinking a 30-line planted failing `.con` program to ≤ 5 lines while preserving the failure predicate |
| `R-0739` | 5 kernel sketches + 5 paired refused twins parsed against `R-0728`'s combined 12-slice LL(1) grammar | All 5 sketches parse cleanly with `ll1`-checked combined grammar; friction log fed into Batch 2 (`R-0567`) |
| `R-0740` | Per-commit Trust & Speed Scorecard generator over existing `Premises`, `R-0550`, `R-0702`, `R-0424` outputs | Deterministic regeneration control; planted added premise immediately increments open-premise count |
| `R-0741` | Synthetic `synthetic32LE` and `synthetic32BE` `TargetProfile` instances exercised in `C3`/`Memory`/`BIR` Lean tests | Layout, `rep_roundtrip`, and lowering proofs/tests pass on 32-bit and big-endian profiles; planted hardcoded-64-bit/LE assumption fails |
| `R-0528` / `R-0730` | Deterministic work-count budgets on admission checker and `ValidatedPass` validators (no wall-clock admission timeouts) | Exact-work-count refusal control reproducing identically across fast/slow hosts; zero wall-clock nondeterminism |
| `R-0742` | `concrete inspect <fn>` side-by-side source ↔ IR (+ `--asm` at `LP0`/`R-0517`) over `Origin` + `FactTable` + `EliminationLedger` | Golden inspection output showing discharged/eliminated vs. residual checks with exact source spans; planted wrong-origin mapping fails |
