**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27).** Maps to [R-0736](../../../ROADMAP.md#r-0736)–[R-0742](../../../ROADMAP.md#r-0742) and extends R-0528 and [R-0730](../../../ROADMAP.md#r-0730).

# 06 — Stage-0 (`B0b`) Day-One Compiler Foundations & Engineering Multipliers

**Scope.** These are proposed B0b and later engineering increments. The cutover adds only closed pass slots and the provenance split. R-0741 needs an interface change before synthetic targets can reach lowering. R-0737 works within the existing proof trust ban.

---

## <a id="1-round-trip-ir-text-format"></a>1. Canonical Round-Trip Text + Binary Format for Every IR (`R-0736`)

### Why Now (Stage 0)
The scalar test emitter already writes a BIR value dump. `LCB.render` prints the adapted LCB function and `Runtime.qbe` prints the runtime. The artifact encoder supplies framing for certificates, receipts and facts. [Encoding](../../../Concrete/Artifact/Encoding.lean).

R-0736 adds versioned text and binary codecs for Core and the indexed MIR, Memory, SSA and BIR of a specified slice and profile.

### Design
Target the current slice-indexed IR carriers in [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean) (`MIR lang profile`, `Memory lang profile`, `SSA lang profile`, `BIR lang profile`) alongside Core:
- Prove both round-trips for supported well-formed values: parsing the printed IR and decoding the binary encoding must each recover the same value, including IDs, spans, occurrences and check dispositions. Return typed `Refusal` on invalid input. Add optional fact annotations when their schema exists.
- Add a bounded canonical decoder under R-0528. Test malformed, over-budget, wrong-profile and forged-evidence inputs. Validate imported IR before lowering and identify it as the starting artifact in receipts.
- Expose the proposed `--emit-ir` for inspection and `--from-ir` only for dev/test pass isolation, golden tests and fuzzing. Imported IR has no source admission witness and cannot issue admitted source certificates or facts.

---

## <a id="2-dense-arrays-and-proof-stability"></a>2. Dense `Array`-Indexed Tables & Modular Proof-Stability Discipline (`R-0737`)

### Problem 1 — The `AList` / `List.lookup` Scaling Cliff
The retiring scalar MIR, Memory and BIR use some linear searches; scalar SSA already uses a hash map. The indexed `AList` fields in `Interfaces/Carriers.lean` are frozen. R-0737 concerns pass-local working tables. Changing a frozen carrier requires an interface round.

### Problem 2 — Cross-Slice Proof Churn
In verified compilers, adding one field to a `State` struct or one constructor to `Expr` in Slice 4 or Slice 7 routinely breaks hundreds of lines of brittle `cases`/`simp` proofs across earlier slices.

### Stage-0 Standing Engineering Discipline
1. **Pass-local `IdTable`.** Use bounded dense indices for measured hot lookups. Validate or remap sparse external IDs before allocating arrays. Prove lookup/update, bounds and compaction laws. Count lookup work on adversarial inputs and measure stage scaling under R-0550. The span probe counts particular operations; an elapsed-time ratio alone cannot identify a lookup implementation.
2. **Proof stability.** Use frame lemmas to localize proof changes. Build pure Lean lemma combinators for `CheckpointSimulation` and `BehaviorRefinement` under R-0560. The trust ban excludes custom syntax, macros, elaborators and `run_tac`. Any relaxation requires R-0724 approval.

---

## <a id="3-bug-minimizer-concrete-reduce"></a>3. Syntax/Core-Directed Bug Minimizer (`concrete reduce`) (`R-0738`)

### Why Now (Stage 0)
As soon as `B0b` turns on differential fuzzing (`R-0272`) and cross-backend parity (`R-0578`: LLVM vs. LCB vs. Lean reference evaluator), failing generated programs will routinely be 50–300 lines long. Manually shrinking them wastes hours every week.

### Design
R-0738 supplies a standalone deterministic reducer over retained tokens and supported AST or Core forms. The proposed `concrete reduce` subcommand implements R-0272's reduction clause. Slice 0 uses `Surface0.Token` and `Surface0.Program`; the indexed cutover adds `TokenStream` and `SyntaxTree`. Trivia belongs to the token stream. Reproduce a source edit by slicing retained bytes or by printing edited AST/Core forms as valid `.con` source.

Run the chosen predicate under `bounded_run` for each candidate. Preserve the selected failure signature and report the smallest candidate found within the work budget. R-0272 owns fuzzer integration and regression persistence.

---

## <a id="4-early-kernel-sketches"></a>4. Early Real-World Kernel Sketches + Paired Negative Twins Against the 0.1 Grammar (`R-0739`)

### Why Now (Stage 0, Before Slice 2–6 Lock Shapes)
The combined grammar has a compatibility draft before the Slice 11 Core freeze. R-0567 owns the pre-freeze sketch corpus and its friction dispositions. R-0573 owns executable interaction workloads.

### Design
Pair five non-normative sketches with syntactically valid programs that violate a named semantic rule:
1. **Zero-copy ring buffer** (bitmask wraparound `R-0712`, `borrow mut`, linear ownership).
2. **Open-addressed hash-table probe loop** (counted loop `#[variant]`, modulo/mask indexing, `Option` matching).
3. **QOI or PNG chunk decoder** using byte-slice reads, arithmetic and typed errors.
4. **Fixed-buffer tokenizer** using Slice 7/11 borrows and caller-provided storage.
5. **`grep` inner search loop** using byte views, a fixed buffer and an explicit grant.

Use the R-0728 compatibility draft to parse all five sketches and their five syntactically valid negative twins. Record the intended semantic refusal for each twin. Run that refusal check when its feature checker lands. `docs/grammar/Check.lean` checks syntax.

---

## <a id="5-trust-and-speed-scorecard"></a>5. Single Live Trust & Speed Scorecard (`R-0740`)

### Why Now (Stage 0)
Premise inventories, build measurements, scaling results and external proof replay live in different reports.

### Design
Report the declared evidence class separately from a checked theorem for the production implementation, its premises, and a reachable successful witness. Count stage proof coverage only for kernel-checked `Stage.Sound` results for the production stage, under the allowed axioms. A registry entry tagged `directProof` or `provedValidator` supplies a declaration of intent.

Record the revision, target and measurement inputs. Include open premises, clean build time, R-0550 scaling and available native size/speed comparisons. Show unavailable replay and benchmark results explicitly. The roadmap assigns full R-0702 replay to nightly and release qualification. Current CI also runs external rechecks of `ConcreteSpec` and `ConcreteSpecEvidence` on PRs and merge groups. Cite the measured revision and scope. [CI workflow](../../../.github/workflows/lean_action_ci.yml). Identical recorded inputs must regenerate identical reports.

---

## <a id="6-synthetic-target-profile"></a>6. Synthetic 32-Bit and Big-Endian Layout Tests Before A2c (R-0741)

### Why Now (Before Core Freezes at Slice 11)
The hosted backend profiles all select the same 64-bit little-endian memory target. C3 defines `Target`; the early layout and compiler dispatch are in `Slice0Layout.Profile` and `Concrete.Interfaces.Profile`.

### Design
Exercise target-parametric layout operations on well-formed synthetic targets. Test explicit expected bytes for a multibyte value in both byte orders. Preserve u64 length semantics and require a fit proof for narrower stored lengths. The legacy `Rep.rep_roundtrip` has no target argument. `ConcreteSpec.Core.Hybrid.rep_roundtrip` already quantifies over a target and storable value.

Extend the lowering tests when a synthetic memory target can reach the actual lowering path. This requires generalizing layout/target dispatch in the frozen `ConcreteSpec/StageInterface.lean` and `Concrete/Interfaces/Carriers.lean`. Report exactly the operations exercised.

---

## <a id="7-deterministic-work-budgets"></a>7. Deterministic Work-Count Budgets for Admission & Validators (Extends `R-0528` & `R-0730`)

### Why Now
Meter parser, checker and witness replay work explicitly under fixed inputs, versions and limits. Mandatory verification reports budget exhaustion as incomplete. An optional optimization returns its validated input unchanged. Operational time and memory limits remain separate.

### Design
Define deterministic counters and check them during the work. A mandatory `workLimitExceeded` is a resource refusal (exit 2). Compare the same inputs, versions and budgets across hosts when both complete without an operational failure such as OOM.

---

## <a id="8-proof-backed-inspector"></a>8. Proof-Backed Source ↔ IR ↔ Assembly Inspector (`concrete inspect <fn>`) (`R-0742`)

### Why Now
The indexed carriers thread `Origin` and `OperationCheck` (`CheckDisposition`). The pass-carried `FactTable` (Slice 3+) and checked elimination ledger are proposed additions.

### Design
Stage 0 displays recorded source spans, IR occurrences and available discharge evidence. Later passes add checked facts and elimination witnesses. Show native instructions, spills and stack slots only when the emitted artifact has a validated mapping, and mark unavailable locations explicitly. The native `--asm` increment requires upstream LCB origin propagation and Phase E's R-0517.

---

## Acceptance Gates by Increment

| Task | Deliverable | Gate |
|---|---|---|
| R-0736 | Versioned text and binary codecs for Core, MIR, Memory, SSA and BIR | Prove both round-trips. Test malformed, over-budget, wrong-profile and forged inputs and dev/test-only import. |
| R-0737 | Bounded dense pass-local lookup tables and pure frame lemmas | Prove lookup laws, count adversarial lookup work and measure scaling under R-0550. |
| R-0738 | Standalone deterministic reducer for R-0272 | Reduce one planted 30-line example to at most five lines under budget with its failure signature intact. |
| R-0739 | Five grammar sketches and five semantic negative twins for R-0567 | Parse all ten against the compatibility draft. Record each intended refusal and check it when its feature checker lands. |
| R-0740 | Per-revision trust and speed scorecard | Reproduce the report from recorded inputs. An evidence label alone earns no stage proof; an added premise changes the count. |
| R-0741 | C3 synthetic target layout and byte checks | Test expected multibyte bytes in both orders, length fit and target-parametric Hybrid round-trips. Name the exercised operations. |
| R-0528 / R-0730 | Work budgets for mandatory and optional checks | Check deterministic exhaustion under identical inputs and budgets, incomplete mandatory verification and validated identity fallback. |
| R-0742 | Source and IR spans, occurrences and discharge status | Test residual and discharged sites and reject a wrong origin. Extend to native locations under R-0517 once their mapping is validated. |
