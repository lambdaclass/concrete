### Group 1 — Improvements to the four gates

```
ITEM C1: AGREE-WITH-CHANGES
why: Wall-clock timing in `scaling.lean` (`IO.monoNanosNow`) is vulnerable to runner load spikes (cf. `ISSUES.tsv` line 10, `ISS-0006`). However, Lean heartbeats only measure elaborator/kernel reductions, not compiled `lake env lean --run` execution; for in-process compiled Lean passes, use explicit algorithmic step/visit counters (or deterministic allocation/byte counters from the Lean runtime) plus `perf stat -e instructions:u` on Linux (`concrete-ci-2`), keeping wall time secondary.
where it belongs: Gate C (`branches/C-gate-scaling-determinism.diff`, `scripts/checks/compiler/scaling.lean` and `scaling.sh`), now (slice 0 for BIR/backends; extended in slice 1 as front-end/lowering stages land).
size: SMALL
exists already?: no (`branches/C-gate-scaling-determinism.diff` at `scripts/checks/compiler/scaling.lean:371-381` uses `IO.monoNanosNow` and forces output digest length, with no step or instruction counters).
```

```
ITEM C2: AGREE-WITH-CHANGES
why: `scaling.lean:346` (`scaled k`) scales only one dimension (a linear chain of `2k+1` blocks with `6k` registers and `k` sites), which caught `ISS-0030` (`Backend/LLVM.lean:65` `phis` scanning `edges p` per block, and `Backend/LCB.lean:97` `numberingOk`/`regId` scanning `ids p`) but cannot isolate expression depth, local count within one block, or call-graph width. Add one generator per dimension present in the current IR, rather than stubbing slice-2+ dimensions (structs/enums/calls) before their IR exists.
where it belongs: Gate C (`scripts/checks/compiler/scaling.lean`), split across slices: block count, single-block instruction/register length, and multi-predecessor fan-in now (slice 0); expression nesting depth in slice 0 front end; structs/enums/calls at slices 5/9/10.
size: SMALL (for slice-0 dimensions) / LARGE (across all slices)
exists already?: partially in `branches/C-gate-scaling-determinism.diff` (`scripts/checks/compiler/scaling.lean:333-350`, block-chain family only).
```

```
ITEM C3: AGREE-WITH-CHANGES
why: Hardcoding `CORE = ["llvm_module", "lcb_emit", "lcb_assemble"]` in `scaling_report.py:774` will silently miss new compiler stages (`lex`, `parse`, `elab`, `lower`, `memory`, `ssa`) as they land. Derive the required stage list directly from `Concrete/Stages.lean` (once built; `Concrete/README.md:468`), require each stage to declare `O(n)` or `O(n log n)` there (one home per fact), and fit over 4 sizes (`1x, 4x, 16x, 64x` or `1x, 3x, 10x, 30x`) so `ISS-0030`-type superlinear bugs don't blow up the PR budget at `100x`.
where it belongs: Gate C (`scripts/checks/lib/scaling_report.py` and `Concrete/Stages.lean`), slice 0 (provisional list in `scaling.lean` now, bound to `Stages.lean` when front-end stages land).
size: SMALL
exists already?: partially in `branches/C-gate-scaling-determinism.diff` (`scripts/checks/lib/scaling_report.py:774,830-861` checks 3 hardcoded stages over 3 sizes `1x, 10x, 100x` against a uniform `1.35` exponent).
```

```
ITEM C4: AGREE
why: `scaling.lean:387-397` already computes output byte lengths (`String.length` on `LLVM.module` and `LCB.assemble`, `reprStr` length on `LCB.emit`) as its laziness-forcing digest (`measurement.2.2`) and prints them in column 6 of `samples.tsv`, but `scaling_report.py:811` discards column 6 (`_`) and never checks output-size scaling or peak RSS (`/usr/bin/time -v` / `getrusage(RUSAGE_CHILDREN)`). Checking output-size exponent is practically free (`<20` lines in `scaling_report.py`) and catches quadratic IR/table bloat immediately.
where it belongs: Gate C (`scripts/checks/lib/scaling_report.py` and `scripts/checks/compiler/scaling.sh`), now (slice 0).
size: SMALL
exists already?: partially in `branches/C-gate-scaling-determinism.diff` (`scripts/checks/compiler/scaling.lean:363,387-397` emits output sizes in `samples.tsv` field 6, but `scripts/checks/lib/scaling_report.py:811` ignores it; RSS is not measured).
```

```
ITEM C5: AGREE-WITH-CHANGES
why: A constant-factor `5x` slowdown across all sizes preserves a `1.0` scaling exponent and slips past Gate C, while `slice0_artifacts.sh:170` (`compile_times.tsv`) only checks loose timeout bounds (`emit_secs`, `native_secs`). Keep slope checks in PRs and enforce tight absolute counter/time budgets with append-only per-SHA history on `concrete-ci-2` in the daily tier, aligning with `ROADMAP.md:249-251` and `ISS-0006` (`ISSUES.tsv:10`).
where it belongs: Gate C (`scripts/checks/compiler/scaling.sh`) + Gate 11 (`scripts/checks/compiler/slice0_artifacts.sh`), now (slice 0, daily tier on `concrete-ci-2`).
size: SMALL
exists already?: partially in [slice0_artifacts.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/compiler/slice0_artifacts.sh#L74-L75) (`compile_times.tsv` records wall-clock ms and timeout budget, report-only).
```

```
ITEM C6: AGREE-WITH-CHANGES
why: `scaling.lean:360-361` defines `plantQuadratic` and `scaling.sh:521-526` supports `--allow-planted-failure --plant linear|quadratic`, but `scaling.sh` NEVER runs those controls during a normal CI invocation (unlike `determinism.sh:276`, `tests.sh:575`, and `spec_diff.sh:1021`). Furthermore, `scaling.sh:532` sets `in_process_only=1` and `scaling.lean:456` still runs all real stages during a plant run! Make `scaling.sh` invoke the `linear` (must pass) and `quadratic` (must fail) controls automatically on every run, executing *only* the synthetic plant function so the control costs `<50ms`.
where it belongs: Gate C (`branches/C-gate-scaling-determinism.diff`, `scripts/checks/compiler/scaling.sh` and `scaling.lean`), now (must be fixed before C lands).
size: SMALL
exists already?: partially in `branches/C-gate-scaling-determinism.diff` (`scripts/checks/compiler/scaling.lean:356-361` and `scaling.sh:521-526`, but NOT run in `.github/workflows/lean_action_ci.yml:80` or inside `scaling.sh`).
```

```
ITEM D1: AGREE-WITH-CHANGES
why: `determinism.sh:168-174` and `216-226` already perturb output path length, `cwd`, `HOME`, `TMPDIR`, and env-var order across two runs, but keep `LC_ALL=C`, `TZ=UTC`, `umask`, and input order fixed because `slice0_native.sh:24-25` scrubs the environment with `env -i`. Rather than multiplying native builds `N`-fold on every PR, test ambient `LC_ALL`, `TZ`, `umask`, `PYTHONHASHSEED`, ASLR (`setarch -R`), and shuffled CLI/manifest order in the daily perturbation matrix on `concrete-ci-2` ( verifying that the compiler driver itself, not just `slice0_native.sh`, is immune to ambient environment).
where it belongs: Gate C (`scripts/checks/compiler/determinism.sh`), split: 2-run path/cwd/HOME/TMPDIR perturbation in PR tier now; full perturbation matrix in daily tier on `concrete-ci-2`.
size: SMALL
exists already?: partially in `branches/C-gate-scaling-determinism.diff` (`scripts/checks/compiler/determinism.sh:168-174,213-228` perturbs path length, `cwd`, `HOME`, `TMPDIR`, and env order).
```

```
ITEM D2: AGREE
why: `determinism.sh:164-165` and `repro_compare.py:16` already compare every existing backend stage (`bir.txt`, `llvm.ll`, `lcb_job.txt`, `lcb_source.txt`, `lcb_body.ssa`, `lcb_body.s`, `lcb_rt.ssa`, `lcb_rt.s`, `.o`, `.exe`) in order so the first differing stage is named. As the slice-0 front end and lowering land (`Output` in `Concrete/README.md:60,369`: `toks, ast, p, sites, tokens, mir, mem, bir, ll, lcb`), `emit` must dump each intermediate (`toks.txt`, `ast.txt`, `core.txt`, `mir.txt`, `mem.txt`) and prepend them to `STAGES` in `repro_compare.py`.
where it belongs: Gate 8 (`scripts/checks/lib/repro_compare.py`) and Gate C (`scripts/checks/compiler/determinism.sh`), slice 0 (backend stages now; front-end/lowering stages as `Lex`, `Parse`, `Elab`, `MIR`, `Memory` land).
size: SMALL
exists already?: partially in [repro_compare.py](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/lib/repro_compare.py#L16) and `branches/C-gate-scaling-determinism.diff` (`scripts/checks/compiler/determinism.sh:164-165` for all BIR-and-below stages).
```

```
ITEM D3: AGREE
why: In `.github/workflows/lean_action_ci.yml:459-462`, `reproducibility` currently runs both `linux-first` and `linux-independent` on `ubuntu-24.04` (two GitHub-hosted runners), not one GitHub runner vs `concrete-ci-2` as required by `ROADMAP.md:253` and `ISS-0029`. Moreover, when `B`, `C`, or `D` changes the emitted stage set or manifest inventory, `repro-ci2` must use the exact same `slice0_repro.sh` and `repro_compare.py` stage list so cross-machine comparison never drifts.
where it belongs: Gate 8 (`.github/workflows/lean_action_ci.yml:456-502` and `scripts/checks/compiler/slice0_repro.sh`), now (slice 0).
size: SMALL
exists already?: partially in [.github/workflows/lean_action_ci.yml](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/.github/workflows/lean_action_ci.yml#L459-L502) (`compare-linux` compares two `ubuntu-24.04` runs; `ISS-0029` aligns `concrete-ci-2`).
```

```
ITEM A1: AGREE-WITH-CHANGES
why: `SpecDiffSpecGen.lean` and `SpecDiffBirGen.lean` already implement deterministic seeded type-directed generators (`forSeed seed index` via `splitmix64`), where any failure prints a one-command replay (`SPEC_DIFF_REPLAY=seed:index`). However, they lack a shrinker: when a 6-statement Core program or 6-block BIR program fails, shrinking by simplifying statements/blocks/immediates toward `0` before printing the `Concrete.BIR.Examples` snippet (`spec_diff.sh:852-858`) produces minimal regression fixtures automatically.
where it belongs: Gate A (`branches/A-gate-spec-diff.diff`, `scripts/checks/compiler/spec_diff.lean`), now (slice 0).
size: SMALL
exists already?: generator exists in `branches/A-gate-spec-diff.diff` (`Concrete/SpecDiffSpecGen.lean:271-350` and `Concrete/SpecDiffBirGen.lean:101-217`); shrinking does not ("no").
```

```
ITEM A2: AGREE-WITH-CHANGES
why: `spec_diff.sh:1010-1019` already enforces non-zero coverage of every generator production (`let_literal`, `add_variable`, etc.) and outcome (`exit`, `trap`), but does not track coverage of `ConcreteSpec` judgment constructors (`UseAtom`, `Uses`, `StmtOk`, `BodyOk`, and refusal rules). Add a Lean coverage collector over the spec's inductive/checker rules so Gate A (and `tests.sh` over `tests/manifest.tsv`) fails if any active slice rule has 0 hits.
where it belongs: Gate A (`scripts/checks/compiler/spec_diff.lean`) and Gate B (`scripts/checks/compiler/tests.sh`), slice 0 (over `Slice0` rules now; over full `ConcreteSpec` rules in slice 1).
size: SMALL
exists already?: production coverage exists in `branches/A-gate-spec-diff.diff` (`scripts/checks/compiler/spec_diff.sh:820-840,1010-1019`); spec-rule coverage does not ("no").
```

```
ITEM A3: AGREE-WITH-CHANGES
why: In `spec_diff.lean:658-660`, candidates rejected by `ConcreteSpec.check` are simply counted as `reject` and skipped (`if !ConcreteSpec.check core then rejected := rejected + 1`), never passed to `compileCore` to verify that the compiler also refuses them with the same rule ID! Today `ConcreteSpec.check` returns `Bool` (`ConcreteSpec/Spec/Slice1Checker.lean`), and `Slice0.lean` has no rule-ID diagnostic output yet (`Concrete/README.md:620`, owner decision 9). Once `check` and `compile` return rule IDs (slice 0 front-end / slice 1), Gate A must pass rejected candidates to the compiler and require `check` refusal iff `compile` refusal with identical rule ID.
where it belongs: Gate A (`scripts/checks/compiler/spec_diff.lean`) and Gate B (`scripts/checks/compiler/tests.sh`), blocked on front-end/checker refusal rule IDs (slice 0 front-end / slice 1).
size: SMALL (in `spec_diff.lean`, once rule IDs exist in `check` and `compile`)
exists already?: manifest schema exists in `branches/B-gates-manifest.diff` (`scripts/checks/lib/test_manifest.py:772`, `refuse RULE-ID`), and `Concrete/README.md:574` specifies it, but `spec_diff.lean:658` skips rejected programs.
```

```
ITEM A4: AGREE
why: `spec_diff.lean:454-486` has two built-in point mutations (`drop-overflow-llvm` and `lcb-off-by-one`) as fast PR controls, which prove the harness catches a broken output, not that the test/generator corpus kills arbitrary compiler mutants across `LLVM.lean`, `LCB.lean`, `MIR.lean`, `Memory.lean`, and `BIR.lean`. Running a systematic source-mutation campaign over unproved compiler passes (and statement/token-carrying logic) on `concrete-ci-2` weekly directly measures gate effectiveness.
where it belongs: New weekly gate `scripts/checks/compiler/mutation.sh` (reusing the concurrent-edit-safe design from `AGENTS.md:190-195`), starting now on `Concrete/Backend/LLVM.lean` and `Runtime.lean` (slice 0) and expanding in slice 1.
size: LARGE (ROADMAP row under B0/B2 qualification)
exists already?: point controls exist in `branches/A-gate-spec-diff.diff` (`scripts/checks/compiler/spec_diff.lean:454-486`); systematic compiler mutation testing does not ("no" on the new root).
```

```
ITEM A5: AGREE
why: `Concrete/README.md:48-50` and `ROADMAP.md:65-67,182` state that `adapter_correct` / `emit_correct` is already proved for BIR→LCB, that per-lowering proofs (`lower_correct`, `memory_correct`, `ssa_correct`) accompany each pass, and that LP0/Phase E extend proofs to native code. Documenting in `Concrete/README.md` and `ROADMAP.md` that differential gates shrink as stages move from `Premise` to `Proved`—turning into fast stage-contract coverage checks (Gate 3, `Concrete/Stages.lean`) plus executable cross-checks of theorems—keeps the long-term architecture clear.
where it belongs: `Concrete/README.md` (Gates section) and `ROADMAP.md` (B0 / LP0 / Phase E notes), now (slice 0).
size: SMALL
exists already?: partially in [Concrete/README.md](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/Concrete/README.md#L468-L503) and [ROADMAP.md](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/ROADMAP.md#L180-L183).
```

```
ITEM T1: AGREE-WITH-CHANGES
why: In `branches/D-trusted-base.diff`, `Concrete/Premises.lean:90-173` hand-writes 18 `Component` records alongside the derived environment audits (`axiomErrors`, `trustedDeclarationErrors`, `collectAxioms`) and Linux `strace` tool list. Keep only irreducible human metadata (`why`, `removal`, `slice`) in `Concrete/Premises.lean`, and *derive* the exact inventory of axioms, `extern`/`implemented_by`/`opaque`/`partial`/`unsafe` decls, Lake pins, toolchain hashes, and `strace`'d executables into `tests/golden/slice0_trusted_base.txt`; add a check in `review.sh` / `train.sh` that any diff growing `slice0_trusted_base.txt` requires explicit owner sign-off.
where it belongs: Gate D (`branches/D-trusted-base.diff`, `Concrete/Premises.lean` and `scripts/checks/compiler/slice0_trusted_base.sh`), now (slice 0).
size: SMALL
exists already?: partially in `branches/D-trusted-base.diff` (`Concrete/Premises.lean:225-308` derives theorem axioms, library axioms, and trusted declarations, and `slice0_trusted_base.sh:43-82` derives pins and `strace` tools, but `components` in `Premises.lean:90-173` is hand-written and no owner-sign-off check guards growth).
```

```
ITEM G1: AGREE
why: Currently, root checks live in `scripts/checks/run.sh:12` (`REQUIRED=...`), proof gates in `scripts/proofs/*.sh`, and compiler gates in `.github/workflows/lean_action_ci.yml`, with no machine-checked registry verifying that every gate has a tier, a time budget, and `>=1` planted control that fails for its planted reason. Adding `scripts/checks/gates_manifest.tsv` and a root check `scripts/checks/gate_manifest.sh` enforces the owner's no-vacuity and tier-budget rules structurally in one place.
where it belongs: Root checks (`scripts/checks/gates_manifest.tsv` and `scripts/checks/gate_manifest.sh`, wired into `scripts/checks/run.sh`), now (slice 0).
size: SMALL
exists already?: no (`required-checks.txt` lists only PR CI job names; `scripts/checks/run.sh:12` lists only root check basenames).
```

```
ITEM G2: AGREE-WITH-CHANGES
why: Distinguish *timeout recovery under load* from *behavioral or build flake*. `bounded_run` (`scripts/checks/lib/bounded.sh:104-135`) retries once ONLY when an attempt hits its wall-clock timeout (because host contention can stall a compile, `bounded.sh:5-8`), and a second timeout is `INCOMPLETE` (exit 3/124, failing the gate). However, `.github/workflows/lean_action_ci.yml:139-156` retries `leanprover/lean-action@v1` on *any* failure (because `lean-action` conflates network download with `lake build`!). Separate `elan` toolchain download from `lake build` in `lean_action_ci.yml` so a build or test failure is NEVER retried, and audit all scripts so any non-timeout non-zero exit or rerun disagreement fails immediately.
where it belongs: `.github/workflows/lean_action_ci.yml:138-318` and `scripts/checks/gate_manifest.sh` (root check), now (slice 0).
size: SMALL
exists already?: partially in [bounded.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/lib/bounded.sh#L112-L135) (only timeouts retry; non-zero exits do not retry), but violated in [.github/workflows/lean_action_ci.yml](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/.github/workflows/lean_action_ci.yml#L138-L156) where `lean-action` (which runs `lake build`) has `continue-on-error: true` and a retry step.
```

```
ITEM G3: AGREE
why: `scripts/checks/run.sh:21-23` prints elapsed seconds per root check, and `slice0_artifacts.sh:170` writes `compile_times.tsv`, but nothing records per-gate elapsed time across the whole suite or enforces per-gate PR time budgets. Have every gate emit a timing row (`gate\ttier\telapsed_ms\tbudget_ms\tstatus`) checked against `gates_manifest.tsv` by the meta-gate (G1), failing when a PR-tier gate exceeds its budget on `concrete-ci-2`.
where it belongs: Root checks (`scripts/checks/gate_manifest.sh`) and `scripts/checks/lib/` shared wrapper, now (slice 0).
size: SMALL
exists already?: partially in [run.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/run.sh#L21-L23) and [slice0_artifacts.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/compiler/slice0_artifacts.sh#L74-L75), but no cross-gate budget enforcement exists.
```

---

### Group 2 — Candidate new gates

```
ITEM N1: AGREE-WITH-CHANGES
why: `tests/manifest.tsv` (`branches/B-gates-manifest.diff`, `test_manifest.py:19,772`) already validates `refuse RULE-ID` syntax, and `Concrete/README.md:574,620` specifies that every refusal carries a Lean rule name + revision. Build the bidirectional check (`every rule ID in ConcreteSpec has >=1 refuse row in tests/manifest.tsv` and `every refuse row names an existing ConcreteSpec rule ID`), gated as active once `ConcreteSpec` attaches rule IDs to its refusal judgments and the slice-0 front end/checker lands.
where it belongs: Gate B (`scripts/checks/test_manifest.sh` and `scripts/checks/compiler/tests.sh`), blocked on rule-ID tags in `ConcreteSpec` and the slice-0 front-end/checker (`Surface0.lean` / `Elab.lean`).
size: SMALL
exists already?: syntax validation exists in `branches/B-gates-manifest.diff` (`scripts/checks/lib/test_manifest.py:19,772`); bidirectional rule-ID coverage does not ("no").
```

```
ITEM N2: AGREE
why: `ConcreteSpec/Spec/Slice0.lean:98-132` and `Concrete/BIR.lean:645-647` already include `example : ... := by decide` witnesses for positive and negative cases, and `scripts/proofs/trust_ban.sh:134-158` (`X: the examples, in the kernel's terms`) already rewrites every `example` into a named declaration and checks its axioms! Extending `scripts/proofs/` so every inductive judgment or spec law in `ConcreteSpec/Spec/` is paired with at least one positive witness `example` and one negative (`¬ ...` or `= false`) counter-example `example` in the environment is small, fast, and prevents vacuous definitions (like an unsatisfiable premise) before proofs are built on them.
where it belongs: Proof gates (`scripts/proofs/axioms.sh` or a new `scripts/proofs/vacuity.sh` in `scripts/proofs/`), now (slice 0, over `ConcreteSpec`).
size: SMALL
exists already?: partially in [Slice0.lean](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/ConcreteSpec/Spec/Slice0.lean#L98-L132), [BIR.lean](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/Concrete/BIR.lean#L645-L647), and [trust_ban.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/trust_ban.sh#L134-L158) (which enumerates all `example`s).
```

```
ITEM N3: AGREE-WITH-CHANGES
why: `ConcreteSpec/README.md:25-27` and `ROADMAP.md:48-50` (`R-0555`) require that decided rows in `docs/decisions.md` (`Q1.1..Q9.19`, `P1..P23`, `OD*`) map to `ConcreteSpec` declarations and vice versa. Because `docs/decisions.md` already covers slices 0 through 28 while `ConcreteSpec` currently implements only Slice 0 / Slice 1 fragments (with A2i in progress), the gate must key on the row's target slice: every decided row for slices `<= current_slice` (or Foundation C1–C10 once A2i lands) must cite an existing `ConcreteSpec` declaration and `tests/manifest.tsv` fixture, and every `ConcreteSpec/Spec/` rule doc comment must cite its decision row.
where it belongs: Root check (`scripts/checks/spec_traceability.sh`, or inside `scripts/spec/lean_site.sh`), starting at A2i / slice 0 for active-slice rows.
size: SMALL
exists already?: no (planned under `R-0555` in `ROADMAP.md:48-50`, not yet a check in `scripts/checks/`).
```

```
ITEM N4: AGREE-WITH-CHANGES
why: Lexer/parser fuzzing (`never panics/crashes, always terminates, refuses with a rule ID, and parse(print(ast)) == ast`) is explicitly on the slice-0 roadmap (`ROADMAP.md:180,261-263`). Note that `Concrete/Lex.lean`, `Concrete/Parse.lean`, and `ConcreteSpec/Spec/Surface0.lean` do not exist on `main` yet (only `Concrete/LL1.lean` exists). Build a deterministic seeded byte/token generator in `spec_diff.lean` (PR tier: small fixed count; Daily tier: 1h fuzzing) as soon as `Lex.lean` and `Parse.lean` land.
where it belongs: Gate A (`scripts/checks/compiler/spec_diff.sh` / `spec_diff.lean`), blocked on `Concrete/Lex.lean` and `Concrete/Parse.lean` (slice 0 front end).
size: SMALL
exists already?: no (`Concrete/LL1.lean` exists, but `Lex.lean` and `Parse.lean` are not built yet).
```

```
ITEM N5: AGREE
why: Confirming: backend agreement on the whole `bir:` manifest IS already implemented in `branches/B-gates-manifest.diff` (`scripts/checks/compiler/tests.sh:525-540,584-590`) and `scripts/checks/compiler/slice0_bir.lean:428-449`! For every row in `tests/manifest.tsv`, `slice0_bir.lean` checks that `BIR.run` and LCB's in-process evaluator `LCB.evalBody` agree with the manifest expectation, and `tests.sh` runs both `llvm.exe` and `lcb.exe` under `observe` and checks with `judge` that exit status, exact `stderr` bytes (including trap site/kind line), and empty `stdout` match. When `con:` rows land with the slice-0 front end, `tests.sh` must run the same 3-way check (`ConcreteSpec.run`, `llvm.exe`, `lcb.exe`) on every accepted `con:` row.
where it belongs: Gate B (`branches/B-gates-manifest.diff`, `scripts/checks/compiler/tests.sh`), exists now for `bir:` rows; extend to `con:` rows when the slice-0 front end lands.
size: SMALL
exists already?: yes, for `bir:` rows in `branches/B-gates-manifest.diff` (`scripts/checks/compiler/tests.sh:525-540,584-590` and `scripts/checks/compiler/slice0_bir.lean:428-449`).
```

```
ITEM N6: AGREE
why: "No hidden traps" (`R-0574`, `Concrete/README.md:526-528`) is a core Concrete guarantee: every trap site in emitted code must trace to a typed `SiteId`, source `Span`/`Loc`, and `Trap` kind in `SiteTable` (`Concrete/BIR.lean:77-84`), and an accepted slice-0 program (where all `+` have discharged-evidence tokens) must emit zero trap sites and zero trap branches in `.ll` and LCB assembly. We can check both properties right now on `tests/manifest.tsv` (e.g., `answer_discharged` has 0 trap sites in `llvm.ll` and `lcb_body.s`, whereas `overflow`, `site0`, `site1` map 1-to-1 to `SiteTable` entries).
where it belongs: Gate 10/11/12 (`scripts/checks/compiler/slice0_artifacts.sh`), now for BIR/discharged examples (slice 0) and extended to `concrete facts` when `Facts.lean` lands.
size: SMALL
exists already?: partially in [BIR.lean](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/Concrete/BIR.lean#L213-L220) (`wf` checks `trap s` has `s < p.sites.length` and `Loc.wf`) and `branches/B-gates-manifest.diff` (`slice0_bir.lean:445` checks `e.kind == expected.trapKind`), but no artifact-level audit checks that discharged programs emit zero trap paths in `.ll`/`.s`/`.exe`.
```

```
ITEM N7: AGREE
why: `Concrete/README.md:531` (`Trusted / FFI / opaque (artifact)`) notes that behavioral tests cannot rule out unexpected runtime/libc imports in the linked executable, and `scripts/checks/lib/stack_bound.py` (called by `slice0_artifacts.sh:138-150`) ALREADY parses disassembly and Mach-O indirect symbol tables to extract external calls (`ext`, which must be a subset of `{write, _exit, abort}`). Extending `slice0_artifacts.sh` with `llvm-readobj`/`llvm-nm` to check the dynamic symbol table (`UND` symbols = subset of `{write, _exit, abort}` plus standard loader stubs), banned sections, and absence of executable stack (`GNU_STACK` `RWE`) takes `<50` lines and closes the `Concrete/README.md:531` gap.
where it belongs: Gate 10/11/12 (`scripts/checks/compiler/slice0_artifacts.sh`), now (slice 0).
size: SMALL
exists already?: partially in [slice0_artifacts.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/compiler/slice0_artifacts.sh#L125-L150) (extracts `external` calls from disassembly via `stack_bound.py`), but no symbol-table/section/import audit on the ELF/Mach-O headers exists yet.
```

```
ITEM N8: AGREE
why: `scripts/checks/compiler/build_identity.sh:28-46` already hashes `flake.nix`, `flake.lock`, `lake-manifest.json`, `lean-toolchain`, `nixpkgs_rev`, and checks that every `.lake/packages/*` git checkout is clean at the exact pinned rev; and `scripts/proofs/trust_ban.sh:105-121` validates `lakefile.toml` against `lake-manifest.json`. What is missing is recording the exact Nix store derivation paths / SHA-256 digests of the `lean`, `clang`, and `llvm-objdump` binaries in `slice0_trusted_base.txt` (PR tier) and running a clean from-scratch nix + lake rebuild verification (Daily/Weekly tier on `concrete-ci-2`).
where it belongs: Gate D (`scripts/checks/compiler/slice0_trusted_base.sh` and `scripts/checks/compiler/build_identity.sh`), now (slice 0: store-binary digests in PR tier; clean rebuild in daily/weekly tier).
size: SMALL
exists already?: partially in [build_identity.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/checks/compiler/build_identity.sh#L28-L46), [trust_ban.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/trust_ban.sh#L105-L121), and `branches/D-trusted-base.diff` (`slice0_trusted_base.sh:36-41`).
```

```
ITEM N9: AGREE-WITH-CHANGES
why: Keeping documentation examples executable prevents prose drift (`ROADMAP.md:47-51,121-122`). Right now, there is no `docs/book` outside `legacy/`, and `ConcreteSpec/Spec/Slice0.lean:86-93` embeds surface `.con` examples in Lean doc comments alongside kernel-checked Core `example`s. Rather than inventing a second test runner for markdown blocks, require every code example in `ConcreteSpec` doc comments and future user-guide chapters to either reference a row ID in `tests/manifest.tsv` (one home per test, extracted and compared byte-for-byte) or be checked by a small extractor script feeding `tests.sh`.
where it belongs: Root check (`scripts/checks/doc_links.sh` or `scripts/checks/test_manifest.sh`) + `scripts/checks/compiler/tests.sh`, starting when `con:` surface files land in slice 0.
size: SMALL
exists already?: no (`ConcreteSpec/Spec/Slice0.lean:86-132` checks the Core translations by `decide`, not the doc-comment surface strings).
```

```
ITEM N10: AGREE
why: Proof fragility and elaboration timeouts are a major long-term risk as slices accumulate (`Concrete/README.md:464` also requires measured budgets for certificates). Measuring per-module build/elaboration time and `leanchecker` kernel-replay time (`scripts/proofs/kernel_replay.sh:84-94` already times total `leanchecker` in seconds), enforcing a hard per-module cap in PRs, and recording per-theorem/per-module elaboration heartbeats and kernel time history on `concrete-ci-2` daily catches proof-search blowups before they hit timeout walls.
where it belongs: `scripts/proofs/kernel_replay.sh` (PR hard cap per module) + a daily proof-profile script on `concrete-ci-2` (`scripts/proofs/proof_budget.sh`), now (slice 0).
size: SMALL
exists already?: partially in [kernel_replay.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/kernel_replay.sh#L84-L94) and [external.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/external.sh#L184-L187) (records whole-library replay seconds in log output, no per-module/per-theorem budget or history).
```

```
ITEM N11: AGREE
why: Look at `branches/A-gate-spec-diff.diff` lines 47-58 (`Concrete.lean`): Gate A's diff literally adds `import Concrete.SpecDiffRandom`, `import Concrete.SpecDiffSpecGen`, and `import Concrete.SpecDiffBirGen` to `Concrete.lean`, polluting the production compiler library (and Gate D's `concreteDeclarations` trusted-base audit in `Concrete/Premises.lean:242`!) with test generators! Move `SpecDiff*.lean` under `scripts/checks/compiler/` (imported only by `spec_diff.lean`), and add an import-layering check that `ConcreteSpec` imports only `ConcreteSpec`, `Concrete` imports only `ConcreteSpec`, `Concrete`, and `LeanCompilerBackend`, and neither imports test/generator code.
where it belongs: Gate A fix round (`branches/A-gate-spec-diff.diff`, removing `SpecDiff*.lean` from `Concrete.lean` and `Concrete/`) + Gate D / `scripts/proofs/trust_ban.sh`, now (must be fixed before A and D land together).
size: SMALL
exists already?: `ConcreteSpec` import isolation exists in [trust_ban.sh](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/trust_ban.sh#L39-L44,L233-L240); `Concrete` import layering does not ("no", and `branches/A-gate-spec-diff.diff` lines 51-58 violates it).
```

```
ITEM N12: AGREE
why: `Concrete/README.md:60,468-503` ("Stage registry (`Concrete/Stages.lean`)") already designs Gate 3: enumerate the constants `compile` calls from its elaborated term, require each to be a registered stage in `Stages.lean`, and require each stage to have a contract theorem/premise and at least one gate in `gates_manifest.tsv` (G1) that exercises it and plants a failure in it.
where it belongs: Gate 3 (`Concrete/Stages.lean` + `scripts/checks/gate_manifest.sh`), now for backend stages (`LLVM.print`, `LCB.emit`, `LCB.assemble`) and extended in slice 0 as `Stages.lean` lands.
size: SMALL
exists already?: designed in [Concrete/README.md](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/Concrete/README.md#L468-L503) (Gate 3), not yet implemented in code ("no").
```

---

### Group 3 — Up to 5 further gates or improvements (`X1..X5`)

```
ITEM X1: AGREE (Extend `scripts/proofs/` trust ban, kernel replay, and external re-check to `Concrete`)
why: `Concrete/README.md:551-557` (Gate 2) notes that `scripts/proofs/trust_ban.sh`, `kernel_replay.sh`, and `external.sh` currently check `ConcreteSpec` only—they do NOT check `Concrete` (so `Concrete/BIR/Progress.lean` and `Concrete/Backend/LCB.lean`'s 2,100-line `adapter_correct` proof are not replayed through `leanchecker`, `con-leche`, or `nanoda`, nor scanned for `debug.skipKernelTC` or `example` axioms!). Passing `Concrete` to `kernel_replay.sh`, `external.sh`, and `trust_ban.sh` (allowing only `Premises.lean`-listed declarations) closes a real soundness hole on code already on `main`.
where it belongs: `scripts/proofs/kernel_replay.sh`, `scripts/proofs/external.sh`, and `scripts/proofs/trust_ban.sh` (Gate 2 in `Concrete/README.md:551`), now (slice 0).
size: SMALL
exists already?: exists for `ConcreteSpec` in `scripts/proofs/*.sh`; "no" for `Concrete` (`Concrete/README.md:557` explicitly notes "Not yet a job").
```

```
ITEM X2: AGREE (Deduplicate `slice0_bir.lean` and `spec_diff.lean` emitter/judge glue into one module)
why: `branches/A-gate-spec-diff.diff` (`scripts/checks/compiler/spec_diff.lean:400-448`) copies `writeExpected`, `writeLcb`, and `writeBackends` verbatim from `scripts/checks/compiler/slice0_bir.lean:409-435`, violating the owner's "no duplication (one form per situation, one home per fact)" rule. Extract the shared backend-emission and observation-formatting functions into a single helper module `scripts/checks/compiler/ EmitCommon.lean` (or `Concrete/Driver.lean` once `Main.lean` lands) imported by `slice0_bir.lean`, `spec_diff.lean`, and `scaling.lean`.
where it belongs: Gate A / B / C shared Lean helper (`scripts/checks/compiler/`), now (in the A/C fix round).
size: SMALL
exists already?: duplicated across `scripts/checks/compiler/slice0_bir.lean:409-435`, `branches/A-gate-spec-diff.diff` (`spec_diff.lean:400-448`), and `branches/C-gate-scaling-determinism.diff` (`scaling.lean:432-448`).
```

```
ITEM X3: AGREE (Printer injectivity / round-trip gate for `LLVM.print` and `BIR`)
why: `Concrete/README.md:420` states that the LLVM native-model consistency witness and per-compilation certificate rely on **printer injectivity**: `LLVM.unprint (LLVM.print b) = some b` for every well-formed BIR program `b`, so the `.ll` text uniquely determines its BIR. Running `LLVM.unprint (LLVM.print b) == some b` on every `tests/manifest.tsv` program and every generated BIR candidate in `spec_diff.lean` (with a planted control that drops an instruction or swaps two phi arms) catches any printer ambiguity or silent instruction drop immediately.
where it belongs: Gate A (`scripts/checks/compiler/spec_diff.lean`) and Gate B (`scripts/checks/compiler/slice0_bir.lean`), slice 0.
size: SMALL
exists already?: designed in [Concrete/README.md](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/Concrete/README.md#L420), not yet implemented in `Concrete/Backend/LLVM.lean` ("no").
```

```
ITEM X4: AGREE (Premise-definition digest gate — Gate 3b)
why: `Concrete/README.md:499-502` specifies that Gate 3 must compare the structural digests (canonical AST/environment hashes from `scripts/proofs/theorems.lean:123` `declLine`) of every definition mentioned in theorem statements and premises (`Behaviour`, `BIR.behaviour`, `BIR.WF`, `LCB.bodyResult`, `encode`, `decode`, `IntTy.inRange`) against a golden file. Gate D (`branches/D-trusted-base.diff`) checks axiom closures and `Premises.lean` text strings, but if a change weakens `BIR.WF` or `LCB.bodyResult`, `emit_correct` still typechecks with the same name and axioms! Reusing `theorems.lean`'s canonical `Dag` renderer (`PROOF_MODE=decls`) to pin the definitions of headline theorem dependencies closes this gap in `<60` lines.
where it belongs: Gate D (`scripts/checks/compiler/slice0_trusted_base.sh`, reusing `scripts/proofs/theorems.lean` `declLine`), now (slice 0).
size: SMALL
exists already?: `declLine` exists in [theorems.lean](file:///home/unbalancedparen/projects/gates-briefs/prop-agy/scripts/proofs/theorems.lean#L123-L151); premise-definition pinning in Gate 9 does not ("no").
```

---

### TIERS

#### 1. Per-item tier assignment and estimated PR-tier cost (on `concrete-ci-2` / warm Lake cache)

| Item | Recommended Tier | Est. PR Cost (s) | Notes on Split / Execution |
|---|---|---|---|
| **C1** | **Split (PR + Daily)** | ~15s | **PR:** Deterministic step/allocation counters at small sizes (`1x, 4x, 16x`), in-process only. **Daily:** `perf stat -e instructions:u` + wall-clock cross-check on `concrete-ci-2`. |
| **C2** | **Split (PR + Daily)** | (in C1) | **PR:** Small-size step counts across active slice-0 dimensions (blocks, single-block length, phi fan-in) in one Lean process. **Daily:** Large sizes (`1x..100x`) + native `clang`/`lcb` builds per dimension family. |
| **C3** | **PR** | ~1s | Stage-complexity manifest check against `Stages.lean` + 4-size exponent fit over C1's deterministic counters. |
| **C4** | **Split (PR + Daily)** | ~0s (in C1) | **PR:** Output byte-size scaling exponent (already computed in `scaling.lean`). **Daily:** Peak RSS memory scaling across large sizes. |
| **C5** | **Daily** | 0s | Absolute wall-time/step budgets and append-only SHA history on `concrete-ci-2`. |
| **C6** | **PR** | ~1s | Planted `linear` (pass) and `quadratic` (fail) synthetic controls run automatically in `scaling.sh` (in-process synthetic stage only). |
| **D1** | **Split (PR + Daily)** | ~35s (shared) | **PR:** In-process `emit-twice` + 2-run path/cwd/HOME/TMPDIR determinism check folded into `Tests`/`Artifacts` build. **Daily:** Full env, locale, TZ, umask, input-order, hash-seed, ASLR perturbation matrix. |
| **D2** | **PR** | ~1s | Intermediate stage printer comparison (`toks..bir..exe`) inside `repro_compare.py`. |
| **D3** | **PR** | ~180s (parallel job) | Cross-machine reproducibility (`ubuntu-24.04` vs `concrete-ci-2`, `ISS-0029`), already required. |
| **A1** | **Split (PR + Daily)** | ~30s | **PR:** Seeded generator (`SPEC_DIFF_SEED=fixed`, e.g., 32 candidates native + 300 in-process `BIR.run` vs `LCB.evalBody`) with automatic shrinker on failure. **Daily:** 1-hour random-seed campaign (`SPEC_DIFF_N=5000+`). |
| **A2** | **PR** | ~2s | Spec-rule coverage check over `tests/manifest.tsv` fixtures + PR seeded batch. |
| **A3** | **PR** | ~5s | Refusal agreement (`check` iff `compile` with same rule ID) on `tests/manifest.tsv` and PR seeded batch (once slice-0/1 rule IDs land). |
| **A4** | **Weekly** | 0s | Systematic compiler mutation testing campaign on `concrete-ci-2`. |
| **A5** | **PR (doc/Gate 3)** | 0s | Roadmap/README documentation now; Gate 3 stage-proof check as proofs land. |
| **T1** | **PR** | ~15s | Derived trusted-base golden (`collectAxioms`, `trustedDeclarations`, pins, Linux `strace` on 1 representative build + controls, owner-sign-off check on growth). |
| **G1** | **PR** | ~3s | Meta-gate (`scripts/checks/gate_manifest.sh` in `Root checks`) verifying every gate's tier, budget, and planted control. |
| **G2** | **PR** | ~1s | Workflow/script audit banning non-timeout retry-passes; separate `elan` install retry from `lake build`. |
| **G3** | **PR** | ~1s | Per-gate timing log and PR budget enforcement inside `gate_manifest.sh`. |
| **N1** | **PR** | ~2s | Bidirectional rule-ID <-> refusal fixture check in `test_manifest.sh` (once rule IDs land). |
| **N2** | **PR** | ~10s | Mechanized vacuity check over `ConcreteSpec` examples (folded into `Proof trust ban` step `X`, which already builds all `example`s). |
| **N3** | **PR** | ~2s | Decision <-> spec traceability check in `Root checks` (`scripts/checks/spec_traceability.sh`). |
| **N4** | **Split (PR + Daily)** | ~10s | **PR:** 200 seeded lex/parse/round-trip inputs + refusal rule-ID controls (once `Lex`/`Parse` land). **Daily:** 1-hour lexer/parser fuzzer on `concrete-ci-2`. |
| **N5** | **PR** | ~45s (existing) | Backend agreement (`Lean` vs `LLVM` vs `LCB`) on `tests/manifest.tsv` (`Tests (x86-64)` and `Tests (aarch64-macos)`). |
| **N6** | **PR** | ~2s | Trap-site accounting (`SiteTable` bijection + zero trap code in discharged programs), folded into `slice0_artifacts.sh`. |
| **N7** | **PR** | ~3s | Output binary symbol/section/import audit (`llvm-readobj`/`llvm-nm`), folded into `slice0_artifacts.sh`. |
| **N8** | **Split (PR + Daily/Weekly)** | ~2s | **PR:** Toolchain/store-path hash check in `build_identity.sh` / `slice0_trusted_base.sh`. **Daily:** Clean compiler+LCB rebuild. **Weekly:** From-scratch Nix + toolchain rebuild. |
| **N9** | **PR** | ~5s | Doc examples mapped to `tests/manifest.tsv` and checked by `tests.sh`. |
| **N10** | **Split (PR + Daily)** | ~2s | **PR:** Hard per-module elaboration and `leanchecker` time cap. **Daily:** Per-theorem heartbeat/time history on `concrete-ci-2`. |
| **N11** | **PR** | ~2s | Import layering check for `ConcreteSpec` and `Concrete` (in `trust_ban.sh` and `slice0_trusted_base_audit.lean`). |
| **N12** | **PR** | ~2s | Stage-to-gate coverage check in `gate_manifest.sh` / `Stages.lean`. |
| **X1** | **PR** | ~25s | Extend `kernel_replay.sh`, `external.sh`, and `trust_ban.sh` to `Concrete` library. |
| **X2** | **PR** | 0s (saves time) | Shared `EmitCommon.lean` module eliminating duplicate driver code across A, B, C. |
| **X3** | **PR** | ~2s | `LLVM.unprint (LLVM.print b) == some b` check in `slice0_bir.lean` / `spec_diff.lean`. |
| **X4** | **PR** | ~3s | Canonical AST digest check (`declLine`) for definitions used by premises/headline theorems. |

---

#### 2. Is the 10-minute PR budget realistic? What must change or move to Daily?

**As currently structured in `.github/workflows/lean_action_ci.yml` and `branches/A,C,D.diff`, a 10-minute PR budget is NOT realistic unless we eliminate redundant builds and shrink PR-tier sample counts.**

Look at why `.github/workflows/lean_action_ci.yml` + `A,B,C,D` is currently slow:
1. **No Lake build cache across compiler jobs, and every job rebuilds `Concrete` and `lean-compiler-backend/lcb` from scratch.** In `lean_action_ci.yml:357-479` + `A,C,D`, there are **11 separate compiler matrix jobs** (`Tests` x2, `Spec differential` x2, `Determinism` x2, `Scaling` x1, `Slice 0 artifacts` x2, `Slice 0 trusted base` x2, `Reproducibility` x3). None of them uses `lean-action` or `actions/cache` for `.lake`, and `slice0_common.sh:50` runs `lake build Concrete lean-compiler-backend/lcb` (with a 2,100-line `Concrete/Backend/LCB.lean` proof!) at the start of every single job. On `concrete-ci-2` (8 self-hosted runners capped at 24 cores total in `concrete-runners.slice`, `ISS-0007`, `ISSUES.tsv:11`), 11 jobs compiling `Concrete` + `lcb` + `ConcreteSpec` simultaneously will saturate the 24-core slice and take 15–25+ minutes!
2. **Repeated native compilation of the same 32 `manifest.tsv` examples.** Across `Tests`, `Slice 0 artifacts`, `Determinism` (2x), `Trusted base` (which in `D-trusted-base.diff:496-510` `strace`s all 32 `bir:` emissions and native builds!), and `Reproducibility` (2x), each PR compiles the same 32 BIR examples through `lean --run`, `clang`, and `lcb` **7 times on Linux and 5 times on macOS**.
3. **Gate A (`spec_diff.sh:781`) runs up to `N=300` native builds (`clang` + `lcb` + 2 executables each) per arch with a 600s budget.** On `macos-15` and under load, 150–200 native links alone take 5–10 minutes.
4. **Gate C (`scaling.sh:557-575`) runs 5 repetitions of `1x, 10x, 100x` native builds and `100x` wall-clock timing.** Before `ISS-0030` lands, `100x` takes minutes; even after `ISS-0030` lands, 5x interleaved `100x` native builds is noisy on `ubuntu-latest` and heavy.

**What must move to Daily or be consolidated to hit the ~10-minute PR wall-clock budget:**
1. **Build `Concrete` + `lcb` ONCE per arch (or cache `.lake/build` keyed on `build_identity.sh`'s `source_sha256`)** for all non-reproducibility jobs (`Tests`, `Artifacts`, `Trusted base`, `Determinism`, `Spec differential`, `Scaling`). Only Gate 8 (`Reproducibility`) needs `LAKE_ARTIFACT_CACHE=false` from-scratch builds.
2. **Move large-size native scaling (`100x` x 5 reps, wall-clock exponent, RSS, multi-dimension families) to Daily on `concrete-ci-2`.** In the PR tier, run **in-process deterministic counter + output-size scaling** at small sizes (`1x, 4x, 16x`) plus the planted `linear`/`quadratic` controls (`<15s` total).
3. **Cap PR-tier `spec_diff.sh` native builds at `SPEC_DIFF_NATIVE_N=24` (fixed seed `SPEC_DIFF_SEED=slice0-pr-v1`), while keeping `SPEC_DIFF_N=300` for fast in-process differential (`BIR.run` vs `LCB.evalBody` vs `LLVM.unprint`).** Move the 300+ native-build campaign and random-seed exploration to Daily on `concrete-ci-2`.
4. **In Gate D (`slice0_trusted_base.sh:496-510`), `strace` 1 representative `bir:` build + the `objcopy`/`without-clang` controls in PR (~5s), instead of running `lake env lean --run` + `clang` under `strace -f` 32 separate times in a bash loop.** (Or collect the `strace` log during `Tests (x86-64)`).
5. **Fold per-test `Determinism` (`determinism.sh`) into `Tests` or `Reproducibility`**, rather than spinning up two more full-checkout VM jobs (`Determinism (x86-64)` and `Determinism (aarch64-macos)`).

With those 5 adjustments, total added PR work is **~2.5 minutes of CPU time** and fits comfortably inside the **10-minute PR wall-clock budget** even on the capped `concrete-runners.slice`.

---

#### 3. How scheduled tiers (Daily / Weekly) should be run, recorded, and monitored for "missing"

To stay strictly within existing repository conventions (`ISSUES.tsv`, `required-checks.txt`, `scripts/checks/`, git notes `refs/notes/*`):

1. **How they run (GitHub Actions `schedule` + `workflow_dispatch` targeting self-hosted `concrete-ci-2`, NOT an unrecorded local systemd timer):**
   - Use a dedicated workflow `.github/workflows/scheduled_gates.yml` with `on: schedule:` (one cron for `daily`, one cron for `weekly`) plus `workflow_dispatch: inputs: { tier: daily|weekly, sha: ... }`, running on `["self-hosted", "linux", "x64", "concrete"]` (`concrete-ci-2`) under the existing machine-wide heavy-run semaphore `scripts/checks/lib/heavy_slots.sh` (`AGENTS.md:224-226`).
   - Why GitHub workflow on `concrete-ci-2` instead of a bare systemd timer? Because `train.sh`, `landing_script.sh`, and `gh run list --commit <full-sha>` (`AGENTS.md:292`) already query GitHub Actions runs and artifacts; logs and TSV artifacts (`scaling.tsv`, `proof_times.tsv`, `fuzz_summary.tsv`) are automatically archived and inspectable via `gh run view` from any machine.
   - Add a **lightweight systemd watchdog timer on `concrete-ci-2` (or a small check in `train.sh` / `/cstatus`)** whose ONLY job is to query the GitHub API (`gh api`) once a day: if GitHub's cron scheduler dropped or delayed the scheduled run, the timer dispatches `gh workflow run scheduled_gates.yml -f tier=daily`.

2. **Where results are recorded per SHA:**
   - **Git notes on `origin` (`refs/notes/concrete-gates-daily` and `refs/notes/concrete-gates-weekly`)**, following the exact pattern of `refs/notes/concrete-review` (`AGENTS.md:298-303`, `ISS-0015`). Each completed scheduled run writes a structured note on the exact tested `main` SHA (`tier: daily`, `sha: <40-hex>`, `run_id: <id>`, `timestamp: <iso8601>`, `verdict: PASS | FAIL | INCOMPLETE`, `summary: ...`) and pushes it forward-only to `origin`.
   - **Append-only TSV history on `concrete-ci-2`** (`/var/lib/concrete-ci/history/{scaling,proof_times,sizes}.tsv`, backed up as a workflow artifact on each run) keyed by `(timestamp, full_sha, target, backend, metric, value)` as required by `ROADMAP.md:249-251`.

3. **How failures and "missing" / cancelled / skipped runs are detected and reported:**
   - **No silent pass on skip/cancel:** In `scheduled_gates.yml`, a final `verdict` job (`if: always()`) checks that every scheduled job for that tier ran at the exact `HEAD` SHA and exited `0`. If any job was `cancelled`, `skipped`, or exited `3` (`INCOMPLETE`), the run records `verdict: INCOMPLETE` (never `PASS`).
   - **Freshness check in `/cstatus` and `scripts/land/ready.sh` (non-blocking for unrelated PRs, loud to agents/owner):** `ready.sh` and `/cstatus` fetch `refs/notes/concrete-gates-daily` and `refs/notes/concrete-gates-weekly` and check:
     1. Did a `daily` run complete with `PASS` within the last 30 hours (and `weekly` within 8 days)? If not (or if the latest run is `cancelled`/`skipped`/`INCOMPLETE`), report `SCHEDULED-TIER MISSING: <tier> (last pass: <sha> at <time>)`.
     2. Did the latest `daily` or `weekly` run record `FAIL`? If so, report `STOP-THE-LINE: <tier> failed at <sha> (run <id>)`.
   - **Stop-the-line defect filing:** On a `FAIL`, a row is added to `ISSUES.tsv` (`area: compiler` or `ci`, `severity: high`, `evidence: <run_id> <sha> <reproducer seed:index>`, `closure_condition: fix landed with a PR-tier regression row in tests/manifest.tsv`), and the printed reproducer is added to `tests/manifest.tsv` as part of the fix branch.

---

#### 4. Build order of small branches (dependencies first, marking slice-1/front-end blockers)

**Phase 1 — Land now on Slice 0 (no front-end or slice-1 dependency):**
1. **`wip/gate-b-manifest` (`B-gates-manifest.diff`, already accepted):** Lands `tests/manifest.tsv`, `test_manifest.py`, `scripts/checks/test_manifest.sh`, and `scripts/checks/compiler/tests.sh` (covers **B, N5**).
2. **`wip/shared-emit-and-layering` (Pre-A/C/D cleanup — X2 + N11):**
   - Extract shared `writeExpected`/`writeLcb`/`writeBackends` from `slice0_bir.lean` into `scripts/checks/compiler/EmitCommon.lean` (**X2**).
   - Add import-layering check to `scripts/proofs/trust_ban.sh` ensuring `Concrete.lean` and `Concrete/*` never import test generators (**N11**), with a planted bad-import control.
3. **`wip/gate-d-trusted-base` (`D-trusted-base.diff` + T1 + N8-PR + X4):**
   - Land `Concrete/Premises.lean` component registry + derived axiom/trusted-decl audits + Linux `strace` audit (**Gate D**).
   - Include store-binary SHA256s in `build_identity.sh`, optimize `trace_all` to trace 1 build + controls in PR, add canonical AST digest check (`declLine`) for premise definitions (**X4**), and add owner-sign-off check on trusted-base growth (**T1, N8**).
4. **`wip/fix-iss0030-scaling` (Fix `ISS-0030` in `Backend/LLVM.lean` and `Backend/LCB.lean`):**
   - Must land *before* Gate C becomes a required check so `Scaling (x86-64)` is green on `main`.Precompute incoming-edge maps in `LLVM.lean:57` (`edges`/`phis`) and `LCB.lean:156` (`incoming`), and use a `Std.HashMap` / `Array` for `ids`/`regId`/`numberingOk` in `LCB.lean:75-104`.
5. **`wip/gate-c-scaling-determinism` (`C-gate-scaling-determinism.diff` + C1 + C3 + C4 + C6 + D1-PR + D2-BIR):**
   - Wire automatic execution of `linear` (pass) and `quadratic` (fail) controls into `scaling.sh` (**C6**).
   - Check output-size scaling exponent from column 6 of `samples.tsv` (**C4**), add deterministic step counters at 4 small sizes (`1x, 4x, 16x, 64x`) (**C1, C3**), and fold per-test determinism into CI (**D1-PR, D2**).
6. **`wip/gate-a-spec-diff` (`A-gate-spec-diff.diff` + A1 + X3):**
   - Move `SpecDiffRandom.lean`, `SpecDiffSpecGen.lean`, `SpecDiffBirGen.lean` out of `Concrete/` into `scripts/checks/compiler/` (fixing **N11**).
   - Add candidate shrinking on failure (**A1**), `LLVM.unprint (LLVM.print b) == some b` round-trip check (**X3**), and split PR (`SPEC_DIFF_NATIVE_N=24`, `SPEC_DIFF_N=300` in-process) vs Daily parameters.
7. **`wip/proof-gates-concrete-and-vacuity` (X1 + N2 + N10-PR):**
   - Extend `scripts/proofs/trust_ban.sh`, `kernel_replay.sh`, and `external.sh` to replay and check `Concrete` in addition to `ConcreteSpec` (**X1**).
   - Add mechanized vacuity check (every `ConcreteSpec` law/judgment has positive + negative `example` witnesses) (**N2**) and per-module replay time cap (**N10-PR**).
8. **`wip/artifact-trap-and-binary-audit` (N6 + N7):**
   - Extend `scripts/checks/compiler/slice0_artifacts.sh` to verify zero trap code/sites in discharged programs (`answer_discharged`) and `SiteTable` bijection in trapping ones (**N6**), plus `llvm-readobj` symbol/section/import audit (`UND` symbols subset of `{write, _exit, abort}`, no `RWE` stack) with planted bad-symbol control (**N7**).
9. **`wip/meta-gate-and-ci-tiers` (G1 + G2 + G3 + D3 + A5):**
   - Add `scripts/checks/gates_manifest.tsv` and `scripts/checks/gate_manifest.sh` (declaring every gate's property, planted control, tier, and PR time budget, **G1, G3**).
   - Separate `elan` download retry from `lake build` in `.github/workflows/lean_action_ci.yml` (**G2**), keep `repro-ci2` aligned (**D3**), add `.github/workflows/scheduled_gates.yml` for Daily/Weekly tiers on `concrete-ci-2` (**C2-daily, C5, D1-daily, N8-daily, N10-daily**), and update `Concrete/README.md` (**A5**).
10. **`wip/spec-traceability` (N3):**
    - Add `scripts/checks/spec_traceability.sh` checking bidirectional links between active-slice decided rows in `docs/decisions.md` and `ConcreteSpec` declarations (completes with A2i).
11. **`wip/weekly-mutation` (A4):**
    - Add `scripts/checks/compiler/mutation.sh` to the weekly workflow on `concrete-ci-2`, mutating `Concrete/Backend/LLVM.lean` and `Runtime.lean` and requiring `tests.sh` / `spec_diff.sh` to kill every mutant.

**Phase 2 — Blocked on Slice-0 Front End (`Surface0.lean`, `Lex.lean`, `Parse.lean`, `Elab.lean`, `Stages.lean`) or Slice-1 Lowering (`MIR.lean`, `Memory.lean`, `compileCore`):**
12. **`wip/stage-registry-gate` (N12 + C3-full + D2-frontend):** Blocked on `Concrete/Stages.lean` and front-end stages landing. Binds `scaling_report.py`, `repro_compare.py`, and `gate_manifest.sh` to `Stages.lean`.
13. **`wip/frontend-fuzz-and-doc-examples` (N4 + N9):** Blocked on `Concrete/Lex.lean` and `Concrete/Parse.lean`. Adds PR + Daily lexer/parser fuzzing and round-trip `parse(print(ast)) == ast` (**N4**) and wires doc-comment `.con` examples to `tests/manifest.tsv` (**N9**).
14. **`wip/refusal-rule-ids-and-coverage` (N1 + A2 + A3):** Blocked on refusal rule IDs in `ConcreteSpec` and front-end/checker refusal output. Enforces bidirectional rule-ID <-> `manifest.tsv` refusal coverage (**N1**), spec-rule coverage (**A2**), and differential refusal agreement (**A3**).
15. **`wip/wire-compileCore-spec-diff` (Gate A `compileCore` hook):** Blocked on Core→MIR→Memory→BIR lowering (`Concrete/README.md:580`). Wires `compileCore` in `spec_diff.lean` and requires `spec_judged > 0`.

---

### Ranked Top-5 Items by Payoff/Cost for the Next Two Weeks

1. **N11 + X2 (Fix Gate A's import layering & deduplicate emitter glue before landing A/C):** `branches/A-gate-spec-diff.diff` currently imports `SpecDiff*.lean` into `Concrete.lean`, polluting the compiler library and colliding with Gate D's `Concrete` declaration audit, while duplicating `slice0_bir.lean`'s backend emission code. Fixing this in the A/C fix round costs ~50 lines and prevents immediate technical debt and a Gate A vs Gate D conflict.
2. **C6 + C1 + C4 (Run Gate C's planted controls automatically + deterministic counters & output-size scaling):** Gate C (`C-gate-scaling-determinism.diff`) already proved its worth by catching `ISS-0030` (cubic LCB adapter, quadratic LLVM printer), but its planted quadratic control is never executed in CI and column 6 (output byte size) is thrown away by `scaling_report.py`. Running the planted controls automatically and checking deterministic step/size scaling takes `<80` lines and makes Gate C noise-free and non-vacuous.
3. **X1 + N2 (Extend `scripts/proofs/` kernel replay, external re-check, and trust ban to `Concrete` + mechanized vacuity):** `Concrete/Backend/LCB.lean` already contains 2,100 lines of kernel proofs (`adapter_correct`, `emit_correct`), yet `kernel_replay.sh` (`leanchecker`), `external.sh` (`con-leche`/`nanoda`), and `trust_ban.sh` only check `ConcreteSpec`. Extending them to `Concrete` and checking positive/negative `example` witnesses closes the biggest soundness-verification gap on `main` today.
4. **G1 + G2 + G3 (Gate manifest meta-gate, tier/budget enforcement, and separating `elan` retry from `lake build`):** With 30+ gate items approved across PR, Daily, and Weekly tiers, a single `gates_manifest.tsv` checked in `Root checks` (enforcing that every gate declares its tier, stays within its PR time budget, has `>=1` planted control, and never retries a failed build/test) is the structural backbone that keeps PRs under 10 minutes and prevents vacuous gates.
5. **N6 + N7 (Artifact trap-site accounting & ELF/Mach-O binary symbol/section audit in `slice0_artifacts.sh`):** `slice0_artifacts.sh` already disassembles every `manifest.tsv` executable for stack bounds; adding `<80` lines to verify that discharged programs (`answer_discharged`) contain zero trap paths and that linked binaries import only `{write, _exit, abort}` with no executable stack immediately upgrades two `notEstablished` rows in `Concrete/README.md`'s Facts table (lines 528, 531) at near-zero CI cost.

DONE
agy rc=0
