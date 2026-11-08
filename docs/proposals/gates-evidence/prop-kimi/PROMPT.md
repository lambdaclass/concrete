You are one of three reviewers (Codex, Kimi, agy) judging proposals to improve and extend the Concrete project's CI gates. The current directory is a read-only export of main at ea212942 (the new compiler's slice 0 just landed; legacy/ is the frozen old compiler and is not built). branches/*.diff are four gate branches under review, not on main yet:
- B-gates-manifest.diff: one test manifest tests/manifest.tsv, reader scripts/checks/lib/test_manifest.py, runner scripts/checks/compiler/tests.sh (accepted for landing).
- A-gate-spec-diff.diff: gate A, a seeded spec-vs-compiler differential. The spec leg generates slice-0 Core programs with spec `run` as oracle; a `compileCore` hook is still unwired because there is no Core->BIR lowering yet (slice-1 work). There is also an interim BIR leg: generated BIR, BIR run vs LLVM-native vs LCB-native vs LCB evaluator.
- C-gate-scaling-determinism.diff: gate C. A 1x/10x/100x scaling exponent test with a planted quadratic control, and a per-test determinism double-run. The scaling gate found ISS-0030: the LCB adapter is cubic and the LLVM printer quadratic. The fix is on its own branch.
- D-trusted-base.diff: gate D, extending gate 9. Concrete/Premises.lean becomes the trusted-component list; audits of library axioms, extern/unsafe/partial and strace'd pipeline tools.

Read the relevant files (Concrete/README.md: gates section; ConcreteSpec/README.md; ROADMAP.md; ISSUES.tsv; scripts/checks/; scripts/proofs/; .github/workflows/lean_action_ci.yml; required-checks.txt; AGENTS.md; docs/decisions.md if present) before judging. Do not modify files, do not build, no network.

The owner rules that matter: Lean is the only source of the spec; no duplication (one form per situation, one home per fact); every gate needs planted-failure controls that fail for the planted reason (no vacuity); prefer the best long-term structural design; CI landing tier must stay fast (heavy work runs on concrete-ci-2); never decide an owner language decision (OD) — flag it if a proposal needs one. Also: process tooling is otherwise frozen by the owner, so a proposal that adds gates needs a clear payoff.

For EACH item below answer on one block:
  ITEM <id>: AGREE | AGREE-WITH-CHANGES | DISAGREE
  why (2-4 lines, concrete, citing files);
  where it belongs: which existing gate/branch (A, B, C, D, gate 8, gate 9, a root check, ...) or a new gate, and which slice (now / slice 0 / slice 1 / later, with the dependency);
  size: SMALL (fits in the current A/C/D fix round, < ~150 lines) or LARGE (a ROADMAP row);
  exists already?: cite file:line if it (or part of it) already exists, or "no".

Group 1 — improvements to the four gates (the owner asked "can these be made better?"):
C1 Deterministic cost counters as the primary signal (Lean heartbeats/step counts, allocation counts, `perf stat` instructions where available); wall time secondary; noise-free exponents.
C2 One generator family per program dimension (blocks, nesting depth, functions, locals, expression length, literal size, struct/enum count, call-graph width), each scaled alone.
C3 A stage-complexity manifest: each stage declares O(n) or O(n log n); fit over >=4 sizes; fail when the exponent exceeds the declaration + margin; an undeclared stage fails.
C4 Scale peak memory (RSS) and output size, not only time.
C5 Absolute budgets and a history on concrete-ci-2 for reference workloads, so a regression shows even when the slope is unchanged.
C6 A planted quadratic control that must be caught (check whether C already has it and whether it is run automatically).
D1 Determinism perturbation runs: env, locale, TZ, cwd, path length, umask, input order, hash seeds, ASLR — byte-identical output.
D2 Compare every intermediate printer (the B0 U5/U11 printers), not only final artifacts, so the first differing stage is named.
D3 Cross-machine comparison GitHub runner vs concrete-ci-2 (ISS-0029, repro-ci2) kept aligned.
A1 A type-directed program generator with shrinking, in addition to fixtures.
A2 A spec-rule coverage metric: every spec rule exercised, the gate fails otherwise.
A3 Refusal agreement: the spec refuses iff the compiler refuses, with the same rule ID.
A4 Mutation testing of the compiler to show the comparison kills mutants (periodic, on concrete-ci-2).
A5 Roadmap note: the end state is a theorem (verified pipeline); the gate becomes a check that the proof covers the stage.
T1 Derive the trusted list instead of hand-writing it (Lean axioms via #print axioms over pinned theorems, extern/trusted decls, toolchain hashes); the committed list must equal the derived one; growth needs owner sign-off.
G1 A meta-gate reading a gate manifest: every gate declares its property, controls, cost and where it runs, and has >=1 planted control that must fail; a gate without one fails.
G2 A rerun disagreement is a failure, never a retry-pass.
G3 Track each gate's cost so the landing tier stays fast.

Group 2 — candidate new gates (the owner asked "any other gate we should add?"):
N1 Rule-ID refusal gate: every refusal names a spec rule ID, every spec rule ID has >=1 refusal fixture; orphans fail both ways.
N2 Mechanized vacuity gate: every spec law has a non-trivial instance and a planted counter-example.
N3 Decision<->spec traceability: every decided row cites its spec definition/fixture, every spec rule cites its decision row.
N4 Lexer/parser fuzzing: never crashes, always terminates, refuses with a rule ID, round-trips parse(print(ast)) = ast.
N5 Backend agreement: LLVM path and LCB path give identical observable behaviour on the whole manifest (maybe already in tests.sh; confirm).
N6 Trap-site accounting: every trap in emitted code maps to a source obligation and a trap name (no hidden traps).
N7 Output binary audit: symbols, sections and imports of emitted objects; an unexpected libc/runtime import fails.
N8 Toolchain/supply chain: pin and hash Lean, LLVM and the nix lock; a clean rebuild reproduces them.
N9 Documentation examples: every code example in docs/book compiles or refuses exactly as it says.
N10 Proof-check budget: kernel and elaboration time per theorem, history on concrete-ci-2, fail early on creep toward timeouts.
N11 Import layering: the compiler library may not import the spec-diff generators or test code (gate A's first draft put generators in Concrete/SpecDiff*.lean imported by Concrete.lean).
N12 Gate coverage: every pipeline stage is covered by at least one gate.

Then: up to 5 further gates or improvements YOU would add (same format, ids X1..X5), only if clearly worth it.

End with a ranked top-5 of all items by payoff/cost for the next two weeks, and the last line exactly: DONE.
