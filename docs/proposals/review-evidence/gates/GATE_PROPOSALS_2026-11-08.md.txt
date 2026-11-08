# Gate proposals approved by the owner, 2026-11-08

Owner approval, verbatim:
- "ok add all the gates but we should run some of them in every pr and others every day and others every week?"
- "ok let's add the tiers when you get back"

The proposals were sent to the gates worker for one Codex/Kimi/agy round. The round decides how each gate is built and which tier it runs in, not whether it is added.

## Improvements to existing gates

### C, scaling and determinism
- C1. Deterministic cost counters as the primary signal: Lean heartbeats or steps, allocations, perf instructions. Wall time is secondary.
- C2. One generator family per program dimension: blocks, depth, functions, locals, expression length, literal size, types, call-graph width.
- C3. A stage-complexity manifest. Each stage declares O(n) or O(n log n). The gate fits over at least 4 sizes and fails when the exponent exceeds the declared one plus a margin. A stage with no declaration fails.
- C4. Scale peak RSS and output size too.
- C5. Absolute budgets plus a history on concrete-ci-2 for the reference workloads.
- C6. A planted quadratic control that the gate must catch.
- D1. Perturbation runs (env, locale, TZ, cwd, path length, umask, input order, hash seeds, ASLR) must produce byte-identical output.
- D2. Compare every intermediate printer, so the first differing stage is named.
- D3. Compare a GitHub runner against concrete-ci-2 (ISS-0029).

### A, spec vs compiler
- A1. A type-directed random program generator, with shrinking.
- A2. A spec-rule coverage metric. The gate fails if a rule is not exercised.
- A3. Refusal agreement: the spec refuses if and only if the compiler refuses, with the same rule ID.
- A4. Mutation testing of the compiler, to show that the comparison kills mutants.
- A5. End state: a theorem (the verified pipeline) replaces the gate.

### Trusted list
- T1. Derived rather than handwritten, from Lean axioms, extern and trusted declarations, and toolchain hashes. Any growth needs an owner sign-off.

### All gates
- G1. A meta-gate over the gate manifest. Every gate declares its property, controls, cost, location and tier, and has at least one planted control that fails.
- G2. A disagreement between reruns is a failure, never a retry-pass.
- G3. Track each gate's cost.

## New gates 1-12
1. Rule-ID refusal: every refusal names a spec rule, and every rule has a refusal fixture.
2. Mechanized vacuity: every law has an instance and a planted counter-example.
3. Decision-to-spec traceability, in both directions.
4. Lexer and parser fuzzing: no crash, termination, refusal with a rule ID, and the round-trip parse(print(ast)) = ast.
5. Backend agreement: LLVM and LCB agree on the whole manifest.
6. Trap-site accounting: every emitted trap maps to an obligation and a trap name.
7. Output binary audit: symbols, sections and imports.
8. Toolchain and supply chain: pinned and hashed Lean, LLVM and the nix lock, reproduced by a clean rebuild.
9. Documentation examples compile or refuse exactly as stated.
10. A proof-check time budget per theorem, with history.
11. Import layering: compiler code must not import test or spec-diff code.
12. Gate coverage: every pipeline stage is covered by at least one gate.

## Proposed tiers
PROPOSED, and open until the gates worker's round reports the final assignment.

### PR tier
Required. About 10 minutes added in total.
- Test manifest and backend agreement.
- Spec-diff on the fixtures, with refusal agreement and rule coverage.
- Rule IDs, vacuity, traceability, import layering, gate coverage and the meta-gate.
- The derived trusted list and the axioms property.
- Trap accounting, the binary audit and the docs examples.
- Determinism on two runs, with printers compared at every stage.
- The GitHub-vs-concrete-ci-2 repro leg.
- Small-size counter scaling.
- A hard cap on proof time.

### Daily tier, on concrete-ci-2
- Full scaling: all dimensions, large sizes, memory and output size, with history.
- The determinism perturbation matrix.
- Fuzzing, and the random spec-diff generator, about one hour each.
- A clean toolchain rebuild with hashes.
- Proof-time history.

### Weekly tier, on concrete-ci-2
- A compiler mutation campaign.
- Gate mutation.
- An overnight fuzz run.
- A from-scratch rebuild including nix.
- A supply-chain audit.
- A trend review.

## Rules
- Every gate declares its tier, and the meta-gate enforces this along with the PR-tier budget.
- A daily or weekly failure is a stop-the-line bug against main, at the exact SHA, with a reproducer. It never blocks an unrelated landing, and it is never ignored.
- Every bug a scheduled run finds adds a regression fixture to the PR tier.
- A skipped or cancelled scheduled run counts as missing, never as passed.

## Test types T-a to T-j
Owner question, 2026-11-08: "any more type of tests we should add early on?"
APPROVED by the owner ("ok let's add all this"). The round assigns tiers and build details only.
- T-a metamorphic: rename, reorder, dead code, reformat.
- T-b exhaustive arithmetic over 8-bit operands, with boundary values at wider widths.
- T-c a validator after every pass.
- T-d valgrind on emitted binaries.
- T-e C layout and ABI comparison with clang.
- T-f golden diagnostics.
- T-g theorem-statement pinning.
- T-h limit tests.
- T-i termination and bounded-execution refusals.
- T-j a regression fixture for every bug.
