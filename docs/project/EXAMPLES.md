# Examples

Status: canonical reference. One document for the named examples in `examples/`: the inventory
(what each example demonstrates, which phase owns it, what compiler claim it exercises, how its
correctness is checked, and its promotion level), the four promotion levels and the path between
them, and the no-duplicate rule. It replaces, as of 2026-10-29, the separate
`EXAMPLE_INVENTORY.md`, `EXAMPLE_LIFECYCLE.md` and `EXAMPLE_NO_DUPLICATES.md`.

How an example is classified for the gates (`scripts/tests/example_manifest.txt`, `@run` rules and
golden outputs) is in [STYLE.md](STYLE.md#examples) and [GOLDEN_ORACLES.md](GOLDEN_ORACLES.md).

Contents:

1. [Inventory](#inventory)
2. [Lifecycle and promotion](#lifecycle-and-promotion)
3. [No duplicate examples](#no-duplicate-examples)

## Inventory

This inventory is the single source of truth for every named example in `examples/`. Each entry records what the example demonstrates, which phase owns it, what compiler claim it exercises, how correctness is checked, and its current promotion level.

If an example is not in this table, it does not exist as a named workload.

### Flagship examples (proof-backed, multi-gate, permanent regression targets)

| Name | Path | Phase | Claim exercised | Oracle strategy | Test gates |
|------|------|-------|-----------------|-----------------|------------|
| crypto_verify | `examples/crypto_verify/` | Phase 1 + Phase 2 | Proof-backed predictable code with fingerprint drift detection | Trust-drift consistency, proved/stale transitions, capability escalation detection | 16+ (policy, proof, stack-depth, drift) |
| elf_header | `examples/elf_header/` | Phase 1 + Phase 2 | Authority visible in signatures, Lean-backed proofs, trusted FFI boundary | End-to-end Lean attachment, trust-drift detection | 12+ (proof, drift, compilation) |
| proof_pressure | `examples/proof_pressure/` | Phase 2 (item 1) | Original pressure set for proved, stale, missing, blocked, ineligible, and trusted; the later `unbound` state is exercised by the evidence-class and contract-negative fixtures | Reproducible extraction, deterministic fingerprints, Lean theorem stub generation | 34+ (proof-status, obligations, extraction, eligibility, fingerprints, lean-stubs, check-proofs, registry) |

### Canonical examples (phase centerpieces, trust-gate tested)

| Name | Path | Phase | Claim exercised | Oracle strategy | Test gates |
|------|------|-------|-----------------|-----------------|------------|
| fixed_capacity | `examples/fixed_capacity/` | Phase 1 (item 24) | Zero allocation, bounded loops, stack-only, proof-eligible core | Stack-depth reporting, predictable profile validation, effects report | 12+ (predictable, bounded loops, zero alloc, stack-depth) |
| parse_validate | `examples/parse_validate/` | Phase 1 (item 29) | Explicit Result-style error propagation, 6 error categories, pure functions | Predictable profile validation, error code consistency | 10 (build, run, predictable, effects, trusted, alloc, enums, bounded loop, policy, purity) |
| service_errors | `examples/service_errors/` | Phase 1 (item 30) | 4-stage service pipeline, 3 stage-specific error enums, deterministic error codes | Predictable profile validation, stage-specific error enum consistency | 10 (build, run, predictable, effects, trusted, alloc, error code fns, pipeline handlers, policy, purity) |
| thesis_demo | `examples/thesis_demo/` | Phase 1 (item 1) | Three pillars: capability-visible separation, predictable execution, formal proof | Proof status validation, effects reporting, drift detection | 8+ (proof, effects, predictable, drift) |
| packet | `examples/packet/` | Phase 1 | Binary protocol decoder, pure parsing core, trusted FFI boundary | Predictable profile checks, effects reporting | 4 (predictable, effects) |

### Pressure examples (stdlib/feature discovery, compile-only or manual)

These examples exist to discover missing language features, stdlib gaps, and ergonomic friction. They are not trust-gate tested. They may not compile cleanly at any given time.

| Name | Path | What it pressures | Predictable | Status |
|------|------|-------------------|-------------|--------|
| grep | `examples/grep/` | String/text processing, argument parsing, line I/O | No (File cap) | Compiles |
| http | `examples/http/` | Network I/O, HTTP parsing, capability separation | No (Network, File) | Compiles |
| integrity | `examples/integrity/` | SHA-256, file I/O, manifest validation | No (File cap) | Compiles |
| json | `examples/json/` | Recursive-descent parsing, collections, string handling | No (Alloc) | Pressure only |
| kvstore | `examples/kvstore/` | Persistence, HashMap, crash recovery, line parsing | No (File cap) | Compiles |
| lox | `examples/lox/` | Tree-walk interpreter, pool allocation, deep control flow | No | Most complex (48 functions) |
| mal | `examples/mal/` | Make-A-Lisp interpreter, pure eval core, effectful shell | No (Alloc) | Pressure only |
| policy_engine | `examples/policy_engine/` | Capability-aware policy gatekeeper, pure evaluation | Partially | Pressure only |
| toml | `examples/toml/` | TOML parser, string pool, capability discipline | No (Alloc) | Compiles |
| verify | `examples/verify/` | SHA-256 artifact integrity, capability boundaries | No | Compiles |

### Supporting

| Name | Path | Purpose |
|------|------|---------|
| project | `examples/project/` | Minimal multi-module project structure stub |
| snippets | `examples/snippets/` | 60+ small educational code snippets, feature demonstrations |

### Multi-phase ownership

Some examples serve multiple phases. This is intentional and preferred over duplication (see [No duplicate examples](#no-duplicate-examples)).

| Example | Primary phase | Additional phases | Why shared |
|---------|--------------|-------------------|------------|
| crypto_verify | Phase 1 (predictable core) | Phase 2 (proof validation) | Proof registry + drift detection reuses the same trusted codebase |
| elf_header | Phase 1 (authority boundaries) | Phase 2 (Lean attachment) | FFI trust boundary is naturally the proof target |
| proof_pressure | Phase 2 (proof workflow) | Phase 2 (registry, extraction, Lean-stubs) | All 6 obligation states live in one program by design |
| thesis_demo | Phase 1 (three pillars) | Phase 2 (drift detection) | Drift demo reuses the thesis example rather than creating a separate one |

### Promotion log

| Date | Example | From | To | Reason |
|------|---------|------|----|--------|
| Phase 1 | thesis_demo | pressure | canonical | Three-pillar demonstration |
| Phase 1 | crypto_verify | canonical | flagship | Proof-backed + drift detection added |
| Phase 1 | elf_header | canonical | flagship | Lean attachment + drift detection added |
| Phase 2 item 1 | proof_pressure | new | flagship | Purpose-built for all 6 proof states |
| Phase 1 item 24 | fixed_capacity | new | canonical | Zero-alloc bounded predictable workload |
| Phase 1 item 29 | parse_validate | new | canonical | Error-flow for predictable subset |
| Phase 1 item 30 | service_errors | new | canonical | Service-style error propagation |

### Verification

Run `ls examples/*/Concrete.toml` to confirm all project-form examples have manifests. Run `./scripts/tests/run_tests.sh --trust-gate` to verify all trust-gate examples pass their gates.

## Lifecycle and promotion

This section defines four promotion levels for examples, the bar required to reach each level, and the promotion path between them. The goal is to prevent unnamed workloads from accumulating without clear purpose or validation.

### Promotion levels

#### 1. Pressure example

**Bar**: compiles (or documents why it does not yet compile).

**Purpose**: discover missing language features, stdlib gaps, and ergonomic friction. A pressure example exists to break the compiler, not to demonstrate that things work.

**Requirements**:
- Lives in `examples/<name>/`
- Has a `Concrete.toml` (or a comment explaining why not, e.g., pre-project-form)
- Documents what it pressures in a top-of-file comment
- Does NOT need to pass tests, predictable checks, or effects validation

**Examples today**: grep, http, integrity, json, kvstore, lox, mal, policy_engine, toml, verify

**Maintenance obligation**: it must keep building and producing its recorded output. Until 2026-10-20 pressure examples were allowed to rot (json, mal, vm and policy_engine had stopped compiling behind a `skip`); now every example is compiled by `scripts/tests/check_examples.sh` and every runnable one has `@run` rules checked by `scripts/tests/check_example_outputs.sh`. When the language moves, port the example in the same change, or record the compiler bug that blocks it as `known-bug NNN` in the manifest.

#### 2. Canonical example

**Bar**: compiles, runs correctly, trust-gate tested.

**Purpose**: demonstrate a specific phase claim with compiler-observable evidence. A canonical example is the primary workload for one roadmap item. It should be the first place someone looks to understand what that item delivered.

**Requirements**:
- Everything from pressure, plus:
- Runs and returns 0 (all self-tests pass)
- Has at least one trust-gate test section in `run_tests.sh`
- Tests check at least: build, run, and one compiler-observable property (predictable, effects, stack-depth, or proof status)
- Listed in the [inventory](#inventory) with owning phase, claim, and oracle strategy
- Top-of-file comment describes what it demonstrates and which profile/policy it targets

**Promotion from pressure**: when the example is adopted as the centerpiece of a roadmap item, add trust-gate tests and update the inventory.

**Examples today**: fixed_capacity, parse_validate, service_errors, thesis_demo, packet

#### 3. Flagship example

**Bar**: canonical + proof-backed or multi-phase + extensive test gates.

**Purpose**: demonstrate the full Concrete thesis with proof evidence, drift detection, or cross-phase validation. Flagship examples are the strongest evidence that the compiler delivers on its claims.

**Requirements**:
- Everything from canonical, plus:
- Has at least one source-linked proved function, OR serves 2+ phases with independent test gates in each
- Has drift detection (drifted variant + diff-based consistency tests) if proof-backed
- 10+ trust-gate assertions
- Listed in the inventory with multi-phase ownership if applicable

**Promotion from canonical**: when proof registration, drift detection, or multi-phase test gates are added.

**Examples today**: crypto_verify, elf_header, proof_pressure

#### 4. Permanent regression target

**Bar**: flagship + no removal without explicit roadmap decision.

**Purpose**: an example that is permanently load-bearing for CI and cannot be removed or reorganized without a roadmap item tracking the migration.

**Requirements**:
- Everything from flagship, plus:
- Used in 3+ independent test sections
- Removal requires a roadmap item explaining where its test coverage migrates

**Examples today**: crypto_verify, elf_header, proof_pressure (all three are permanent regression targets by virtue of their test coverage breadth)

### Promotion path

```
pressure  →  canonical  →  flagship  →  permanent regression target
   ↑              ↑             ↑
   new          roadmap       proof/
  example       item          drift
  created       adopts it     added
```

**Demotion**: an example can be demoted if its tests are removed and no roadmap item replaces them. Document the demotion in the inventory promotion log.

### Anti-patterns

1. **Unnamed workload**: an example exists in `examples/` but is not in the inventory. Every example must be listed.
2. **Test-free canonical**: an example is called "canonical" but has no trust-gate tests. Either add tests or demote to pressure.
3. **Duplicate workload**: two examples demonstrate the same claim at the same level. See [No duplicate examples](#no-duplicate-examples).
4. **Phantom flagship**: an example is called "flagship" but has no proof registry or drift detection. Either add evidence or demote to canonical.

### Adding a new example

1. Create `examples/<name>/` with `Concrete.toml` and `src/main.con`
2. Add a top-of-file comment explaining purpose and target profile
3. Add to the [inventory](#inventory) at the pressure level
4. If the example is a roadmap item deliverable, add trust-gate tests and promote to canonical
5. Log the promotion in the inventory promotion log

## No duplicate examples

### Rule

When one example serves multiple phases, reuse it with explicit multi-phase ownership instead of creating near-duplicate programs that fragment validation effort.

### Motivation

Duplicate examples cause three problems:

1. **Fragmented validation**: test gates split across two programs that exercise the same claim, making it unclear which one is authoritative.
2. **Maintenance drift**: the two copies evolve independently, one rots, and the project loses confidence in which version represents current behavior.
3. **Inventory bloat**: the example set grows without growing coverage, making it harder to identify what is actually tested versus what is decorative.

### When to reuse

Reuse an existing example when:

- The new phase needs the same language features (error enums, match, structs, capabilities) that the existing example already exercises
- The new test gates are additive (e.g., adding drift detection to an example that already has predictable checks)
- The example's code does not need to change to serve the new phase

**Example**: `crypto_verify` started as a Phase 1 predictable-core example. Phase 2 added proof registration and drift detection. No new example was created — the same program gained in-source proof links and a `main_drifted.con` variant, and new test gates were added to the existing test section.

### When to create a new example

Create a new example when:

- The claim being tested is fundamentally different (e.g., error propagation vs. proof obligation states)
- The existing example would need substantial restructuring to serve the new purpose
- The language features needed (e.g., capabilities, FFI, allocation) conflict with the existing example's profile (e.g., adding File cap to a predictable=true example)

**Example**: `parse_validate` (item 29) and `service_errors` (item 30) are both predictable error-flow examples, but they exercise different patterns: single-function validation pipeline vs. multi-stage service handler with 3 separate error enums. The structural difference justifies separate examples.

### Recording multi-phase ownership

When an example serves multiple phases, record it in the inventory's
[multi-phase ownership](#multi-phase-ownership) table (primary phase, additional phases, why shared).

Each phase that uses a shared example must have its own test gates in `run_tests.sh`. The primary phase owns the example's structure; additional phases add test assertions without modifying the source.

### Detecting violations

Before creating a new example, check:

1. Does an existing example already exercise the same compiler feature?
2. Could the new claim be tested by adding gates to an existing example's test section?
3. Would the new example's source code overlap >50% with an existing example?

If yes to any of these, prefer reuse. Document the multi-phase ownership in the inventory.

### Current multi-phase examples

- **crypto_verify**: Phase 1 (policy enforcement, predictable checks) + Phase 2 (proof validation, drift detection)
- **elf_header**: Phase 1 (authority boundaries, FFI) + Phase 2 (Lean attachment, drift detection)
- **proof_pressure**: Phase 2 (proof workflow) + Phase 2 (registry integrity, extraction, Lean-stubs) — single phase but multiple concerns
- **thesis_demo**: Phase 1 (three pillars) + Phase 2 (drift detection)

### Current near-duplicates to watch

- `parse_validate` and `service_errors` both demonstrate predictable error propagation. They are distinct enough (single-stage vs. multi-stage) to justify separate examples. If a third predictable error example is proposed, it should be blocked unless it exercises a genuinely new pattern.
- `packet` and `fixed_capacity` both demonstrate predictable bounded parsing. They are distinct (binary protocol vs. message processor). A third bounded-parsing example should reuse one of these.
- `integrity` and `verify` both demonstrate SHA-256 artifact checking. If either is promoted to canonical, the other should be evaluated for merge.
