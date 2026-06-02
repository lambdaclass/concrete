# Example Lifecycle and Promotion Policy

Status: canonical reference

This document defines four promotion levels for examples, the bar required to reach each level, and the promotion path between them. The goal is to prevent unnamed workloads from accumulating without clear purpose or validation.

## Promotion levels

### 1. Pressure example

**Bar**: compiles (or documents why it does not yet compile).

**Purpose**: discover missing language features, stdlib gaps, and ergonomic friction. A pressure example exists to break the compiler, not to demonstrate that things work.

**Requirements**:
- Lives in `examples/<name>/`
- Has a `Concrete.toml` (or a comment explaining why not, e.g., pre-project-form)
- Documents what it pressures in a top-of-file comment
- Does NOT need to pass tests, predictable checks, or effects validation

**Examples today**: grep, http, integrity, json, kvstore, lox, mal, policy_engine, toml, verify

**Maintenance obligation**: none. A pressure example may rot if the language moves. That is acceptable — the gap it reveals is the point.

### 2. Canonical example

**Bar**: compiles, runs correctly, trust-gate tested.

**Purpose**: demonstrate a specific phase claim with compiler-observable evidence. A canonical example is the primary workload for one roadmap item. It should be the first place someone looks to understand what that item delivered.

**Requirements**:
- Everything from pressure, plus:
- Runs and returns 0 (all self-tests pass)
- Has at least one trust-gate test section in `run_tests.sh`
- Tests check at least: build, run, and one compiler-observable property (predictable, effects, stack-depth, or proof status)
- Listed in `docs/EXAMPLE_INVENTORY.md` with owning phase, claim, and oracle strategy
- Top-of-file comment describes what it demonstrates and which profile/policy it targets

**Promotion from pressure**: when the example is adopted as the centerpiece of a roadmap item, add trust-gate tests and update the inventory.

**Examples today**: fixed_capacity, parse_validate, service_errors, thesis_demo, packet

### 3. Flagship example

**Bar**: canonical + proof-backed or multi-phase + extensive test gates.

**Purpose**: demonstrate the full Concrete thesis with proof evidence, drift detection, or cross-phase validation. Flagship examples are the strongest evidence that the compiler delivers on its claims.

**Requirements**:
- Everything from canonical, plus:
- Has a `proof-registry.json` with at least one proved function, OR serves 2+ phases with independent test gates in each
- Has drift detection (drifted variant + diff-based consistency tests) if proof-backed
- 10+ trust-gate assertions
- Listed in the inventory with multi-phase ownership if applicable

**Promotion from canonical**: when proof registration, drift detection, or multi-phase test gates are added.

**Examples today**: crypto_verify, elf_header, proof_pressure

### 4. Permanent regression target

**Bar**: flagship + no removal without explicit roadmap decision.

**Purpose**: an example that is permanently load-bearing for CI and cannot be removed or reorganized without a roadmap item tracking the migration.

**Requirements**:
- Everything from flagship, plus:
- Used in 3+ independent test sections
- Removal requires a roadmap item explaining where its test coverage migrates

**Examples today**: crypto_verify, elf_header, proof_pressure (all three are permanent regression targets by virtue of their test coverage breadth)

## Promotion path

```
pressure  →  canonical  →  flagship  →  permanent regression target
   ↑              ↑             ↑
   new          roadmap       proof/
  example       item          drift
  created       adopts it     added
```

**Demotion**: an example can be demoted if its tests are removed and no roadmap item replaces them. Document the demotion in the inventory promotion log.

## Anti-patterns

1. **Unnamed workload**: an example exists in `examples/` but is not in the inventory. Every example must be listed.
2. **Test-free canonical**: an example is called "canonical" but has no trust-gate tests. Either add tests or demote to pressure.
3. **Duplicate workload**: two examples demonstrate the same claim at the same level. See `EXAMPLE_NO_DUPLICATES.md`.
4. **Phantom flagship**: an example is called "flagship" but has no proof registry or drift detection. Either add evidence or demote to canonical.

## Adding a new example

1. Create `examples/<name>/` with `Concrete.toml` and `src/main.con`
2. Add a top-of-file comment explaining purpose and target profile
3. Add to `docs/EXAMPLE_INVENTORY.md` at the pressure level
4. If the example is a roadmap item deliverable, add trust-gate tests and promote to canonical
5. Log the promotion in the inventory promotion log
