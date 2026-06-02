# No-Duplicate-Example Rule

Status: canonical reference

## Rule

When one example serves multiple phases, reuse it with explicit multi-phase ownership instead of creating near-duplicate programs that fragment validation effort.

## Motivation

Duplicate examples cause three problems:

1. **Fragmented validation**: test gates split across two programs that exercise the same claim, making it unclear which one is authoritative.
2. **Maintenance drift**: the two copies evolve independently, one rots, and the project loses confidence in which version represents current behavior.
3. **Inventory bloat**: the example set grows without growing coverage, making it harder to identify what is actually tested versus what is decorative.

## When to reuse

Reuse an existing example when:

- The new phase needs the same language features (error enums, match, structs, capabilities) that the existing example already exercises
- The new test gates are additive (e.g., adding drift detection to an example that already has predictable checks)
- The example's code does not need to change to serve the new phase

**Example**: `crypto_verify` started as a Phase 1 predictable-core example. Phase 2 added proof registration and drift detection. No new example was created — the same program gained a `proof-registry.json` and a `main_drifted.con` variant, and new test gates were added to the existing test section.

## When to create a new example

Create a new example when:

- The claim being tested is fundamentally different (e.g., error propagation vs. proof obligation states)
- The existing example would need substantial restructuring to serve the new purpose
- The language features needed (e.g., capabilities, FFI, allocation) conflict with the existing example's profile (e.g., adding File cap to a predictable=true example)

**Example**: `parse_validate` (item 29) and `service_errors` (item 30) are both predictable error-flow examples, but they exercise different patterns: single-function validation pipeline vs. multi-stage service handler with 3 separate error enums. The structural difference justifies separate examples.

## Multi-phase ownership

When an example serves multiple phases, record this in `docs/EXAMPLE_INVENTORY.md`:

| Example | Primary phase | Additional phases | Why shared |
|---------|--------------|-------------------|------------|
| crypto_verify | Phase 1 | Phase 2 | Same codebase gains proof registry |
| elf_header | Phase 1 | Phase 2 | FFI boundary is naturally the proof target |

Each phase that uses a shared example must have its own test gates in `run_tests.sh`. The primary phase owns the example's structure; additional phases add test assertions without modifying the source.

## Detecting violations

Before creating a new example, check:

1. Does an existing example already exercise the same compiler feature?
2. Could the new claim be tested by adding gates to an existing example's test section?
3. Would the new example's source code overlap >50% with an existing example?

If yes to any of these, prefer reuse. Document the multi-phase ownership in the inventory.

## Current multi-phase examples

- **crypto_verify**: Phase 1 (policy enforcement, predictable checks) + Phase 2 (proof validation, drift detection)
- **elf_header**: Phase 1 (authority boundaries, FFI) + Phase 2 (Lean attachment, drift detection)
- **proof_pressure**: Phase 2 (proof workflow) + Phase 2 (registry integrity, extraction, Lean-stubs) — single phase but multiple concerns
- **thesis_demo**: Phase 1 (three pillars) + Phase 2 (drift detection)

## Current near-duplicates to watch

- `parse_validate` and `service_errors` both demonstrate predictable error propagation. They are distinct enough (single-stage vs. multi-stage) to justify separate examples. If a third predictable error example is proposed, it should be blocked unless it exercises a genuinely new pattern.
- `packet` and `fixed_capacity` both demonstrate predictable bounded parsing. They are distinct (binary protocol vs. message processor). A third bounded-parsing example should reuse one of these.
- `integrity` and `verify` both demonstrate SHA-256 artifact checking. If either is promoted to canonical, the other should be evaluated for merge.
