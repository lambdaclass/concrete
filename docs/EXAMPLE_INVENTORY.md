# Example Inventory

Status: canonical reference

This is the single source of truth for every named example in `examples/`. Each entry records what the example demonstrates, which phase owns it, what compiler claim it exercises, how correctness is checked, and its current promotion level.

If an example is not in this table, it does not exist as a named workload.

## Inventory

### Flagship examples (proof-backed, multi-gate, permanent regression targets)

| Name | Path | Phase | Claim exercised | Oracle strategy | Test gates |
|------|------|-------|-----------------|-----------------|------------|
| crypto_verify | `examples/crypto_verify/` | Phase 1 + Phase 2 | Proof-backed predictable code with fingerprint drift detection | Trust-drift consistency, proved/stale transitions, capability escalation detection | 16+ (policy, proof, stack-depth, drift) |
| elf_header | `examples/elf_header/` | Phase 1 + Phase 2 | Authority visible in signatures, Lean-backed proofs, trusted FFI boundary | End-to-end Lean attachment, trust-drift detection | 12+ (proof, drift, compilation) |
| proof_pressure | `examples/proof_pressure/` | Phase 2 (item 1) | One function per proof obligation state (proved, stale, missing, blocked, ineligible, trusted) | Reproducible extraction, deterministic fingerprints, Lean theorem stub generation | 34+ (proof-status, obligations, extraction, eligibility, fingerprints, lean-stubs, check-proofs, registry) |

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

## Multi-phase ownership

Some examples serve multiple phases. This is intentional and preferred over duplication (see `EXAMPLE_LIFECYCLE.md`).

| Example | Primary phase | Additional phases | Why shared |
|---------|--------------|-------------------|------------|
| crypto_verify | Phase 1 (predictable core) | Phase 2 (proof validation) | Proof registry + drift detection reuses the same trusted codebase |
| elf_header | Phase 1 (authority boundaries) | Phase 2 (Lean attachment) | FFI trust boundary is naturally the proof target |
| proof_pressure | Phase 2 (proof workflow) | Phase 2 (registry, extraction, Lean-stubs) | All 6 obligation states live in one program by design |
| thesis_demo | Phase 1 (three pillars) | Phase 2 (drift detection) | Drift demo reuses the thesis example rather than creating a separate one |

## Promotion log

| Date | Example | From | To | Reason |
|------|---------|------|----|--------|
| Phase 1 | thesis_demo | pressure | canonical | Three-pillar demonstration |
| Phase 1 | crypto_verify | canonical | flagship | Proof-backed + drift detection added |
| Phase 1 | elf_header | canonical | flagship | Lean attachment + drift detection added |
| Phase 2 item 1 | proof_pressure | new | flagship | Purpose-built for all 6 proof states |
| Phase 1 item 24 | fixed_capacity | new | canonical | Zero-alloc bounded predictable workload |
| Phase 1 item 29 | parse_validate | new | canonical | Error-flow for predictable subset |
| Phase 1 item 30 | service_errors | new | canonical | Service-style error propagation |

## Verification

Run `ls examples/*/Concrete.toml` to confirm all project-form examples have manifests. Run `./scripts/tests/run_tests.sh --trust-gate` to verify all trust-gate examples pass their gates.
