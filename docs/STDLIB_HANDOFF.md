# Stdlib Handoff Contract (Phase 6 → Phase 7)

Status: handoff contract — ROADMAP Phase 6 #19.

Phases 5–6 decide the *language surfaces* the standard library depends on; the
actual library APIs are built in the dedicated stdlib phase (Phase 7). This
document is the contract between the two: for each required surface it records a
status and the gate/doc that backs that status, so "the stdlib can rely on X" is
a checked claim, not an aspiration.

## Status vocabulary

- **`stable_for_stdlib`** — the surface is implemented and locked by a CI gate.
  Phase 7 may build on it directly; a breaking change must update both the gate
  and this contract.
- **`provisional_with_gate`** — the surface works and is gated, but some part of
  its design is deliberately deferred (tracked by a ROADMAP item). Phase 7 may
  use it within the documented bounds; the deferred part is named so stdlib APIs
  don't assume it.
- **`blocked`** — the surface is not usable yet. **Phase 7 may not start while
  any required surface is `blocked`** — this is enforced by
  `scripts/tests/check_stdlib_handoff.sh`.

## Required surfaces

The `backing` column names a file that must exist (a CI gate `check_*.sh`, or a
design doc for a deferred part). The handoff gate parses this table.

| surface | status | backing |
| --- | --- | --- |
| modules-imports | stable_for_stdlib | scripts/tests/check_module_visibility.sh |
| project-model | stable_for_stdlib | scripts/tests/check_project_model.sh |
| tests | stable_for_stdlib | scripts/tests/check_concrete_test.sh |
| diagnostics | stable_for_stdlib | scripts/tests/check_diagnostics_quality.sh |
| bytes-text-path | stable_for_stdlib | scripts/tests/check_byte_view.sh |
| collections | stable_for_stdlib | scripts/tests/check_collections.sh |
| callable-values | stable_for_stdlib | scripts/tests/check_callable_values.sh |
| capability-callbacks | stable_for_stdlib | scripts/tests/check_capability_polymorphism_design.sh |
| cli-verbs | stable_for_stdlib | scripts/tests/check_cli_contract.sh |
| const-generics | provisional_with_gate | docs/CONST_GENERICS_V1.md |
| iteration | provisional_with_gate | scripts/tests/check_loop_control.sh |
| build-profiles | provisional_with_gate | docs/PROFILES.md |

## Notes on the provisional surfaces

- **const-generics** — narrow const generics for fixed-capacity APIs are
  *designed and workload-gated* (see [CONST_GENERICS_V1.md](CONST_GENERICS_V1.md)).
  Fixed-capacity stdlib APIs may assume the documented V1 subset only; anything
  beyond it must wait for a real workload to pull it in (ROADMAP #6a).
- **iteration** — `for` loops, `for_each`/`fold`/`map` with
  capability-polymorphic callbacks, and byte/parse cursors already work and are
  gated by [check_loop_control.sh] + [check_pattern_ergonomics.sh]. The *formal
  iteration protocol* (the official story, authority/allocation visibility, the
  no-closure/no-trait-object stance) is ROADMAP #17 and not yet written; stdlib
  APIs should use the existing callback/cursor mechanics, not assume a trait-based
  `Iterator`.
- **build-profiles** — debug/release profile behavior and the policy surface are
  designed ([PROFILES.md](PROFILES.md)); the remaining work is checked-overflow
  codegen (ROADMAP #10). Until that lands, stdlib APIs must not depend on
  checked-overflow *trapping* semantics — overflow is the documented opt-in
  arithmetic policy, not a guarantee.

## Phase 7 readiness

No required surface is `blocked`, so Phase 7 stdlib work is **not gate-blocked**.
The three `provisional_with_gate` surfaces (const-generics, iteration,
build-profiles) bound what early stdlib APIs may assume; their deferred parts are
tracked by ROADMAP #6a / #17 / #10 respectively. When those close, promote the
corresponding row to `stable_for_stdlib` here and in the gate.
