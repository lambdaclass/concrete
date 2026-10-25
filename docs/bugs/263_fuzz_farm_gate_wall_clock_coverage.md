# Bug 263 — fuzz-farm gate assumes timed campaigns complete fixed coverage

<!-- bug-ledger
summary: fuzz-farm gate timed campaigns could expire before the required generator coverage and drift observations; bounded batch targets now make coverage deterministic and incomplete campaigns fail.
coverage: tooling-only defect pinned by scripts/tests/check_fuzz_farm.sh and scripts/tests/check_fuzz_farm_batches.py, including virtual-clock speed, partial-batch, pause and default-mode controls; no compiler fixture applies.
silent: no
-->

**Status:** FIXED on `fix/fuzz-farm-deterministic-gate` (pending landing).
**Found:** 2026-10-25, while landing the R-0507 proof-library research roadmap update.
**Severity:** loud validation failure; no Concrete compiler defect or new soundness claim.

## Reproducer

At main `525ea19e`, run `nix develop -c bash scripts/tests/check_fuzz_farm.sh`.
On the ARM64 laptop the standalone run reported `PASS=49 FAIL=7`: missing repeated crashes,
no linearity finding, incomplete generator coverage, missing notifications and a drift alert
that did not clear. The pooled run failed more assertions. Classification controls passed.
This gate uses fake compilers and generators; native/interpreter compiler reproduction does
not apply. Expected: all planted behaviors are exercised independently of machine speed.

## Cause

The gate requested 8 seconds of a rotating three-generator campaign but asserted coverage
across all three generators. Classification, process startup and minimization consume that
budget. The 2-second drift-reset campaign also assumed it would complete at least 20 cases,
the minimum for clearing a previous alert. An expired campaign returned success even though
the test's required work had not completed. Later notification and status failures cascaded
from missing findings. The 3-second recheck campaigns have the same cause.

## Fix

Add opt-in `campaign --batches N`: stop after N complete batches; `--seconds` remains a
watchdog, and expiry before N complete batches returns 2 with an incomplete diagnostic.
The ordinary timed production campaign keeps its existing behavior. The gate requests fixed
batches for all five campaigns and checks every campaign exit status. Its 180-second watchdog
does not establish coverage. Partial batches interrupted by the deadline or pause do not count.

Regression coverage: `scripts/tests/check_fuzz_farm_batches.py`, invoked by
`scripts/tests/check_fuzz_farm.sh`, compares identical generated work at different virtual
speeds, checks partial/zero-time/paused runs cannot pass, and preserves default timed behavior.
The original end-to-end fake-compiler assertions still require deduplication, minimization,
notifications, rechecking and drift recovery.

## Structural cause

Test coverage was inferred from elapsed time instead of a completed-work contract. This is a
test-orchestration defect outside the compiler structural classes. The other spellings in this
gate are its initial discovery campaign, both recheck campaigns, and drift raise/reset campaigns;
all now use the same completion contract. Load limits alone cannot establish coverage.

## Structural owner

R-0543 (validation under load), implemented at the fuzz-farm campaign driver and its owning gate.
Separate the workload required for a test verdict from its timeout; an exhausted watchdog is
incomplete, never evidence that required cases ran.
