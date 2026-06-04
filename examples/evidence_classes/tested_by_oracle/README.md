# tested_by_oracle  — PLANNED

**Class:** evidence from a differential oracle — the compiled program agrees
with an independent reference implementation across a vector set. This is a
`tested` (not `proved`) evidence class: it raises confidence, it does not
kernel-verify.

**Status: planned.** Concrete has an oracle harness (`reference.py` +
`run_oracle.sh` per flagship, and the `tests/oracle` differential gate), but it
is wired per-flagship, not as a tiny standalone corpus entry. Building this means
a minimal `src/main.con` with a reference `oracle/reference.py` and a vector set,
plus a per-example oracle runner. Deferred rather than half-wired.
