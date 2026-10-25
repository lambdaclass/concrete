### Two Heavy Runs Per Machine, A Shared Compile Cache, And A Measured Gate Profile (R-0543, R-0546, 2026-10-22)

Heavy local runs are limited to two at a time. `run_ci_gates_local.sh` (full or larger
selections), `run_tests.sh --full`, `test_mutation.sh` and the mutation campaign take one of
`CONCRETE_HEAVY_SLOTS` (default 2) slots, shared by every worktree and clone of one user (under
`XDG_RUNTIME_DIR`, or `~/.cache`); different users do not share the limit; a waiter prints
the holders, a dead holder's slot is reclaimed, and `--no-wait` exits 75 instead of waiting.
`check_heavy_slots.sh` (CI) holds the controls. The project's
[CLAUDE.md](CLAUDE.md) states the rule for agents.

[GATE_PROFILE.md](docs/project/GATE_PROFILE.md) records where CI time went at named SHAs: the
critical path, the 25 slowest gates and why the top ten are slow. Its fixes:

- `scripts/tests/lib/compile_cache.sh` caches allowlisted pure reports, keyed by the compiler
  binary's hash, the build identity, the argv and working directory, the input's project closure,
  std, `proofs/` and the tools; anything the key cannot cover runs uncached, and a result is
  published only if no covered file changed during the compile. Measured warm: `check_vc_schema`
  356 s to 26 s, the report views 259 s to 94 s, the Phase 3 capstone 774 s to 348 s.
  `check_compile_cache.sh` (CI) holds the controls.
- New worktrees seed `.lake` from another checkout, and `lake build` re-verifies the seed.
- Gates that perturb or populate the checkout now run in a private checkout
  (`scripts/tests/lib/private_checkout.sh`), umbrella gates delegate to the gates they used to
  re-run, independent sub-gates and fuzz cases run concurrently, and the local runner orders its
  pool longest-first from `scripts/tests/gate_weights.tsv`. The full local set stopped scaling at
  about 32 jobs, where the longest gate bounds it.

`run_fast_surface_gates.sh` now keeps each constituent's output and prints a failing
constituent's FAIL lines, exit code and log (2026-10-23).
