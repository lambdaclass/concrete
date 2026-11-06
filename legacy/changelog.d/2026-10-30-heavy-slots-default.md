### Every suite mode and identity sweep takes a machine-wide heavy slot (2026-10-30)

Only `run_tests.sh --full` took one of the two heavy slots (R-0543), so agents running the default
mode in parallel pushed a 32-thread host past load 30 and wall-clock budget probes failed for no
code reason. Every suite mode now takes a slot (the listing and a single stdlib module stay light;
in CI, where the runner count bounds concurrency, only `--full` does), `--slot-probe` reports the
decision for the gate, and `scripts/dev/identity_sweep.sh` (emit and report byte-identity of two
binaries, the evidence a behaviour-preserving refactor owes) takes a slot too.
