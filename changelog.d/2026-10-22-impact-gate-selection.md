### The Pre-Push Hook Selects Gates By Impact (R-0513, 2026-10-22)

The pre-push hook had picked gates by a hand-kept map from paths to gate names, and on 2026-10-22
four failures it did not select reached CI (the stack-budget gate, the trust gate's bug-audit
corpus, the float round-trip judge and the linear-discard gate), each costing a CI round of about
1.5 h. `scripts/tests/select_gates.sh` now derives, from the gate scripts themselves, every workflow
gate command a diff can break: what each gate reads, transitively through the scripts it runs;
every compiler- or Lean-running gate for a compiler, toolchain or std change; and every gate for a
change no rule maps. It is the hook's default; the area map remains the fallback when the selector
fails, or on request (`CONCRETE_PREPUSH_SELECT=area`). `run_ci_gates_local.sh --impact` runs the
selection, and the landing script calls it over an exact `--base`/`--head` range.
`check_gate_selection.sh` (CI) replays the four misses as fixed controls. Bug 237 (map notes read as
calls) is its first recorded defect.
