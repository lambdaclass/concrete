#!/usr/bin/env bash
# log_reports_incomplete LOG: true when a gate log reports an INCOMPLETE result (a command that timed
# out twice, or whose runner reported no outcome; scripts/tests/lib/bounded.sh). Such a run is red
# without being a verdict, so a mutation driver must not read it as a kill (ROADMAP "Promotion
# pipeline", result vocabulary: incomplete is neither failed nor passed).
#
# The forms, as the gates print them: a per-test line "  INCOMPLETE <label>" (check_example_outputs,
# check_callable_values, check_hashmap_probe_invariants, check_model_diff, run_tests' projects,
# test_ssa's "INCOMPLETE <test>"), "INCOMPLETE: ..." (the watchdog form and bounded_gate_status), and
# run_tests' summary "  incomplete: N" with N > 0. A zero count ("INCOMPLETE=0", "INCOMPLETE: 0",
# "0 incomplete", "incomplete=0") does not match.
log_reports_incomplete() {
  grep -qE '^[[:space:]]*INCOMPLETE[[:space:]]|^INCOMPLETE: |^[[:space:]]*incomplete: [1-9]' "$1" 2>/dev/null
}
