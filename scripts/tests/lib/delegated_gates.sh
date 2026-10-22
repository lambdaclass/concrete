#!/usr/bin/env bash
# DELEGATED SUB-GATES: an umbrella states, instead of re-running, that each gate it covers runs as its
# own unconditional CI step (R-0547).
#
# Four umbrella gates (check_phase1_contracts, check_phase2_vc, check_phase3_obligation_core and the
# nested check_snapshots/test_prove_cli runs in the obligation view gates) re-ran gates that CI already
# runs as separate steps in the same workflow run, and the local full set extracts those same steps.
# Every such assertion therefore ran two or three times per run: about 12 of phase 3's 13 CI minutes,
# and most of phase 2's 8. The umbrella keeps its own legs; for a delegated gate it now asserts the
# one thing the re-run added that the standalone step does not already establish — that the gate IS
# a standalone step, so dropping or disabling that step turns the umbrella red instead of silently
# removing the coverage. The gate's own assertions then run exactly once: in its step on CI, and as
# its extracted command in run_ci_gates_local.sh.
#
# "Runs as its own unconditional CI step" means: some step's one-line `run:` IS the invocation of
# `scripts/tests/<name>` (optional VAR= prefixes, bash/python3/sh or ./, plain arguments; no pipeline,
# list or substitution), the step has no `if:` other than exactly `always()` and no
# `continue-on-error`, and its job has no job-level `if:` (the scheduled-only jobs have one). The
# workflow is read as text with the indentation conventions run_ci_gates_local.sh's extractor uses.

_DELEGATED_WF="${DELEGATED_GATES_WORKFLOW:-.github/workflows/lean_action_ci.yml}"

# delegated_gate_step <gate-file-name> [workflow] -> prints "job:line" of each qualifying step; 0 if any
delegated_gate_step() {
  local name="$1" wf="${2:-$_DELEGATED_WF}"
  [ -f "$wf" ] || return 2
  awk -v want="scripts/tests/$name" '
    function flush() {
      if (step_has && !step_bad && !job_if) { print job ":" step_line; found = 1 }
      step_has = 0; step_bad = 0; step_line = 0
    }
    /^  [A-Za-z0-9_-]+:[[:space:]]*$/ { flush(); job = $1; sub(/:$/, "", job); job_if = 0; next }
    /^    if:/ { job_if = 1; next }
    /^      - / { flush() }
    # A STEP KEY, whether it is the first key (on the "- " line) or a later one.
    /^      - [a-z-]+:/ || /^        [a-z-]+:/ {
      kv = $0; sub(/^      - /, "", kv); sub(/^        /, "", kv)
      key = kv; sub(/:.*$/, "", key)
      val = kv; sub(/^[a-z-]+:[[:space:]]*/, "", val); sub(/[[:space:]]+$/, "", val)
      # A condition other than exactly always() may skip the step; continue-on-error turns its failure
      # into a pass. Either one disqualifies the step.
      if (key == "if" && val != "always()" && val != "${{ always() }}") step_bad = 1
      if (key == "continue-on-error" && val != "false") step_bad = 1
      # THE GATE MUST BE THE WHOLE COMMAND OF A ONE-LINE run: optional VAR=value prefixes,
      # bash/python3/sh or ./, the path, then plain arguments. A block run: (where the gate could sit
      # inside an `if`), a pipeline, a list (`|| true`, `;`, `&&`) or a substitution does not count.
      if (key == "run") {
        cmd = val
        while (match(cmd, /^[A-Z_][A-Z0-9_]*=[^[:space:]]+[[:space:]]+/)) cmd = substr(cmd, RLENGTH + 1)
        sub(/^(bash|python3|sh)[[:space:]]+/, "", cmd); sub(/^\.\//, "", cmd)
        if (index(cmd, want) == 1) {
          rest = substr(cmd, length(want) + 1)
          if ((rest == "" || rest ~ /^[[:space:]]/) && rest !~ /[|;&<>`$(]/) { step_has = 1; step_line = NR }
        }
      }
    }
    END { flush(); exit(found ? 0 : 1) }
  ' "$wf"
}

# require_delegated <ok-fn> <no-fn> <gate-file-name>...
#   Reports one assertion per delegated gate through the caller's ok/no functions.
require_delegated() {
  local okf="$1" nof="$2" g where; shift 2
  for g in "$@"; do
    if where="$(delegated_gate_step "$g")"; then
      "$okf" "$g runs as its own CI step ($(printf '%s' "$where" | head -1 | tr '\n' ' ')) — not re-run here"
    else
      "$nof" "$g is no longer an unconditional CI step of its own; this umbrella delegated it, so its assertions would now run nowhere — restore the step or run it here"
    fi
  done
}

# CONTROL: a fixture workflow where one gate is a plain step, one is in a scheduled-only job, one has a
# disabling step condition and one appears only in a comment. Callers run this once so an inert
# matcher cannot pass every delegation.
delegated_gates_control() {
  local okf="$1" nof="$2" t
  t="$(mktemp)"
  cat > "$t" <<'WF'
jobs:
  plain:
    runs-on: ubuntu-latest
    steps:
      - name: a
        run: bash scripts/tests/check_alpha.sh
      - name: b
        if: always()
        run: EXAMPLE_FLAG=1 bash ./scripts/tests/check_beta.sh --flag
      - name: k
        run: |
          echo hi
          bash ./scripts/tests/check_kappa.sh
      - name: l
        run: bash scripts/tests/check_lambda.sh || true
      - if: false
        run: bash scripts/tests/check_mu.sh
      - continue-on-error: true
        run: bash scripts/tests/check_nu.sh
      - name: |
          bash scripts/tests/check_xi.sh
        run: echo skipped
      - name: c
        if: github.event_name == 'schedule'
        run: bash scripts/tests/check_gamma.sh
      # bash scripts/tests/check_delta.sh
      - name: f
        if: always() && false
        run: bash scripts/tests/check_zeta.sh
      - name: g
        run: echo scripts/tests/check_eta.sh
      - name: h
        run: |
          if false; then bash scripts/tests/check_theta.sh; fi
      - name: i
        continue-on-error: true
        run: bash scripts/tests/check_iota.sh
  nightly:
    if: github.event_name == 'schedule'
    runs-on: ubuntu-latest
    steps:
      - name: e
        run: bash scripts/tests/check_epsilon.sh
WF
  local got=""
  for g in check_alpha.sh check_beta.sh check_gamma.sh check_delta.sh check_epsilon.sh check_alp.sh \
           check_zeta.sh check_eta.sh check_theta.sh check_iota.sh check_kappa.sh check_lambda.sh \
           check_mu.sh check_nu.sh check_xi.sh; do
    delegated_gate_step "$g" "$t" >/dev/null && got="$got ${g%.sh}"
  done
  rm -f "$t"
  if [ "$got" = " check_alpha check_beta" ]; then
    "$okf" "CONTROL: the delegation matcher accepts plain and always() one-line steps and refuses scheduled, conditional (either key position), continue-on-error, block-run, || true, echoed, shell-guarded, name-only, commented and prefix-only matches"
  else
    "$nof" "CONTROL: the delegation matcher accepted:${got:- nothing} (want: check_alpha check_beta)"
  fi
}
