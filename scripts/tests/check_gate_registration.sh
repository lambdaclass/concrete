#!/usr/bin/env bash
# EVERY GATE IS REACHABLE FROM CI, OR EXPLICITLY SKIPPED WITH A REASON.
#
# This exists because eight R-0004 gates — the replay producer, receipt issuance and consumption,
# the V2 migration plan, clean-checkout reproducibility, build identity, the Slice 8 adversarial
# suite, the attestation manifest — were written, passed when run by hand, and were absent from
# `.github/workflows/lean_action_ci.yml`. That file is where `run_ci_gates_local.sh` derives its
# gate list, so nothing ever ran them again: not on the next commit, not for anyone else. A fully
# green local pass certified the entire evidence path while executing none of it.
#
# ABSENCE IS QUIETER THAN FAILURE, which is the whole problem. A failing gate is loud and gets
# fixed. A gate nobody runs looks exactly like a gate that passes, and the pass count keeps rising
# while coverage does not. `check_snapshots.sh` sat outside CI long enough to accumulate eleven real
# drifts, visible only as another gate's one-line "snapshot gate failed"; `check_v1_fingerprint_golden.sh`
# was invoked only by the convergence inventory, which discarded its stderr and read the silence as
# "inconclusive".
#
# REACHABILITY IS TRANSITIVE, and it is computed as a real closure rather than assumed to be one or
# two hops. A gate invoked by `run_fast_surface_gates.sh` IS run by CI, because that aggregator is
# itself a workflow step. Demanding a direct entry for each would be noise — about a dozen gates are
# aggregated that way and are perfectly well covered.
#
# The first version of this gate walked ONE level and asserted in its own comment that one level was
# enough for the aggregators this repository has. It was not: `check_roadmap_linear.sh` is reached as
# workflow -> run_fast_surface_gates.sh -> check_docs_drift.sh -> it, and the gate reported a covered
# file as unreachable. A fixpoint costs a few lines and cannot be wrong about depth.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
WORKFLOW=".github/workflows/lean_action_ci.yml"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -f "$WORKFLOW" ] || { echo "FATAL: $WORKFLOW missing — reachability cannot be determined" >&2; exit 2; }

# DECLARED SKIPS. Each needs a reason, and the reason is checked for existence rather than for
# content — a skip with an empty justification is the thing this list exists to prevent.
declare -A SKIP=(
  ["check_gate_registration.sh"]="this gate; it is the registration rule itself"
  ["check_rocksdb_slice1.sh"]="RocksDB slice 1 acceptance gate; acceptance is blocked by exits 0b, 2i and 2n (ROADMAP feature prerequisites) and it fails until gap G3 in docs/platform/ROCKSDB_SLICE1.md closes (G1 is closed); wire it into the full gate set, with librocksdb-dev on the runner, when the slice is accepted"
)

# Seed the reachable set with everything the workflow names directly, then repeatedly add any script
# invoked by something already reachable, until nothing new appears.
REACHABLE_FILE="$(mktemp)"; trap 'rm -f "$REACHABLE_FILE"; rm -rf "${STRIP_DIR:-}"' EXIT
# A MENTION IS NOT AN INVOCATION.
#
# Reachability was `grep -q "$b" "$file"`: the gate's NAME appearing anywhere counted, including in
# a comment. `check_classification_freshness.sh` was certified reachable solely because
# check_dependency_edges.sh contains the prose line "Table freshness lives in
# check_classification_freshness.sh, NOT here" — a sentence saying the gate runs somewhere else read
# as proof that it runs. Measured at 0df0d264: four gates had no non-comment reference anywhere,
# three of them were RED, and CI had never executed any while this gate reported full reachability.
#
# A reference counts only from a comment-stripped line carrying an execution context. The direction
# is deliberately CONSERVATIVE: a gate invoked in a form this cannot see is reported unreachable and
# must be registered or skipped explicitly, which demands clarity rather than granting a silent pass.
#
# A '#' is only a comment when it starts a word. Stripping every '#' destroyed the real invocation
# `nix develop .#provers --command bash ./scripts/tests/check_bv_certificates.sh`, whose flake
# selector contains one, and reported a registered gate as unreachable.
#
# ONE SCAN PER FILE, NOT ONE PER PAIR. Testing each (file, candidate) pair separately meant two
# processes for each of ~220x220 pairs per fixpoint round; the gate went from a second to being
# killed at five minutes. The adjacency is built once — each file scanned for every gate name in a
# single pass — and the fixpoint then walks it with no further subprocesses.
# scripts/tools/*.sh are containers too: checker_acceptance_evidence.sh is the only run of five gates
# (R-0546), so the edge from the workflow to those gates passes through it.
NAMES_FILE="$(mktemp)"; EDGES_FILE="$(mktemp)"
trap 'rm -f "$REACHABLE_FILE" "$NAMES_FILE" "$EDGES_FILE"' EXIT
# A check_*.py in scripts/tests is a gate too (bug 252: the Markdown-link checker was one, reached only
# from the Makefile, and this enumeration of shell files never saw it).
for f in scripts/tests/*.sh scripts/ci/*.sh scripts/gen/*.sh scripts/tools/*.sh scripts/tests/check_*.py; do
  [ -f "$f" ] || continue; basename "$f"
done | sort -u > "$NAMES_FILE"

_exec_lines() { # path -> comment-stripped lines that INVOKE something
  # Two forms count, because this repository dispatches gates both ways.
  #
  #   1. a literal interpreter/exec/source/./ invocation;
  #   2. DATA-DRIVEN DISPATCH: a gate named as a quoted argument, which is how the mutation
  #      campaign runs the gate each family declares — `add "fam" "File.lean" "check_foo.sh"` — and
  #      how test_mutation.sh does it via `gate_for_last "scripts/tests/check_foo.sh"`. CI runs the
  #      full campaign (workflow step "Gate mutation coverage ... heavy; rebuilds per mutation"), so
  #      those gates genuinely execute in CI. Ignoring this form reported three registered gates as
  #      unreachable — a false alarm is as damaging here as a false pass, because it trains the
  #      reader to ignore the gate.
  #
  # Comments are stripped first, so the prose that caused the original defect — "Table freshness
  # lives in check_classification_freshness.sh, NOT here" — cannot satisfy either form.
  sed -E 's/(^|[[:space:]])#.*$/\1/' "$1" 2>/dev/null \
    | grep -E '(^|[[:space:];&|(=])(bash|sh|zsh|exec|source|python3|python|\./)([[:space:]]|/|$)|["'"'"'][^"'"'"']*check_[A-Za-z0-9_]+\.sh["'"'"']'
}

# edges: "<container> <invoked-gate>", container "" meaning the workflow itself
_exec_lines "$WORKFLOW" | grep -oFf "$NAMES_FILE" 2>/dev/null | sort -u \
  | while IFS= read -r n; do [ -n "$n" ] && echo "WORKFLOW $n"; done > "$EDGES_FILE"
for f in scripts/tests/*.sh scripts/ci/*.sh scripts/gen/*.sh scripts/tools/*.sh; do
  [ -f "$f" ] || continue
  src="$(basename "$f")"
  _exec_lines "$f" | grep -oFf "$NAMES_FILE" 2>/dev/null | sort -u \
    | while IFS= read -r n; do
        [ -n "$n" ] && [ "$n" != "$src" ] && echo "$src $n"
      done >> "$EDGES_FILE"
done

awk '$1=="WORKFLOW"{print $2}' "$EDGES_FILE" | sort -u > "$REACHABLE_FILE"
while :; do
  before="$(wc -l < "$REACHABLE_FILE")"
  awk 'NR==FNR{r[$0]=1;next} ($1 in r){print $2}' "$REACHABLE_FILE" "$EDGES_FILE" \
    >> "$REACHABLE_FILE"
  sort -u "$REACHABLE_FILE" -o "$REACHABLE_FILE"
  [ "$(wc -l < "$REACHABLE_FILE")" = "$before" ] && break
done

reachable() { grep -qxF "$1" "$REACHABLE_FILE"; }

echo "=== every gate is reachable from CI or explicitly skipped ==="
TOTAL=0; UNREACHABLE=""
for f in scripts/tests/check_*.sh scripts/tests/check_*.py scripts/ci/*.sh; do
  [ -f "$f" ] || continue
  b="$(basename "$f")"
  TOTAL=$((TOTAL+1))
  if [ -n "${SKIP[$b]:-}" ]; then continue; fi
  reachable "$b" || UNREACHABLE="$UNREACHABLE $b"
done

# NON-VACUITY. If the enumeration found nothing, every assertion below is trivially satisfied and
# would read exactly like full coverage. The floor is deliberately far below the real count (211 at
# the time of writing) so it catches a broken glob rather than tracking the population.
if [ "$TOTAL" -ge 100 ]; then
  ok "enumerated $TOTAL gate scripts (floor 100 — a broken glob cannot pass as full coverage)"
else
  no "only $TOTAL gate scripts enumerated; the glob is broken and every check below is vacuous"
  echo "GATE-REGISTRATION: PASS=$PASS FAIL=$FAIL"; exit 1
fi

if [ -z "$UNREACHABLE" ]; then
  ok "every enumerated gate is reachable from $WORKFLOW, directly or through an aggregator"
else
  no "gate(s) unreachable from CI:$UNREACHABLE"
  echo "       A gate nobody runs is indistinguishable from one that does not exist. Add a workflow"
  echo "       step, or declare it in this gate's SKIP list with a reason."
fi

# THE SKIP LIST IS ALSO CHECKED, in both directions. A skip for a gate that no longer exists is
# stale and would silently excuse a future file of the same name; a skip with no reason is an
# exemption nobody has to justify.
for b in "${!SKIP[@]}"; do
  if [ ! -f "scripts/tests/$b" ] && [ ! -f "scripts/ci/$b" ]; then
    no "declared skip '$b' names a gate that does not exist — remove it before it excuses a future file"
  elif [ -z "${SKIP[$b]}" ]; then
    no "declared skip '$b' has no reason recorded"
  else
    ok "declared skip '$b' exists and states its reason"
  fi
done

# THE CONTROL. Without it, a `reachable()` that returned 0 unconditionally would satisfy everything
# above and report full coverage forever. A name that is certainly not in the workflow must come
# back unreachable.
if reachable "check_this_gate_does_not_exist_$$.sh"; then
  no "CONTROL FAILED: a non-existent gate reported as reachable — the check is inert and its pass proves nothing"
else
  ok "CONTROL: a gate absent from CI is correctly reported unreachable"
fi

echo "=== a CI gate the host cannot run is a named skip in run_ci_gates_local.sh, never a pass ==="
# The Examples job runs check_example_outputs.sh under EXAMPLE_VALGRIND=1; macOS has no valgrind.
# run_ci_gates_local.sh classifies such commands with scripts/tests/lib/host_tools.sh, lists them
# as SKIP, and keeps the run PARTIAL with completed=0 (host_tool_missing). These legs check the
# classifier on controlled PATHs, that the table matches the workflow, and that the runner wires it.
HT_LIB="scripts/tests/lib/host_tools.sh"
if ! source "$HT_LIB" 2>/dev/null || ! command -v host_tool_missing >/dev/null 2>&1; then
  no "$HT_LIB does not load — the runner would refuse every run"
else
  vg_cmd="EXAMPLE_VALGRIND=1 bash ./scripts/tests/check_example_outputs.sh"
  ht_empty="$(mktemp -d)"; ht_stub="$(mktemp -d)"
  printf '#!/bin/sh\nexit 0\n' > "$ht_stub/valgrind"; chmod +x "$ht_stub/valgrind"
  got="$(PATH="$ht_empty" host_tool_missing "$vg_cmd")"; rc=$?
  { [ "$rc" = 0 ] && [ "$got" = "valgrind" ]; } \
    && ok "CONTROL: without valgrind on PATH the valgrind run is classified host-skipped (valgrind)" \
    || no "CONTROL: without valgrind the valgrind run was not classified missing (rc=$rc, got '$got') — the runner would run it and report a misleading FAIL"
  PATH="$ht_stub" host_tool_missing "$vg_cmd" >/dev/null \
    && no "CONTROL: with valgrind on PATH the command was still skipped — CI-capable hosts would never run it" \
    || ok "CONTROL: with valgrind on PATH the valgrind run is runnable"
  PATH="$ht_empty" host_tool_missing "bash ./scripts/tests/check_example_outputs.sh" >/dev/null \
    && no "CONTROL: a command needing no host tool was classified missing" \
    || ok "CONTROL: the plain command needs no host tool"
  # elan is asked the way check_external_checker.sh asks: an executable $ELAN_HOME/bin/lake, on PATH
  # or not. The gate host keeps elan off its ssh PATH, and a PATH probe skipped a gate it could run.
  ec_cmd="bash scripts/tests/check_external_checker.sh"
  mkdir -p "$ht_stub/elan/bin"; printf '#!/bin/sh\nexit 0\n' > "$ht_stub/elan/bin/lake"; chmod +x "$ht_stub/elan/bin/lake"
  got="$(PATH="$ht_empty" ELAN_HOME="$ht_empty" host_tool_missing "$ec_cmd")"; rc=$?
  { [ "$rc" = 0 ] && [ "$got" = "elan" ]; } \
    && ok "CONTROL: without \$ELAN_HOME/bin/lake the external-checker gate is classified host-skipped (elan)" \
    || no "CONTROL: without \$ELAN_HOME/bin/lake the external-checker gate was not classified missing (rc=$rc, got '$got')"
  PATH="$ht_empty" ELAN_HOME="$ht_stub/elan" host_tool_missing "$ec_cmd" >/dev/null \
    && no "CONTROL: an elan lake at \$ELAN_HOME (not on PATH) still skipped the external-checker gate" \
    || ok "CONTROL: an elan lake at \$ELAN_HOME runs the external-checker gate, PATH notwithstanding"
  rm -rf "$ht_empty" "$ht_stub"
  # The table must match the workflow: every row names something CI actually runs (a dead row is a
  # stale exemption), and the valgrind spelling CI uses is covered.
  plan="$(bash scripts/tests/run_ci_gates_local.sh --plan 2>/dev/null)"
  for row in "${HOST_TOOL_TABLE[@]}"; do
    pat="${row%%|*}"
    case "$plan" in
      *"$pat"*) ok "host-tool row '${row}' matches a workflow command" ;;
      *) no "host-tool row '${row}' matches no workflow command — remove it or fix the spelling" ;;
    esac
  done
  if grep -q 'EXAMPLE_VALGRIND=1' <<<"$plan"; then
    vg_plan="$(grep 'EXAMPLE_VALGRIND=1' <<<"$plan" | head -1)"
    [ "$(host_tool_required "$vg_plan")" = "valgrind" ] \
      && ok "the workflow's valgrind command requires valgrind per the table" \
      || no "the workflow's valgrind command ($vg_plan) is not covered by $HT_LIB"
  else
    no "no EXAMPLE_VALGRIND=1 command in the workflow plan — update $HT_LIB and these legs together"
  fi
  # The runner must use the classifier and let the skip block completion (comments stripped, so a
  # mention in prose does not count).
  runner_code="$(sed -E 's/(^|[[:space:]])#.*$/\1/' scripts/tests/run_ci_gates_local.sh)"
  case "$runner_code" in *'lib/host_tools.sh'*host_tool_missing*) ok "run_ci_gates_local.sh classifies commands with the shared host-tool table" ;;
    *) no "run_ci_gates_local.sh does not use $HT_LIB" ;; esac
  case "$runner_code" in *'ALL_REFUSALS="$REFUSALS$SCOPE_NOTES$MODE_NOTES$HOST_NOTES"'*) ok "a host skip is part of the completion-blocking notes (PARTIAL, completed=0)" ;;
    *) no "HOST_NOTES is not in ALL_REFUSALS — a host-skipped run could be recorded completed=1" ;; esac

  echo "=== a verdict deferred to CI is declared, judged on CI, and never a pass (bug 315) ==="
  # host_tools.sh's HOST_DEFERRED_TABLE names the gates whose exit GATE_DEFERRED_RC the runner records
  # as deferred. Each must be a gate CI runs unconditionally (else nothing judges it), with a known
  # judged_on, and must itself use that exit status.
  if [ "${#HOST_DEFERRED_TABLE[@]}" -eq 0 ] || ! command -v gate_deferral >/dev/null 2>&1; then
    no "$HT_LIB declares no deferral table or no gate_deferral — the runner could not accept a deferral"
  else
    for row in "${HOST_DEFERRED_TABLE[@]}"; do
      name="${row%%|*}"; on="${row#*|}"
      case "$on" in ci-counted) ok "deferral row '$row' has a known judged_on ($on)" ;;
        *) no "deferral row '$row': judged_on '$on' is not one this file defines" ;; esac
      if why="$(gate_deferral "bash scripts/tests/$name")"; then
        ok "deferral row '$row': CI runs $name as an unconditional step, so the deferred verdict is made there"
      else
        no "deferral row '$row': $why"
      fi
    done
    # The scaling gate's own exit for "not judged here" is the runner's deferral status.
    grep -qx "NOT_JUDGED_RC = $GATE_DEFERRED_RC" scripts/tests/lib/compile_scaling.py \
      && ok "compile_scaling.py exits $GATE_DEFERRED_RC (GATE_DEFERRED_RC) when it did not judge" \
      || no "compile_scaling.py's NOT_JUDGED_RC is not GATE_DEFERRED_RC ($GATE_DEFERRED_RC)"
    # CONTROLS: an undeclared gate, and a declared gate whose CI step is conditional, are refused.
    gate_deferral "bash scripts/tests/check_this_gate_does_not_exist_$$.sh" >/dev/null \
      && no "CONTROL: gate_deferral accepted an undeclared gate — every exit 77 would read as deferred" \
      || ok "CONTROL: gate_deferral refuses an undeclared gate"
    df_gate="scripts/tests/${HOST_DEFERRED_TABLE[0]%%|*}"
    gate_deferral "bash ./$df_gate" >/dev/null && gate_deferral "bash $df_gate --shape x" >/dev/null \
      && ok "gate_deferral accepts the declared gate as ./path and with plain arguments" \
      || no "gate_deferral refused a plain invocation of the declared gate"
    for df_cmd in "bash scripts/tests/check_gate_lock.sh $df_gate" "bash $df_gate; bash scripts/tests/check_x.sh" \
                  "bash $df_gate || true" "bash $df_gate | tee x" "bash $df_gate \$(echo x)" \
                  "X=x;(exit\${IFS}77);# bash $df_gate" $'true\nbash '"$df_gate" "X=\$(false) bash $df_gate" \
                  "BASH_ENV=/tmp/defer bash $df_gate" "ENV=/tmp/defer sh $df_gate" "FOO=1 bash $df_gate"; do
      gate_deferral "$df_cmd" >/dev/null \
        && no "CONTROL: gate_deferral accepted '$df_cmd', which is not the declared gate's own exit" \
        || ok "CONTROL: gate_deferral refuses '$df_cmd' (the gate as an argument, or in a compound command)"
    done
    df_wf="$(mktemp)"
    printf 'jobs:\n  j:\n    runs-on: x\n    steps:\n      - if: false\n        run: bash scripts/tests/%s\n' \
      "${HOST_DEFERRED_TABLE[0]%%|*}" > "$df_wf"
    gate_deferral "bash scripts/tests/${HOST_DEFERRED_TABLE[0]%%|*}" "$df_wf" >/dev/null \
      && no "CONTROL: a declared gate whose only CI step is conditional was accepted as deferred" \
      || ok "CONTROL: a declared gate whose CI step is conditional cannot defer (nothing would judge it)"
    rm -f "$df_wf"
  fi
  case "$runner_code" in *gate_deferral*GATE_DEFERRED_RC*gate_deferral*) ok "run_ci_gates_local.sh classifies exit GATE_DEFERRED_RC with gate_deferral (sequential and pool)" ;;
    *) no "run_ci_gates_local.sh does not classify GATE_DEFERRED_RC with gate_deferral in both paths" ;; esac
  case "$runner_code" in *'HOST_NOTES="$HOST_NOTES$DEFER_NOTES"'*) ok "a deferral is part of the completion-blocking notes (PARTIAL, completed=0)" ;;
    *) no "DEFER_NOTES is not in HOST_NOTES — a deferred run could be recorded completed=1" ;; esac
  case "$runner_code" in *'EXECUTED=$(( PASS + FAIL + DEFERRED ))'*) ok "a deferral counts as executed, not passed" ;;
    *) no "EXECUTED does not count deferrals apart from passes" ;; esac
fi

echo "GATE-REGISTRATION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
