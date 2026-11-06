#!/usr/bin/env bash
# CI TRIAGE, FLAKE HISTORY AND COVERAGE DIFF (scripts/ci/triage.sh, flake_history.py,
# coverage_diff.py), against fixture logs and a stub `gh`.
#
# The fixtures under scripts/tests/fixtures/ci_triage/ are trimmed job logs: the real ones from
# 2026-10-22 (the bug 065 stack-budget leg, the trust gate's duplicate audit-corpus key, mutation
# shards red on the clean tree, a cancelled campaign, a lean-action compile failure) and shaped
# ones for the rest of the classification table (the float judge reading an old symbol name,
# check_linear_discard, lean4export not building under a leaked GIT_DIR, apt and runner loss,
# bounded-run INCOMPLETE markers, job timeouts, ledger flakes and quarantines).
#
# What is pinned: each case's class, the rerun-once rule (infra and known flakes rerun exactly once,
# never on attempt 2, never with a real failure in the run, never for a run cancelled as a whole),
# the report's contents, that an infra defect is never rerun and never absorbed by the ledger, the
# quarantine semantics (reported separately, never a pass, real after expiry), the ledger checks,
# flake candidates (fail-then-pass on one SHA; infra and infra defects excluded), and the coverage
# diff's regression verdict. No network: `gh` is a stub that serves the fixtures and logs calls.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIX="$ROOT_DIR/scripts/tests/fixtures/ci_triage"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
python3 -I -c 'import yaml' 2>/dev/null || { echo "error: PyYAML is required (python3-yaml)"; exit 2; }
[ -d "$FIX/runs" ] && [ -d "$FIX/logs" ] || { echo "error: fixtures missing under $FIX"; exit 2; }

# ---------------------------------------------------------------- the stub gh
mkdir -p "$TMP/bin" "$TMP/fx/runs" "$TMP/fx/logs" "$TMP/fx/api"
cat > "$TMP/bin/gh" <<'STUB'
#!/usr/bin/env bash
# Serves $GH_FX (overlay) then $GH_FIX; logs every call to $GH_CALLS.
printf '%s\n' "$*" >> "$GH_CALLS"
find_fx() { for d in "$GH_FX" "$GH_FIX"; do [ -f "$d/$1" ] && { cat "$d/$1"; return 0; }; done; return 1; }
case "$1 $2" in
  "run view")   find_fx "runs/$3.json" || { echo "HTTP 404: run $3 not found" >&2; exit 1; } ;;
  "run rerun")  echo "requested rerun of $3" ;;
  "run list")   case "$*" in *--event*) find_fx "runlist_dispatch.json" || echo "[]" ;;
                                   *) find_fx "runlist.json" || echo "[]" ;; esac ;;
  "run download") # gh run download <run> -D <dir> -p <pattern> [-R repo]: copy matching artifact dirs
                 rid="$3"; shift 3; dir=""; pat="*"
                 while [ $# -gt 0 ]; do case "$1" in -D) dir="$2"; shift 2;; -p) pat="$2"; shift 2;; *) shift;; esac; done
                 n=0; mkdir -p "$dir"
                 for a in "$GH_FX/artifacts/$rid"/$pat; do [ -d "$a" ] && cp -R "$a" "$dir/" && n=$((n+1)); done
                 [ "$n" -gt 0 ] || { echo "no artifact matches $pat" >&2; exit 1; } ;;
  "repo view")  echo "o/r" ;;
  api*)
    p="$2"
    case "$p" in
      */actions/jobs/*/logs) j="${p%/logs}"; j="${j##*/}"
                             if find_fx "logs/$j.err" >&2; then exit 1; fi
                             find_fx "logs/$j.txt" || { echo "HTTP 404: Not Found" >&2; exit 1; } ;;
      */compare/*)           find_fx "compare.json" || { echo "HTTP 404" >&2; exit 1; } ;;
      *)                     k="$(printf '%s' "$p" | sed 's#^repos/[^/]*/[^/]*/actions/##; s#[/?=&]#_#g')"
                             find_fx "api/$k.json" || { echo "HTTP 404: $p" >&2; exit 1; } ;;
    esac ;;
  *) echo "stub gh: unsupported: $*" >&2; exit 1 ;;
esac
STUB
chmod +x "$TMP/bin/gh"
export PATH="$TMP/bin:$PATH" GH_FIX="$FIX" GH_FX="$TMP/fx" GH_CALLS="$TMP/gh.calls"
export CI_TRIAGE_TODAY=2026-10-22
: > "$GH_CALLS"
cat > "$TMP/fx/compare.json" <<'JSON'
{"commits":[{"sha":"26c409f8aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","commit":{"message":"bug 065 stack-budget leg: an extern spelled like an intrinsic is refused\n\nbody"}}],
 "files":[{"filename":"scripts/tests/check_stack_budget.sh"},{"filename":"Concrete/Check/Check.lean"}]}
JSON
printf '[{"databaseId": 1, "headSha": "0000000000000000000000000000000000000001", "createdAt": "2026-10-21T00:00:00Z"}]\n' > "$TMP/fx/runlist.json"

# A test ledger: a known flake with a signature, an active quarantine, and a row that tries to
# absorb every failure of the external checker (it must not absorb the infra defect).
LEDGER="$TMP/ledger.tsv"
printf '%s\n' "gate	first_seen	count	owner	status	expires	match	note" \
  "check_tcpserve.sh	2026-10-01	3	alice	known-flaky	-	port [0-9]+ in use	ephemeral port race" \
  "check_workload_differential.sh	2026-10-02	2	bob	quarantined	2026-12-31	curl timing	timing window" \
  "check_external_checker.sh	2026-10-03	1	carol	known-flaky	-	*	network clones" > "$LEDGER"

TRIAGE=(bash "$ROOT_DIR/scripts/ci/triage.sh")
# triage <run> [args...] -> sets OUT, RC, RERUNS (rerun calls this invocation)
triage() {
  local run="$1"; shift
  local before; before=$(grep -c '^run rerun' "$GH_CALLS" || true)
  OUT="$("${TRIAGE[@]}" "$run" --repo o/r --out-dir "$TMP/out" --ledger "$LEDGER" "$@" 2>&1)"; RC=$?
  RERUNS=$(( $(grep -c '^run rerun' "$GH_CALLS" || true) - before ))
}
# expect <run> <class> <gate> <rc> <reruns> <label>
expect() {
  triage "$1"
  if grep -E "^  $2 +[a-z]+ " <<<"$OUT" | has_match -F " :: $3 -- " && [ "$RC" = "$4" ] && [ "$RERUNS" = "$5" ]; then
    ok "$6: $2, exit $4, $5 rerun(s)"
  else
    no "$6: want class $2 for $3, exit $4, $5 rerun(s); got exit $RC, $RERUNS rerun(s):"
    sed 's/^/        /' <<<"$OUT" | awk 'NR<=12'
  fi
}

echo "=== the classification table ==="
expect 39748808077 real check_stack_budget.sh 1 0 "stack-budget leg (bug 065, run 39748808077)"
expect 39760689389 real "run_tests.sh --trust-gate" 1 0 "trust gate duplicate audit-corpus key (run 39760689389)"
expect 9000000003 real check_concrete_fmt.sh 1 0 "float judge grepping the old symbol name (bug 129)"
expect 9000000004 real check_linear_discard.sh 1 0 "check_linear_discard"
expect 9000000005 infra-defect check_external_checker.sh 1 0 "lean4export does not build (GIT_DIR leak)"
expect 9000000009 real check_model_diff.sh 1 0 "INCOMPLETE marker beside a failing assertion"
expect 9000000111 real check_tcpserve.sh 1 0 "a ledger gate failing with another signature"
expect 39802535363 real "Install Lean (via lean-action)" 1 0 "lean-action 'lake build failed' (compile error)"
expect 9000000015 real proof_gate.sh 1 0 "job timeout without an INCOMPLETE marker"
expect 39717336317 real check_gate_mutation_coverage.sh 1 0 "mutation shard red on the clean tree"
expect 9000000077 real "Run examples" 1 0 "no job log (404) but a step concluded failure"
expect 9000000019 real aggregate_mutation_shards.py 1 0 "aggregate refusal no failed shard explains"
expect 9000000020 real check_linear_discard.sh 1 0 "tolerated install blip, then a real gate failure"
expect 9000000021 real check_model_diff.sh 1 0 "INCOMPLETE beside a nonzero FAIL= tally"
expect 9000000024 infra aggregate_mutation_shards.py 0 1 "families in no shard that the cancelled shard reported missing"
expect 9000000022 real aggregate_mutation_shards.py 1 0 "families in no shard while the failed shard's summary was usable"
expect 9000000023 real check_workload_differential.sh 1 0 "same command twice: network exit, then assertion + timeout"
expect 9000000017 real check_linear_discard.sh 1 0 "a real failure beside an infra one (no rerun)"
expect 9000000006 infra "Install clang" 0 1 "apt mirror unreachable"
expect 9000000007 infra "Run examples" 0 1 "runner lost (no job log, no step concluded)"
expect 9000000008 infra check_model_diff.sh 0 1 "bounded run INCOMPLETE, no assertion failed"
expect 9000000115 infra proof_gate.sh 0 1 "job timeout after an INCOMPLETE marker"
expect 9000000011 known-flaky check_tcpserve.sh 0 1 "known flake matching its ledger signature"
expect 9000000012 known-flaky check_workload_differential.sh 0 1 "quarantined gate (active)"
expect 39799334352 infra check_gate_mutation_coverage.sh 0 0 "run cancelled as a whole (no rerun)"

echo "=== details of the real examples ==="
triage 39748808077
grep -qE '^  infra +cancelled +Gate mutation shard 3' <<<"$OUT" && ok "the cancelled shard beside it is infra" \
  || no "cancelled shard not classified infra"
R="$TMP/out/39748808077.md"
if [ -f "$R" ] \
   && grep -q 'FAIL extern_intrinsic_name: expected unbounded naming main.wrap, main.main' "$R" \
   && grep -q "lake build && bash scripts/tests/check_stack_budget.sh'" "$R" \
   && grep -q 'git checkout 8c8db85a0000000000000000000000000000000a' "$R" \
   && grep -q '`26c409f8aaaa` bug 065 stack-budget leg' "$R" \
   && grep -q 'changes the failing gate itself: `scripts/tests/check_stack_budget.sh`' "$R"; then
  ok "report: FAIL lines, reproduction, suspected commits, the gate's own change"
else
  no "stack-budget report incomplete"; [ -f "$R" ] && sed 's/^/        /' "$R" | awk 'NR<=40'
fi
grep -q 'last green run on ci/symbols: 1' "$R" 2>/dev/null && ok "suspected range starts at the last green run on the branch" \
  || no "suspected range base not the last green run"
triage 39760689389
grep -q 'a corpus key is assigned twice: 102 103' "$TMP/out/39760689389.md" 2>/dev/null \
  && grep -q "bash scripts/tests/run_tests.sh --trust-gate'" "$TMP/out/39760689389.md" \
  && ok "trust-gate report names the duplicate key and reproduces without the tee" \
  || no "trust-gate report"
grep -q 'the compiler reads 0 of 1124 float lexemes' "$TMP/out/9000000003.md" 2>/dev/null \
  && ok "float-judge report carries the judge's FAIL line" || no "float-judge report"
grep -q 'never rerun\|pinned external tool did not build' "$TMP/out/9000000005.md" 2>/dev/null \
  && ok "lean4export report says it is a tooling defect" || no "lean4export report"
grep -q 'the ledger' <<<"$(triage 9000000005; echo "$OUT")" && no "infra defect matched the ledger" \
  || ok "the ledger's catch-all external-checker row does not absorb the infra defect"
triage 39717336317
grep -qE '^  real +failed +Gate mutation coverage \(aggregate verdict\) :: aggregate_mutation_shards.py -- derived: every refusal names one of 2' <<<"$OUT" \
  && ok "the mutation aggregate is classified by its shards" || no "aggregate not derived from shards"
grep -q 'atomic-flip-entrance-refuses-pending: INVALID' "$TMP/out/39717336317.md" 2>/dev/null \
  && ok "shard report carries the INVALID family line" || no "shard report"

triage 39799334352
grep -qE '^  infra +cancelled +Gate mutation coverage \(aggregate verdict\) :: aggregate_mutation_shards.py -- derived' <<<"$OUT" \
  && ok "a cancelled campaign's aggregate (every refusal a cancelled shard's) is infra" || no "cancelled aggregate not derived"
triage 9000000078
[ "$RC" = 2 ] && [ "$RERUNS" = 0 ] && grep -q 'rate limit' <<<"$OUT" \
  && ok "an unreadable log (rate limit) is an input error, never evidence" || no "unreadable log (rc $RC)"

echo "=== rerun once, never twice ==="
triage 9000000006
[ "$RERUNS" = 0 ] && grep -q 'already requested' <<<"$OUT" && ok "a second triage of the same attempt does not rerun again (marker)" \
  || no "second triage reran again ($RERUNS)"
triage 9000000013
[ "$RC" = 1 ] && [ "$RERUNS" = 0 ] && grep -q 'escalated' "$TMP/out/9000000013.md" 2>/dev/null \
  && ok "attempt 2 never reruns: the infra failure is escalated to a report" || no "attempt 2 handling (rc $RC, reruns $RERUNS)"
rm -rf "$TMP/out"
triage 9000000006 --no-rerun
[ "$RC" = 0 ] && [ "$RERUNS" = 0 ] && ok "--no-rerun classifies without rerunning" || no "--no-rerun reran"
grep -q "^run rerun 9000000007 --failed -R o/r$" "$GH_CALLS" && ok "the rerun is 'gh run rerun <id> --failed' on the run's repository" \
  || no "rerun command shape"

echo "=== quarantine ==="
rm -rf "$TMP/out"
triage 9000000012
grep -q 'quarantined (reported separately; still a failure, never a pass)' <<<"$OUT" \
  && grep -q 'check_workload_differential.sh: failed (quarantined until 2026-12-31, owner bob)' <<<"$OUT" \
  && ok "a quarantined failure is reported separately as failed" || no "quarantine section"
CI_TRIAGE_TODAY=2027-01-15 triage 9000000112
grep -qE '^  real .*quarantine of check_workload_differential.sh expired 2026-12-31' <<<"$OUT" && [ "$RERUNS" = 0 ] \
  && ok "an expired quarantine is real again" || no "expired quarantine (rerun $RERUNS)"
triage 9000000012 --summary "$TMP/summary.md"
grep -q 'known-flaky (quarantined) | failed' "$TMP/summary.md" && ok "the job summary marks it quarantined and failed" \
  || no "summary quarantine marking"

echo "=== inputs ==="
triage 9000000018
[ "$RC" = 0 ] && grep -q 'no failed or cancelled job' <<<"$OUT" && ok "a green run has nothing to triage" || no "green run"
python3 -I - "$FIX/runs/9000000006.json" "$TMP/fx/runs/9000000099.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1])); d.update(databaseId=9000000099, status="in_progress", conclusion=None)
json.dump(d, open(sys.argv[2], "w"))
PY
triage 9000000099; [ "$RC" = 3 ] && ok "a run in progress is not triaged (exit 3)" || no "in-progress run rc $RC"
triage abc; [ "$RC" = 2 ] && ok "a non-numeric run id is refused" || no "bad run id rc $RC"
triage 123456; [ "$RC" = 2 ] && ok "an unknown run is an input error" || no "unknown run rc $RC"
printf 'gate\tfirst_seen\tcount\towner\tstatus\texpires\tmatch\tnote\nx.sh\t2026-10-01\t1\t-\tquarantined\t-\t*\tno owner\n' > "$TMP/bad.tsv"
OUT="$("${TRIAGE[@]}" 9000000006 --repo o/r --out-dir "$TMP/out" --ledger "$TMP/bad.tsv" 2>&1)"; RC=$?
[ "$RC" = 2 ] && grep -q 'malformed' <<<"$OUT" && ok "triage refuses a malformed ledger" || no "malformed ledger accepted (rc $RC)"

echo "=== impact selection and the bug skeleton are printed, never run ==="
mkdir -p "$TMP/root/scripts/ci" "$TMP/root/scripts/tests" "$TMP/root/scripts/gen" "$TMP/root/docs/project"
cp "$ROOT_DIR"/scripts/ci/{triage.py,triage.sh,ci_logs.py} "$TMP/root/scripts/ci/"
printf '#!/bin/sh\ntouch "%s/new_bug.ran"\n' "$TMP" > "$TMP/root/scripts/gen/new_bug.sh"
printf '#!/bin/sh\ntouch "%s/select.ran"\n' "$TMP" > "$TMP/root/scripts/tests/select_gates.sh"
OUT="$(bash "$TMP/root/scripts/ci/triage.sh" 39748808077 --repo o/r --out-dir "$TMP/out2" --ledger "$LEDGER" 2>&1)"
R2="$TMP/out2/39748808077.md"
grep -q 'nix develop --command bash scripts/tests/select_gates.sh --base 0000000000000000000000000000000000000001 --head 8c8db85a' "$R2" 2>/dev/null \
  && ok "impact-selected command for the suspected range" || no "select_gates command missing"
grep -q 'bash scripts/gen/new_bug.sh "check_stack_budget.sh: extern_intrinsic_name' "$R2" 2>/dev/null \
  && ok "new_bug.sh command printed" || no "new_bug.sh command missing"
[ ! -e "$TMP/new_bug.ran" ] && [ ! -e "$TMP/select.ran" ] && ok "neither helper was executed" || no "a helper was executed"
grep -q 'select_gates\|new_bug' "$TMP/out/39748808077.md" 2>/dev/null \
  && no "commands printed although the helpers do not exist" || ok "absent helpers are not mentioned"

echo "=== the ledger ==="
FH=(python3 -I "$ROOT_DIR/scripts/ci/flake_history.py")
"${FH[@]}" check-ledger >/dev/null 2>&1 && ok "docs/project/flaky_gates.tsv is well formed" || no "the committed ledger is malformed"
"${FH[@]}" check-ledger --ledger "$LEDGER" >/dev/null 2>&1 && ok "the test ledger is well formed" || no "test ledger rejected"
bad_ledger() { # <label> <row>
  printf 'gate\tfirst_seen\tcount\towner\tstatus\texpires\tmatch\tnote\n%s\n' "$2" > "$TMP/l.tsv"
  "${FH[@]}" check-ledger --ledger "$TMP/l.tsv" >/dev/null 2>&1 && no "accepted: $1" || ok "refused: $1"
}
bad_ledger "quarantine without an owner" "a.sh	2026-10-01	1	-	quarantined	2026-12-01	*	n"
bad_ledger "quarantine without an expiry" "a.sh	2026-10-01	1	bob	quarantined	-	*	n"
bad_ledger "known-flaky without an owner" "a.sh	2026-10-01	1	-	known-flaky	-	*	n"
bad_ledger "a row matching an infra defect" "check_external_checker.sh	2026-10-01	1	bob	known-flaky	-	could not build	n"
bad_ledger "an unknown status" "a.sh	2026-10-01	1	bob	ignored	-	*	n"
bad_ledger "a short row" "a.sh	2026-10-01	1	bob"
bad_ledger "an invalid regex" "a.sh	2026-10-01	1	bob	known-flaky	-	(	n"

echo "=== flake history: fail-then-pass on one SHA ==="
# Run 7001: attempt 1 fails the stack-budget step (a real failure), attempt 2 passes it. Run 7002:
# attempt 1 fails the external checker on lean4export (an infra defect), attempt 2 passes. Run 7003:
# attempt 1 fails Install clang on apt (infra), attempt 2 passes. Only 7001 is a flake candidate.
python3 -I - "$TMP/fx" "$FIX" <<'PY'
import json, os, shutil, sys
fx, fix = sys.argv[1], sys.argv[2]
def api(name, d): json.dump(d, open(os.path.join(fx, "api", name + ".json"), "w"))
def step(n, c): return {"name": n, "conclusion": c}
SB = "Stack budget fails closed (an unbounded stack is never a number; bug 065)"
EX = "External-checker gate (proof evidence re-checked by con-leche)"
LDN = "Linear-discard gate (KNOWN_HOLES H6 — silent discard of a non-Copy value rejected; let _/free/consumed compile)"
# Run 7004: a tolerated (continue-on-error) network failure in the Lean install, then the linear
# discard gate fails an assertion. The gate's row must be real, not the install's infra.
api("runs_7004", {"head_sha": "7004".zfill(40), "head_branch": "main", "event": "push",
                  "created_at": "2026-10-20T01:00:00Z", "run_attempt": 1})
api("runs_7004_attempts_1_jobs_per_page_100",
    {"jobs": [{"id": 70041, "name": "Language-surface gates (Phase 5-6)", "conclusion": "failure",
               "steps": [step("Install Lean (via lean-action)", "success"), step(LDN, "failure")]}]})
shutil.copy(os.path.join(fix, "logs", "9100000020.txt"), os.path.join(fx, "logs", "70041.txt"))
cases = [(7001, "Language-surface gates (Phase 5-6)", SB, 119725877209),
         (7002, "External proof re-check (con-leche)", EX, 9100000005),
         (7003, "SSA test suite", "Install clang", 9100000006)]
for rid, job, st, logjob in cases:
    api(f"runs_{rid}", {"head_sha": f"{rid:040d}", "head_branch": "main", "event": "push",
                        "created_at": "2026-10-20T01:00:00Z", "run_attempt": 2})
    for att, concl in ((1, "failure"), (2, "success")):
        jid = rid * 10 + att
        steps = [step("Run actions/checkout@v4", "success"), step(st, concl)]
        if job.startswith("SSA"):
            steps.append(step("Run SSA test suite", "success" if concl == "success" else "skipped"))
        api(f"runs_{rid}_attempts_{att}_jobs_per_page_100",
            {"jobs": [{"id": jid, "name": job, "conclusion": concl, "steps": steps}]})
        if concl == "failure":
            shutil.copy(os.path.join(fix, "logs", f"{logjob}.txt"), os.path.join(fx, "logs", f"{jid}.txt"))
# Runs 7101 (previous night) and 7102 (tonight): fmt passed, then failed; linear discard failed both.
FMT = "`concrete fmt` gate (Phase 6 #1 — subcommand + semantics-preserving)"
LD = "Linear-discard gate (KNOWN_HOLES H6 — silent discard of a non-Copy value rejected; let _/free/consumed compile)"
for rid, fmt in ((7101, "success"), (7102, "failure")):
    api(f"runs_{rid}", {"head_sha": f"{rid:040d}", "head_branch": "main", "event": "workflow_dispatch",
                        "created_at": f"2026-10-2{rid - 7100}T03:23:00Z", "run_attempt": 1})
    api(f"runs_{rid}_attempts_1_jobs_per_page_100",
        {"jobs": [{"id": rid * 10 + 1, "name": "Language-surface gates (Phase 5-6)", "conclusion": "failure",
                   "steps": [step(FMT, fmt), step(LD, "failure")]}]})
    src = "9100000003" if fmt == "failure" else "9100000004"
    shutil.copy(os.path.join(fix, "logs", f"{src}.txt"), os.path.join(fx, "logs", f"{rid * 10 + 1}.txt"))
PY
H="$TMP/hist.tsv"
"${FH[@]}" record --repo o/r --out "$H" --run 7001 --run 7002 --run 7003 > "$TMP/rec.out" 2>&1 \
  && ok "record reads every attempt of three runs" || { no "record failed"; sed 's/^/        /' "$TMP/rec.out"; }
grep -qE $'^7001\t1\t0+7001\tmain\tpush\t2026-10-20\tLanguage-surface gates \\(Phase 5-6\\)\t.*\tcheck_stack_budget.sh\tfailed\treal$' "$H" \
  && grep -qE $'^7001\t2\t.*\tcheck_stack_budget.sh\tpassed\t-$' "$H" \
  && ok "outcome rows: failed (real) on attempt 1, passed on attempt 2" || no "outcome rows for 7001"
"${FH[@]}" record --repo o/r --out "$TMP/h4.tsv" --run 7004 >/dev/null 2>&1
grep -qE $'^7004\t1\t.*\tcheck_linear_discard.sh\tfailed\treal$' "$TMP/h4.tsv" \
  && ok "a gate failing after a tolerated install failure is recorded real (sections matched by gate)" \
  || { no "tolerated-failure pairing"; sed 's/^/        /' "$TMP/h4.tsv"; }
python3 -I - "$ROOT_DIR/scripts/ci" <<'PY' && ok "two failed non-gate steps are unattributed, never each other's class" || no "non-gate step attribution"
import sys
sys.path.insert(0, sys.argv[1])
import ci_logs as L, flake_history as F
steps = [{"name": "Install tools", "conclusion": "failure"}, {"name": "Upload results", "conclusion": "failure"}]
log = ("##[group]Run curl https://example.invalid/tools\n##[endgroup]\ncurl: (6) Could not resolve host: x\n"
       "##[error]Process completed with exit code 6.\n##[group]Run upload-results\n##[endgroup]\n"
       "error: required report does not exist\n##[error]Process completed with exit code 1.\n")
F.gates_at = lambda sha: []
L.gh_json = lambda args, repo=None: ({"head_sha": "abc", "run_attempt": 1} if "/attempts/" not in args[1]
                                     else {"jobs": [{"id": 1, "name": "build", "conclusion": "failure", "steps": steps}]})
L.job_log = lambda *a: log
got = [(r["step"], r["class"]) for r in F.record_run(1, "o/r", [])]
assert got == [("Install tools", "unattributed"), ("Upload results", "unattributed")], got
steps[:] = steps[:1]
log2 = log.split("##[group]Run upload-results")[0]
L.job_log = lambda *a: log2
got = [(r["step"], r["class"]) for r in F.record_run(1, "o/r", [])]
assert got == [("Install tools", "infra")], got
PY
grep -qE $'\tcheck_external_checker.sh\tfailed\tinfra-defect$' "$H" && ok "the lean4export failure is recorded as an infra defect" \
  || no "lean4export outcome class"
CAND="$("${FH[@]}" candidates --history "$H")"
grep -q '^check_stack_budget.sh	' <<<"$CAND" && ok "a real failure that passed on rerun at one SHA is a flake candidate" \
  || no "stack budget not a candidate: $CAND"
grep -q 'check_external_checker\|Install clang' <<<"$CAND" && no "an infra or infra-defect failure became a flake candidate" \
  || ok "infra and infra-defect failures are never flake candidates"
cp "$ROOT_DIR/docs/project/flaky_gates.tsv" "$TMP/ledger2.tsv"
"${FH[@]}" ledger --history "$H" --ledger "$TMP/ledger2.tsv" --write >/dev/null 2>&1
grep -qE $'^check_stack_budget.sh\t2026-10-20\t1\t-\tcandidate\t-\t\\*\t' "$TMP/ledger2.tsv" \
  && "${FH[@]}" check-ledger --ledger "$TMP/ledger2.tsv" >/dev/null 2>&1 \
  && ok "ledger --write proposes a well-formed candidate row (no owner, status candidate)" || no "ledger --write"
"${FH[@]}" ledger --history "$H" --ledger "$TMP/ledger2.tsv" --write >/dev/null 2>&1
[ "$(grep -c '^check_stack_budget.sh' "$TMP/ledger2.tsv")" = 1 ] && ok "a second write does not duplicate the row" \
  || no "ledger row duplicated"
triage 9000000003 >/dev/null
OUTC="$("${TRIAGE[@]}" 39748808077 --repo o/r --no-rerun --out-dir "$TMP/out3" --ledger "$TMP/ledger2.tsv" 2>&1)"
grep -qE '^  real .*check_stack_budget.sh' <<<"$OUTC" && ok "a candidate row is still real to triage (a person confirms flakes)" \
  || no "candidate row changed the classification"

echo "=== flake history: quarantine reporting and new failures ==="
H2="$TMP/hist2.tsv"
"${FH[@]}" record --repo o/r --out "$H2" --run 7101 --run 7102 >/dev/null 2>&1 || no "record of the nightly pair failed"
printf 'gate\tfirst_seen\tcount\towner\tstatus\texpires\tmatch\tnote\ncheck_concrete_fmt.sh\t2026-10-01\t1\tdana\tquarantined\t2026-12-31\t*\tq\n' > "$TMP/lq.tsv"
REP="$("${FH[@]}" report --history "$H2" --ledger "$TMP/lq.tsv")"
grep -q '^- `check_concrete_fmt.sh`: failed in run 7102#1 (quarantined until 2026-12-31, owner dana)' <<<"$REP" \
  && ! grep -q '^| `check_concrete_fmt.sh`' <<<"$REP" \
  && ok "report lists a quarantined gate separately with its real (failed) outcome" || { no "quarantine report"; echo "$REP" | sed 's/^/        /'; }
grep -q '^| `check_linear_discard.sh` | failed | 7102#1' <<<"$REP" && ok "report shows each gate's latest outcome" || no "report table"
NF="$("${FH[@]}" new-failures --history "$H2" --run 7102 --prev 7101)"; rc=$?
[ "$rc" = 1 ] && grep -q '^new failure: check_concrete_fmt.sh (previous run 7101: passed)' <<<"$NF" \
  && ! grep -q 'check_linear_discard' <<<"$NF" \
  && ok "new-failures names the gate that passed last night and fails tonight, not the old failure" || no "new-failures: $NF"

echo "=== coverage diff ==="
CD=(python3 -I "$ROOT_DIR/scripts/ci/coverage_diff.py")
mk_night() { # <dir> <discovered> <killed> <family=disposition>...
  local d="$1" disc="$2" k="$3"; shift 3
  mkdir -p "$d/mutation-campaign-summary"
  printf 'discovered=%s\nkilled=%s\nsurvived=0\ninvalid=0\nqualified=1\nhead=%040d\n' "$disc" "$k" 0 \
    > "$d/mutation-campaign-summary/mutation-campaign-summary.txt"
  local fd
  for fd in "$@"; do
    mkdir -p "$d/mutation-shard-1/evidence/${fd%%=*}"
    printf 'family=%s\ndisposition=%s\n' "${fd%%=*}" "${fd#*=}" > "$d/mutation-shard-1/evidence/${fd%%=*}/verdict.txt"
  done
}
mk_night "$TMP/n1" 3 3 a=killed b=killed c=killed
mk_night "$TMP/n2" 3 3 a=killed b=killed c=killed
mk_night "$TMP/n3" 3 2 a=killed b=survived d=killed
"${CD[@]}" "$TMP/n1" "$TMP/n2" > "$TMP/cd.out"; rc=$?
[ "$rc" = 0 ] && grep -q 'No coverage regression' "$TMP/cd.out" && ok "unchanged coverage is not a regression" || no "coverage diff (same) rc $rc"
"${CD[@]}" "$TMP/n2" "$TMP/n3" > "$TMP/cd.out"; rc=$?
[ "$rc" = 1 ] && grep -q 'REGRESSION family b: killed -> survived' "$TMP/cd.out" \
  && grep -q 'REGRESSION family c: killed last night, no longer discovered' "$TMP/cd.out" \
  && ok "a killed family that survives or disappears is a regression" || no "coverage regression not detected (rc $rc)"
"${CD[@]}" "" "$TMP/n1" > "$TMP/cd.out"; rc=$?
[ "$rc" = 0 ] && grep -q 'No previous nightly' "$TMP/cd.out" && ok "the first night says there is nothing to compare" || no "first night rc $rc"
"${CD[@]}" "$TMP/n1" "$TMP/empty" > "$TMP/cd.out"; rc=$?
[ "$rc" = 2 ] && ok "a run without a campaign summary is refused, not read as no change" || no "missing summary rc $rc"
mk_night "$TMP/n4" 3 3 a=killed b=killed e=survived
mk_night "$TMP/n5" 3 3 a=killed b=killed f=killed
"${CD[@]}" "$TMP/n4" "$TMP/n5" > "$TMP/cd.out"; rc=$?
[ "$rc" = 1 ] && grep -q 'REGRESSION family e: survived last night, no longer discovered' "$TMP/cd.out" \
  && ok "dropping a non-killed family is a regression even when another is added" || no "uncovered removal (rc $rc)"
mkdir -p "$TMP/n6"; cp -R "$TMP/n1/mutation-campaign-summary" "$TMP/n6/"
"${CD[@]}" "$TMP/n1" "$TMP/n6" > "$TMP/cd.out"; rc=$?
[ "$rc" = 2 ] && grep -q 'Evidence incomplete' "$TMP/cd.out" && ok "missing shard records tonight are incomplete evidence, not 'unchanged'" \
  || no "missing shard records (rc $rc)"

echo "=== the nightly report, end to end ==="
# Tonight = run 7102 (its fmt gate newly red, coverage lost family c), previous night = run 7101.
python3 -I - "$TMP/fx" "$(git -C "$ROOT_DIR" rev-parse HEAD)" <<'PY'
import json, os, sys
fx, head = sys.argv[1], sys.argv[2]
def view(rid, created, concl):
    plan = {"databaseId": rid * 100, "name": "Gate mutation plan (inventory, once)", "conclusion": "success",
            "status": "completed", "steps": []}
    lang = {"databaseId": rid * 10 + 1, "name": "Language-surface gates (Phase 5-6)", "conclusion": concl,
            "status": "completed", "steps": [{"name": "Linear-discard gate (H6)", "conclusion": concl}]}
    json.dump({"databaseId": rid, "attempt": 1, "headSha": head, "headBranch": "main", "event": "workflow_dispatch",
               "status": "completed", "conclusion": concl, "workflowName": "CI", "url": "u", "createdAt": created,
               "jobs": [plan, lang]}, open(os.path.join(fx, "runs", f"{rid}.json"), "w"))
view(7101, "2026-10-21T03:23:00Z", "failure")
view(7102, "2026-10-22T03:23:00Z", "failure")
json.dump([{"databaseId": 7102, "createdAt": "2026-10-22T03:23:00Z"},
           {"databaseId": 7101, "createdAt": "2026-10-21T03:23:00Z"}],
          open(os.path.join(fx, "runlist_dispatch.json"), "w"))
PY
mkdir -p "$TMP/fx/artifacts"
mk_night "$TMP/fx/artifacts/7101" 3 3 a=killed b=killed c=killed
mk_night "$TMP/fx/artifacts/7102" 3 2 a=killed b=killed d=killed
NR=(python3 -I "$ROOT_DIR/scripts/ci/nightly_report.py")
"${NR[@]}" 7102 --repo o/r --work "$TMP/nightly" --summary "$TMP/nightly.md" --no-rerun > "$TMP/nr.out" 2>&1; rc=$?
if [ "$rc" = 1 ] \
   && grep -q '^## Nightly qualification: run 7102' "$TMP/nightly.md" \
   && grep -q 'verdict=' "$TMP/nightly.md" \
   && grep -q '^new failure: check_concrete_fmt.sh (previous run 7101: passed)' "$TMP/nightly.md" \
   && grep -q '^### Triage of run 7102' "$TMP/nightly.md" \
   && grep -q 'REGRESSION family c: killed last night, no longer discovered' "$TMP/nightly.md" \
   && [ -s "$TMP/nightly/outcomes-7102.tsv" ]; then
  ok "nightly report: verdict, new failures, triage and coverage diff in the summary; exit 1"
else
  no "nightly report (rc $rc)"; sed 's/^/        /' "$TMP/nr.out" | awk 'NR<=20'; sed 's/^/        /' "$TMP/nightly.md" 2>/dev/null | awk 'NR<=30'
fi
grep -q '^verdict=qualified' "$TMP/nightly.md" && no "a hostile nightly record qualified" \
  || ok "the stage (c) verdict of an incomplete campaign record is not qualified, and the report is red"
"${NR[@]}" 9000000018 --repo o/r --work "$TMP/nightly2" --summary "$TMP/nightly2.md" > "$TMP/nr2.out" 2>&1; rc=$?
[ "$rc" = 0 ] && grep -q 'landing-tier dispatch' "$TMP/nr2.out" && ok "a landing-tier run is not reported on" || no "landing-tier run (rc $rc)"

echo "=== the nightly workflow ==="
NW="$ROOT_DIR/.github/workflows/nightly.yml"
python3 -I - "$NW" "$ROOT_DIR/.github/workflows/lean_action_ci.yml" <<'PY' && ok "nightly.yml dispatches the campaign tier on main under the repository guard and reports on CI completions" || no "nightly.yml shape"
import sys, yaml
nw = yaml.safe_load(open(sys.argv[1])); ci = yaml.safe_load(open(sys.argv[2]))
on = nw.get(True) or nw.get("on")
assert "schedule" in on and "workflow_run" in on and on["workflow_run"]["workflows"] == ["CI"], on
d, r = nw["jobs"]["dispatch"], nw["jobs"]["report"]
assert "lambdaclass/concrete" in d["if"] and "CONCRETE_NIGHTLY_QUALIFICATION" in d["if"], d["if"]
run = "\n".join(s.get("run", "") for s in d["steps"])
assert "gh workflow run CI" in run and "--ref main" in run and "campaign=true" in run, run
assert "CONCRETE_SELF_HOSTED_CAMPAIGN" in str(d["runs-on"]) and "CONCRETE_SELF_HOSTED_CAMPAIGN" in str(r["runs-on"])
assert "nightly_report.py" in "\n".join(s.get("run", "") for s in r["steps"])
assert nw["permissions"]["actions"] == "write"
# The campaign tier is not scheduled twice: CI itself has no cron left.
cion = ci.get(True) or ci.get("on")
assert "schedule" not in cion, "CI still has a schedule: the nightly would run the campaign twice"
PY

echo ""
echo "CI-TRIAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
