#!/usr/bin/env bash
# ONE QUALIFICATION VERDICT OVER EVERY REQUIRED JOB (ROADMAP promotion pipeline, stage (c)).
#
# scripts/ci/qualification_record.py turns GitHub run records into one stage (c) verdict. The
# mutation aggregate's qualified=1 is one component of it: the aggregate job succeeds while the
# proof replay, fuzzing, multi-kernel or regression jobs fail. A real verdict is reachable only by a
# multi-hour dispatched run, so the decision is attacked here with constructed run records built
# from the REAL workflow's jobs, matrix instances and steps: the honest record must qualify
# (positive control), and each hostile one must not.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REC="$ROOT_DIR/scripts/ci/qualification_record.py"
WF="$ROOT_DIR/.github/workflows/lean_action_ci.yml"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
SHA=1111111111111111111111111111111111111111
OTHER=2222222222222222222222222222222222222222

echo "=== the required set is the whole workflow ==="
python3 -I "$REC" --list-required > "$TMP/req" || { echo "error: --list-required failed"; exit 2; }
# Independent count of job ids (a header may carry a trailing comment) vs distinct required ids.
_ids="$(awk -F'\t' '$1=="job"{print $2}' "$TMP/req" | sort -u | grep -c . || true)"
_wf="$(awk '/^jobs:/{j=1;next} j&&/^[^ #]/{j=0} j&&/^  [A-Za-z0-9_-]+:[[:space:]]*(#.*)?$/{n++} END{print n+0}' "$WF")"
[ "$_ids" = "$_wf" ] && [ "$_ids" -gt 0 ] && ok "required job ids ($_ids) equal the workflow's job headers ($_wf)" \
  || no "required job ids $_ids vs workflow job headers $_wf"
for j in proof-replay fuzz-campaign multi-kernel mutation-aggregate mutation-shard main-tests external-checker ssa-tests; do
  grep -q "^job	$j	" "$TMP/req" && ok "$j is required" || no "$j is not required"
done
_shards="$(grep -c '^job	mutation-shard	' "$TMP/req" || true)"
_set="$(sed -n 's/^ *MUTATION_SHARDS: *\([0-9][0-9]*\).*/\1/p' "$WF")"
[ -n "$_set" ] && [ "$_shards" = "$_set" ] && ok "every mutation shard 1..$_set is a required instance" \
  || no "required shard instances $_shards vs MUTATION_SHARDS=$_set"
grep -q '^step	multi-kernel	Multi-kernel mutation coverage' "$TMP/req" \
  && ok "the scheduled multi-kernel mutation step is required" || no "multi-kernel mutation step not required"
grep -q '^step	ssa-tests	Run SSA test suite$' "$TMP/req" \
  && ok "an ordinary gate step (Run SSA test suite) is required" || no "ordinary gate steps are not required"
grep -q '^step	build	Wait before retrying the Lean install$' "$TMP/req" \
  && no "a conditional retry step is required" || ok "conditional retry steps are optional"

echo "=== the workflow parse fails closed ==="
# A trailing comment on a job header is valid YAML: the required set must not change.
sed 's/^  ssa-tests:$/  ssa-tests: # a comment/' "$WF" > "$TMP/wf_comment.yml"
cmp -s "$WF" "$TMP/wf_comment.yml" && no "control wf_comment: could not build its copy"
if python3 -I "$REC" --list-required --workflow "$TMP/wf_comment.yml" > "$TMP/req_comment" 2>/dev/null \
   && cmp -s "$TMP/req" "$TMP/req_comment"; then ok "a commented job header keeps the required set"
else no "a commented job header changed or broke the required set"; fi
# Headers the parser does not understand refuse (exit 2) instead of dropping a job.
sed 's/^  ssa-tests:$/  ssa-tests: { name: x }/' "$WF" > "$TMP/wf_flow.yml"
_rc=0; python3 -I "$REC" --list-required --workflow "$TMP/wf_flow.yml" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a flow-style job header refuses (exit 2)" || no "a flow-style job header: exit $_rc"
sed 's/^\( *MUTATION_SHARDS:\).*/\1 many/' "$WF" > "$TMP/wf_nocount.yml"
_rc=0; python3 -I "$REC" --list-required --workflow "$TMP/wf_nocount.yml" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "an unreadable matrix size refuses (exit 2)" || no "an unreadable matrix size: exit $_rc"
sed 's/^    name: SSA test suite$/    name: SSA test suite ${{ matrix.x }}/' "$WF" > "$TMP/wf_matrix.yml"
_rc=0; python3 -I "$REC" --list-required --workflow "$TMP/wf_matrix.yml" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a matrix job without a known size refuses (exit 2)" || no "an unknown matrix job: exit $_rc"
printf 'jobs:\n  a:\n    name: A\n' > "$TMP/tiny.yml"
_rc=0; python3 -I "$REC" --list-required --workflow "$TMP/tiny.yml" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a vacuous workflow parse refuses (exit 2)" || no "a vacuous workflow parse: exit $_rc"

# mk VARIANT OUT — writes a run record; the variant names the one hostile change.
mk() {
  python3 -I - "$TMP/req" "$1" "$2" "$SHA" "$OTHER" <<'PY'
import json, sys
req, variant, out, sha, other = sys.argv[1:6]
rows = [l.rstrip("\n").split("\t") for l in open(req, encoding="utf-8")]
steps = {}
for kind, jid, name in rows:
    if kind == "step":
        steps.setdefault(jid, []).append(name)
jobs = []
for kind, jid, name in rows:
    if kind != "job":
        continue
    jobs.append({"name": name, "status": "completed", "conclusion": "success", "_id": jid,
                 "steps": [{"name": "Set up job", "status": "completed", "conclusion": "success"}]
                          + [{"name": s, "status": "completed", "conclusion": "success"} for s in steps.get(jid, [])]})
run = {"databaseId": 42, "headSha": sha, "event": "workflow_dispatch", "status": "completed",
       "conclusion": "success", "jobs": jobs}
def job(jid):
    return [j for j in jobs if j["_id"] == jid]
def step(jid, name):
    return [s for s in job(jid)[0]["steps"] if s["name"] == name][0]
v = variant
if v == "cancelled":
    job("proof-replay")[0]["conclusion"] = "cancelled"
elif v == "failed-fuzz":
    job("fuzz-campaign")[0]["conclusion"] = "failure"
elif v == "skipped":
    job("multi-kernel")[0]["conclusion"] = "skipped"
elif v == "missing":
    jobs.remove(job("main-tests")[0])
elif v == "no-shards":
    for j in job("mutation-shard"):
        jobs.remove(j)
elif v == "one-shard-missing":
    jobs.remove(job("mutation-shard")[5])
elif v == "only-first-shard":
    for j in job("mutation-shard")[1:]:
        jobs.remove(j)
elif v == "duplicate-shard":
    jobs.append(json.loads(json.dumps(job("mutation-shard")[0])))
elif v == "unexpected-shard":
    extra = json.loads(json.dumps(job("mutation-shard")[0]))
    extra["name"] = extra["name"].rsplit(" ", 1)[0] + " 99"
    jobs.append(extra)
elif v == "shard-failed":
    job("mutation-shard")[1]["conclusion"] = "failure"
elif v == "other-sha":
    run["headSha"] = other
elif v == "in-progress":
    run["status"] = "in_progress"
elif v == "run-cancelled":
    run["conclusion"] = "cancelled"
elif v == "job-in-progress":
    job("ssa-tests")[0]["status"] = "in_progress"
elif v == "step-skipped":
    step("multi-kernel", "Multi-kernel mutation coverage (scheduled only — mutates source, 2 rebuilds)")["conclusion"] = "skipped"
elif v == "gate-step-omitted":
    j = job("ssa-tests")[0]; j["steps"] = [s for s in j["steps"] if s["name"] != "Run SSA test suite"]
elif v == "gate-step-skipped":
    step("ssa-tests", "Run SSA test suite")["conclusion"] = "skipped"
elif v == "gate-step-failed":
    step("ssa-tests", "Run SSA test suite")["conclusion"] = "failure"
elif v == "no-steps":
    for j in jobs:
        j["steps"] = []
elif v == "only-mutation":
    run["jobs"] = [j for j in jobs if j["_id"].startswith("mutation-")]
elif v != "honest":
    sys.exit("unknown variant " + v)
for j in jobs:
    j.pop("_id", None)
json.dump(run, open(out, "w", encoding="utf-8"))
PY
}

verdict() { # expected-exit label run-json...
  local want="$1" label="$2"; shift 2
  local args=() rc=0
  for f in "$@"; do args+=(--run-json "$f"); done
  python3 -I "$REC" --candidate "$SHA" --workflow "$WF" "${args[@]}" > "$TMP/out" 2>&1 || rc=$?
  if [ "$rc" = "$want" ]; then
    if [ "$want" = 0 ]; then
      grep -q '^verdict=qualified$' "$TMP/out" && ok "$label" || no "$label: exit 0 without verdict=qualified"
    else
      grep -q '^verdict=not-qualified$' "$TMP/out" && grep -q '^reason=' "$TMP/out" \
        && ok "$label" || no "$label: refused without a not-qualified verdict and a reason"
    fi
  else
    no "$label: exit $rc, expected $want"; sed 's/^/      /' "$TMP/out" | awk 'NR<=8'
  fi
}

echo "=== positive control ==="
mk honest "$TMP/honest.json"
verdict 0 "every required job and step succeeded at the candidate: qualified" "$TMP/honest.json"
grep -q '^component=mutation-aggregate qualified=1' "$TMP/out" && ok "the mutation aggregate is reported as a component" \
  || no "the mutation aggregate component line is missing"

echo "=== negative controls: each must not qualify ==="
for v in cancelled failed-fuzz skipped missing no-shards one-shard-missing only-first-shard duplicate-shard \
         unexpected-shard shard-failed other-sha in-progress run-cancelled job-in-progress step-skipped \
         gate-step-omitted gate-step-skipped gate-step-failed no-steps only-mutation; do
  mk "$v" "$TMP/$v.json"
  verdict 1 "$v: not qualified" "$TMP/$v.json"
done
# The mutation aggregate succeeded in only-mutation, yet the verdict is not-qualified.
python3 -I "$REC" --candidate "$SHA" --workflow "$WF" --run-json "$TMP/only-mutation.json" > "$TMP/out" 2>&1 || true
grep -q '^component=mutation-aggregate qualified=1' "$TMP/out" && grep -q '^verdict=not-qualified$' "$TMP/out" \
  && ok "mutation qualified=1 alone does not qualify" || no "mutation qualified=1 alone decided the verdict"
# A failure at the candidate is not hidden by a successful rerun supplied alongside it.
verdict 1 "a cancelled job plus a successful run at the same SHA: not qualified" "$TMP/cancelled.json" "$TMP/honest.json"
# Combining runs never imports a run at another SHA.
verdict 1 "an honest run plus a run at another SHA: not qualified" "$TMP/honest.json" "$TMP/other-sha.json"

echo "=== input errors are refusals (exit 2), never a verdict ==="
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$WF" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "no run given: exit 2" || no "no run given: exit $_rc"
_rc=0; python3 -I "$REC" --candidate abc --workflow "$WF" --run-json "$TMP/honest.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "short candidate SHA: exit 2" || no "short candidate SHA: exit $_rc"
echo '{}' > "$TMP/nojobs.json"
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$WF" --run-json "$TMP/nojobs.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a run record without jobs: exit 2" || no "a run record without jobs: exit $_rc"

echo ""
echo "qualification record: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
