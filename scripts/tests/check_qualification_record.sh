#!/usr/bin/env bash
# ONE QUALIFICATION VERDICT OVER EVERY REQUIRED JOB (docs/project/PROMOTION_PIPELINE.md, stage (c)).
#
# scripts/ci/qualification_record.py turns GitHub run records into one stage (c) verdict. The
# mutation aggregate's qualified=1 is one component of it: the aggregate job succeeds while the
# proof replay, fuzzing, multi-kernel or regression jobs fail. A real verdict is reachable only by a
# multi-hour dispatched run, so the decision is attacked here with constructed run records built
# from the REAL workflow's jobs, matrix instances and steps: the honest record must qualify
# (positive control), and each hostile one must not.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REC="$ROOT_DIR/scripts/ci/qualification_record.py"
WF="$ROOT_DIR/.github/workflows/lean_action_ci.yml"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
OTHER=2222222222222222222222222222222222222222

# Requirements come from the CANDIDATE's workflow (git show <candidate>:<workflow>). The verdict
# legs use HEAD as the candidate, so the fixture is built from the committed workflow.
SHA="$(git -C "$ROOT_DIR" rev-parse HEAD)" || { echo "error: no HEAD commit"; exit 2; }
git -C "$ROOT_DIR" show "HEAD:.github/workflows/lean_action_ci.yml" > "$TMP/wf.yml" \
  || { echo "error: cannot read the committed workflow"; exit 2; }
WF="$TMP/wf.yml"
python3 -I -c 'import yaml' 2>/dev/null || { echo "error: PyYAML is required (python3-yaml)"; exit 2; }

echo "=== the required set is the whole workflow ==="
python3 -I "$REC" --list-required --candidate "$SHA" > "$TMP/req" || { echo "error: --list-required failed"; exit 2; }
# Independent count of job ids (a header may carry a trailing comment) vs distinct required ids.
_ids="$(awk -F'\t' '$1=="job"{print $2}' "$TMP/req" | sort -u | grep -c . || true)"
_wf="$(awk '/^jobs:/{j=1;next} j&&/^[^ #]/{j=0} j&&/^  [A-Za-z0-9_-]+:[[:space:]]*(#.*)?$/{n++} END{print n+0}' "$WF")"
[ "$_ids" = "$_wf" ] && [ "$_ids" -gt 0 ] && ok "required job ids ($_ids) equal the workflow's job headers ($_wf)" \
  || no "required job ids $_ids vs workflow job headers $_wf"
for j in proof-replay fuzz-campaign multi-kernel mutation-aggregate mutation-shard main-tests external-checker ssa-tests \
         language-gates receipt-gates checker-gates evidence-gates; do
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
grep -q '^step	ssa-tests	Run actions/checkout@v4$' "$TMP/req" \
  && ok "an unnamed action step is required as 'Run <action>'" || no "unnamed action steps are not required"
grep -q '^step	mutation-shard	Upload shard record$' "$TMP/req" \
  && ok "an always() evidence-upload step is required" || no "always() steps are not required"
grep -q '^step	build	Wait before retrying the Lean install$' "$TMP/req" \
  && no "a conditional retry step is required" || ok "conditional retry steps are optional"

echo "=== the workflow parse fails closed ==="
# Each variant is the committed workflow with one change. "same" variants are the same YAML
# meaning and must keep the required set; "refuse" variants must exit 2; "changed" variants must
# still require the SSA gate step.
python3 -I - "$WF" "$TMP" <<'PY'
import re, sys
wf, tmp = sys.argv[1], sys.argv[2]
s = open(wf, encoding="utf-8").read()
def one(old, new, name, count=1):
    assert s.count(old) >= 1, name
    open(tmp + "/" + name + ".yml", "w").write(s.replace(old, new, count))
STEP = "      - name: Run SSA test suite\n"
one("\n  ssa-tests:\n", "\n  ssa-tests: # a comment\n", "same_comment")
one(STEP, '      - "name": Run SSA test suite\n', "same_quoted")
one(STEP, "      - name: &ssa Run SSA test suite\n", "same_anchor")
# ssa-tests' steps reindented two spaces deeper (same YAML meaning)
lines, out, inside, insteps = s.split("\n"), [], False, False
for ln in lines:
    if re.match(r"^  [A-Za-z0-9_-]+:", ln):
        inside = ln.startswith("  ssa-tests:"); insteps = False
    elif inside and ln.startswith("    steps:"):
        insteps = True; out.append(ln); continue
    elif inside and re.match(r"^    \S", ln):
        insteps = False
    out.append(("  " + ln) if (inside and insteps and ln.strip()) else ln)
open(tmp + "/same_reindent.yml", "w").write("\n".join(out))
one(STEP, STEP + "        continue-on-error: False\n", "keep_coe_false")
one(STEP, STEP + "        if: true\n", "keep_if_true")
one(STEP, STEP + "        if: # nothing\n", "refuse_if_null")
one(STEP, STEP + "        if: ${{ true }}\n", "keep_if_expr_true")
one(STEP, STEP + "        continue-on-error: ${{ false }}\n", "keep_coe_expr_false")
one(STEP, STEP + "        if: github.ref == 'refs/heads/main'\n", "refuse_if_unknown")
one(STEP, STEP + "        continue-on-error: ${{ matrix.flaky }}\n", "refuse_coe_expr")
one(STEP, '      - name: ""\n', "refuse_empty_name")
one(STEP, "      - run: echo unnamed\n\n" + STEP, "refuse_unnamed_run")
one(STEP, "      - name: Install clang\n        run: true\n\n" + STEP, "refuse_dup_step")
one(STEP, STEP + "        continue-on-error: maybe\n", "refuse_coe_string")
one("        shard: ${{ fromJSON(needs.mutation-plan.outputs.shards) }}\n",
    "        shard: ${{ fromJSON(needs.mutation-plan.outputs.shards) }}\n        os: [ubuntu-latest, macos-latest]\n",
    "refuse_matrix_dim")
one("        shard: ${{ fromJSON(needs.mutation-plan.outputs.shards) }}\n",
    "        shard: ${{ fromJSON(needs.mutation-plan.outputs.shards) }}\n        include:\n          - shard: 99\n",
    "refuse_matrix_include")
one("      MUTATION_SHARDS: 24", "      MUTATION_SHARDS: many", "refuse_matrix_size")
one("    name: SSA test suite\n", "    name: SSA test suite ${{ matrix.x }}\n", "refuse_unknown_matrix")
one("\n  ssa-tests:\n", "\n  grammar-twin:\n    name: LL(1) grammar check\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@v4\n\n  ssa-tests:\n", "refuse_dup_display")
open(tmp + "/refuse_tiny.yml", "w").write("jobs:\n  a:\n    name: A\n    steps:\n      - run: x\n        name: x\n")
PY
for c in same_comment same_quoted same_anchor same_reindent; do
  if python3 -I "$REC" --list-required --workflow "$TMP/$c.yml" > "$TMP/req_$c" 2>/dev/null && cmp -s "$TMP/req" "$TMP/req_$c"; then
    ok "$c keeps the required set"
  else no "$c changed or broke the required set"; fi
done
for c in keep_coe_false keep_if_true keep_if_expr_true keep_coe_expr_false; do
  # Captured to a file first, never piped into `grep -q`: under pipefail, grep exiting at its first
  # match SIGPIPEs the still-writing producer and the pipeline reads as a failure. Once the workflow
  # grew past a pipe buffer of required rows this failed a random variant under load (3 of 7 runs on
  # 2026-10-23, a different variant each time) — the race check_gate_hygiene.sh describes.
  _rc=0; python3 -I "$REC" --list-required --workflow "$TMP/$c.yml" > "$TMP/req_$c" 2>/dev/null || _rc=$?
  if [ "$_rc" = 0 ] && grep -q '^step	ssa-tests	Run SSA test suite$' "$TMP/req_$c"; then
    ok "$c keeps the SSA gate step required"
  else no "$c made the SSA gate step optional"; fi
done
for c in refuse_if_null refuse_if_unknown refuse_coe_expr refuse_empty_name refuse_unnamed_run refuse_dup_step refuse_coe_string refuse_matrix_dim \
         refuse_matrix_include refuse_matrix_size refuse_unknown_matrix refuse_dup_display refuse_tiny; do
  _rc=0; python3 -I "$REC" --list-required --workflow "$TMP/$c.yml" >/dev/null 2>&1 || _rc=$?
  [ "$_rc" = 2 ] && ok "$c refuses (exit 2)" || no "$c: exit $_rc, expected a refusal"
done
# A verdict reads the candidate's workflow: a different --workflow is refused, not obeyed.
sed 's/Run SSA test suite/Renamed SSA step/' "$WF" > "$TMP/wf_nossa.yml"
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$TMP/wf_nossa.yml" --run-json "$TMP/req" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a --workflow that differs from the candidate's is refused (exit 2)" || no "a foreign --workflow: exit $_rc"

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
run = {"attempt": 1, "databaseId": 42, "headSha": sha, "event": "workflow_dispatch", "status": "completed",
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
elif v == "push-event":
    run["event"] = "push"
elif v == "rerun-attempt":
    run["attempt"] = 2
elif v == "duplicate-step":
    j = job("ssa-tests")[0]; j["steps"].append(dict(step("ssa-tests", "Run SSA test suite")))
elif v == "step-status-absent":
    for j in jobs:
        for st in j["steps"]:
            st.pop("status", None)
elif v == "step-status-null":
    step("ssa-tests", "Run SSA test suite")["status"] = None
elif v == "step-status-empty":
    step("ssa-tests", "Run SSA test suite")["status"] = ""
elif v == "split-first-half":
    run["jobs"] = jobs[: len(jobs) // 2]
elif v == "split-second-half":
    run["jobs"] = jobs[len(jobs) // 2 :]
elif v == "no-steps":
    for j in jobs:
        j["steps"] = []
elif v == "landing-only-dispatch":
    # A dispatch WITHOUT campaign=true: the landing tier ran and passed, the campaign tier was
    # skipped (GitHub reports a skipped matrix job once, under its unexpanded name).
    for jid in ("proof-replay", "fuzz-campaign", "mutation-plan", "mutation-aggregate"):
        for j in job(jid):
            j["conclusion"] = "skipped"; j["steps"] = []
    unexpanded = json.loads(json.dumps(job("mutation-shard")[0]))
    for j in job("mutation-shard"):
        jobs.remove(j)
    unexpanded["name"] = "Gate mutation shard ${{ matrix.shard }}"
    unexpanded["conclusion"] = "skipped"; unexpanded["steps"] = []
    jobs.append(unexpanded)
    step("multi-kernel", "Multi-kernel mutation coverage (scheduled only — mutates source, 2 rebuilds)")["conclusion"] = "skipped"
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
  python3 -I "$REC" --candidate "$SHA" "${args[@]}" > "$TMP/out" 2>&1 || rc=$?
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
         gate-step-omitted gate-step-skipped gate-step-failed no-steps only-mutation push-event rerun-attempt \
         landing-only-dispatch \
         duplicate-step step-status-absent step-status-null step-status-empty split-first-half split-second-half; do
  mk "$v" "$TMP/$v.json"
  verdict 1 "$v: not qualified" "$TMP/$v.json"
done
# The mutation aggregate succeeded in only-mutation, yet the verdict is not-qualified.
python3 -I "$REC" --candidate "$SHA" --run-json "$TMP/only-mutation.json" > "$TMP/out" 2>&1 || true
grep -q '^component=mutation-aggregate qualified=1' "$TMP/out" && grep -q '^verdict=not-qualified$' "$TMP/out" \
  && ok "mutation qualified=1 alone does not qualify" || no "mutation qualified=1 alone decided the verdict"
# Partial runs are never combined: two records, even two halves of one complete run, refuse.
_rc=0; python3 -I "$REC" --candidate "$SHA" --run-json "$TMP/split-first-half.json" \
  --run-json "$TMP/split-second-half.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "two partial runs that together cover every job: refused (exit 2)" || no "two partial runs: exit $_rc"
_rc=0; python3 -I "$REC" --candidate "$SHA" --run-json "$TMP/cancelled.json" \
  --run-json "$TMP/honest.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a failed run plus a successful rerun: refused (exit 2)" || no "a failed run plus a rerun: exit $_rc"

echo "=== input errors are refusals (exit 2), never a verdict ==="
_rc=0; python3 -I "$REC" --candidate "$SHA" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "no run given: exit 2" || no "no run given: exit $_rc"
_rc=0; python3 -I "$REC" --candidate abc --run-json "$TMP/honest.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "short candidate SHA: exit 2" || no "short candidate SHA: exit $_rc"
echo '{}' > "$TMP/nojobs.json"
_rc=0; python3 -I "$REC" --candidate "$SHA" --run-json "$TMP/nojobs.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a run record without jobs: exit 2" || no "a run record without jobs: exit $_rc"

echo ""
echo "qualification record: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
