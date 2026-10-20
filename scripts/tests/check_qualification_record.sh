#!/usr/bin/env bash
# ONE QUALIFICATION VERDICT OVER EVERY REQUIRED JOB (ROADMAP promotion pipeline, stage (c)).
#
# scripts/ci/qualification_record.py turns GitHub run records into one stage (c) verdict. The
# mutation aggregate's qualified=1 is one component of it: the aggregate job succeeds while the
# proof replay, fuzzing, multi-kernel or regression jobs fail. A real verdict is reachable only by a
# multi-hour dispatched run, so the decision is attacked here with constructed run records built
# from the REAL workflow's job list: the honest record must qualify (positive control), and each
# hostile one must not.
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
_n="$(grep -c '^job' "$TMP/req" || true)"
_wf="$(awk '/^jobs:/{j=1;next} j&&/^[^ ]/{j=0} j&&/^  [A-Za-z0-9_-]+:[[:space:]]*$/{n++} END{print n+0}' "$WF")"
[ "$_n" = "$_wf" ] && [ "$_n" -gt 0 ] && ok "required jobs ($_n) equal the workflow's jobs ($_wf)" \
  || no "required jobs $_n vs workflow jobs $_wf"
for j in proof-replay fuzz-campaign multi-kernel mutation-aggregate mutation-shard main-tests external-checker; do
  grep -q "^job	$j	" "$TMP/req" && ok "$j is required" || no "$j is not required"
done
grep -q '^step	multi-kernel	Multi-kernel mutation coverage' "$TMP/req" \
  && ok "the scheduled multi-kernel mutation step is required" || no "multi-kernel mutation step not required"

# mk VARIANT OUT — writes a run record; the variant names the one hostile change.
mk() {
  python3 -I - "$TMP/req" "$1" "$2" "$SHA" "$OTHER" <<'PY'
import json, sys
req, variant, out, sha, other = sys.argv[1:6]
jobs = []
for ln in open(req, encoding="utf-8"):
    f = ln.rstrip("\n").split("\t")
    if f[0] != "job":
        continue
    jid, name, kind = f[1], f[2], f[3]
    names = [name.split("${{")[0] + str(i) for i in (1, 2, 3)] if kind == "matrix" else [name]
    for n in names:
        j = {"name": n, "conclusion": "success", "status": "completed", "steps": [], "_id": jid}
        if jid == "multi-kernel":
            j["steps"] = [{"name": s.split("\t")[2].rstrip("\n"), "conclusion": "success"}
                          for s in open(req, encoding="utf-8") if s.startswith("step\tmulti-kernel\t")]
        jobs.append(j)
run = {"databaseId": 42, "headSha": sha, "event": "workflow_dispatch", "status": "completed",
       "conclusion": "success", "jobs": jobs}
def job(jid):
    return [j for j in jobs if j["_id"] == jid]
if variant == "cancelled":
    job("proof-replay")[0]["conclusion"] = "cancelled"
elif variant == "failed-fuzz":
    job("fuzz-campaign")[0]["conclusion"] = "failure"
elif variant == "skipped":
    job("multi-kernel")[0]["conclusion"] = "skipped"
elif variant == "missing":
    jobs.remove(job("main-tests")[0])
elif variant == "no-shards":
    for j in job("mutation-shard"):
        jobs.remove(j)
elif variant == "shard-failed":
    job("mutation-shard")[1]["conclusion"] = "failure"
elif variant == "other-sha":
    run["headSha"] = other
elif variant == "in-progress":
    run["status"] = "in_progress"
elif variant == "step-skipped":
    job("multi-kernel")[0]["steps"][0]["conclusion"] = "skipped"
elif variant == "only-mutation":
    run["jobs"] = [j for j in jobs if j["_id"].startswith("mutation-")]
elif variant != "honest":
    sys.exit("unknown variant " + variant)
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
verdict 0 "every required job succeeded at the candidate: qualified" "$TMP/honest.json"
grep -q '^component=mutation-aggregate qualified=1' "$TMP/out" && ok "the mutation aggregate is reported as a component" \
  || no "the mutation aggregate component line is missing"

echo "=== negative controls: each must not qualify ==="
for v in cancelled failed-fuzz skipped missing no-shards shard-failed other-sha in-progress step-skipped only-mutation; do
  mk "$v" "$TMP/$v.json"
  verdict 1 "$v: not qualified" "$TMP/$v.json"
done
# The mutation aggregate succeeded in only-mutation, yet the verdict is not-qualified.
mk only-mutation "$TMP/om.json"
python3 -I "$REC" --candidate "$SHA" --workflow "$WF" --run-json "$TMP/om.json" > "$TMP/out" 2>&1 || true
grep -q '^component=mutation-aggregate qualified=1' "$TMP/out" && grep -q '^verdict=not-qualified$' "$TMP/out" \
  && ok "mutation qualified=1 alone does not qualify" || no "mutation qualified=1 alone decided the verdict"
# A failure at the candidate is not hidden by a successful rerun supplied alongside it.
verdict 1 "a cancelled run plus a successful run at the same SHA: not qualified" "$TMP/cancelled.json" "$TMP/honest.json"
# Combining runs never imports a run at another SHA.
verdict 1 "an honest run plus a run at another SHA: not qualified" "$TMP/honest.json" "$TMP/other-sha.json"

echo "=== input errors are refusals (exit 2), never a verdict ==="
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$WF" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "no run given: exit 2" || no "no run given: exit $_rc"
_rc=0; python3 -I "$REC" --candidate abc --workflow "$WF" --run-json "$TMP/honest.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "short candidate SHA: exit 2" || no "short candidate SHA: exit $_rc"
printf 'jobs:\n  a:\n    name: A\n' > "$TMP/tiny.yml"
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$TMP/tiny.yml" --run-json "$TMP/honest.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a vacuous workflow parse: exit 2" || no "a vacuous workflow parse: exit $_rc"
echo '{}' > "$TMP/nojobs.json"
_rc=0; python3 -I "$REC" --candidate "$SHA" --workflow "$WF" --run-json "$TMP/nojobs.json" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = 2 ] && ok "a run record without jobs: exit 2" || no "a run record without jobs: exit $_rc"

echo ""
echo "qualification record: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
