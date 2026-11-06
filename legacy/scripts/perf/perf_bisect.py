#!/usr/bin/env python3
"""Name the commit that introduced a performance regression.

  perf_bisect.py --repo CLONE --metric NAME --good SHA --bad SHA --out result.json
                 [--band B] [--machine M] [--work DIR] [--repeats N]

(Named perf_bisect, not bisect: a bisect.py next to suite.py shadows the
standard library module that `random` and `tempfile` import.)

Walks the first-parent commits good..bad of CLONE with the same benchmark the
suite uses (suite.py run --bench <bench>), in one scratch worktree (WORK/bisect)
seeded with CLONE's .lake so each probe is an incremental build. Must run where
`lake` is on PATH (inside `nix develop`). `good` must be on bad's first-parent
chain (verdict "not_ancestor" otherwise).

1. Re-measure both endpoints now. If bad is not above the detector's alarm
   line over good (band from --band, widened by the endpoints' own noise), the
   regression did not reproduce: verdict "not_reproduced", no culprit.
2. Otherwise binary-search with perflib.next_probe; a probe is bad when it is
   above that same alarm line (perflib.classify / regression_limit), so a
   partial step is named where it first crosses the line. A commit that does
   not build, or whose probe could not count instructions, is skipped, which can
   widen the answer to a candidate range. Probes count instructions:u, so a
   bisect can run on a busy machine.
Every probe writes its own record file, and a probe counts only when the suite
exited 0 and its record names the probed sha with status ok.
"""

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

SECTION = {"compile": "compile", "check": "compile", "pass": "compile", "frontend": "compile",
           "growth": "synthetic", "lake": "lake", "tests": "tests", "gate": "gates"}


def section_for(metric):
    group, bench, _ = metric.split(".", 2)
    if group in ("compile", "pass", "frontend") and bench.startswith("syn_"):
        return "synthetic", bench
    return SECTION[group], bench


def unit_for(metric):
    q = metric.rsplit(".", 1)[-1]
    if q == "instr":
        return "instr"
    for suffix, unit in (("_kb", "kb"), ("_exp", "exp"), ("_ms", "ms"), ("_s", "s")):
        if q.endswith(suffix):
            return unit
    return "instr"


def sh(cmd, cwd=None, check=True):
    return subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, check=check)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", required=True)
    ap.add_argument("--metric", required=True)
    ap.add_argument("--good", required=True)
    ap.add_argument("--bad", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--machine", default=os.environ.get("PERF_MACHINE", "local"))
    ap.add_argument("--work")
    ap.add_argument("--repeats", type=int, default=5)
    ap.add_argument("--threshold", type=float, default=perflib.DEFAULTS["threshold"])
    ap.add_argument("--band", type=float, default=perflib.DEFAULTS["min_band"],
                    help="the noise band detect.py used for this metric (summary.json)")
    a = ap.parse_args()

    repo = os.path.abspath(a.repo)
    section, bench = section_for(a.metric)
    unit = unit_for(a.metric)
    good = sh(["git", "-C", repo, "rev-parse", a.good]).stdout.strip()
    bad = sh(["git", "-C", repo, "rev-parse", a.bad]).stdout.strip()
    result = {"schema": "concrete.perf.bisect.v1", "metric": a.metric, "machine": a.machine,
              "good": good, "bad": bad}
    chain = set(sh(["git", "-C", repo, "rev-list", "--first-parent", bad]).stdout.split())
    if good not in chain:
        result["verdict"] = "not_ancestor"
        with open(a.out, "w") as f:
            json.dump(result, f, indent=1)
            f.write("\n")
        sys.stderr.write("perf_bisect: %s is not on the first-parent chain of %s\n" % (good, bad))
        return 2
    commits = [good] + sh(["git", "-C", repo, "rev-list", "--first-parent", "--reverse",
                           "%s..%s" % (good, bad)]).stdout.split()
    result["range"] = len(commits) - 1
    work = os.path.abspath(a.work or tempfile.mkdtemp(prefix="perf-bisect-"))
    os.makedirs(work, exist_ok=True)
    wt = os.path.join(work, "bisect")
    if os.path.exists(wt):
        sh(["git", "-C", repo, "worktree", "remove", "--force", wt], check=False)
        shutil.rmtree(wt, ignore_errors=True)
    sh(["git", "-C", repo, "worktree", "add", "--detach", wt, bad])
    lake_src = os.path.join(repo, ".lake")
    if os.path.isdir(lake_src):
        sh(["cp", "-a", lake_src, os.path.join(wt, ".lake")])

    probes_log = []
    entries = {}
    seq = [0]

    def measure_once(sha):
        if section != "lake":
            b = subprocess.run(["lake", "build"], cwd=wt, capture_output=True, text=True)
            if b.returncode != 0:
                return None, "build failed"
        seq[0] += 1
        out = os.path.join(work, "probe-%03d-%s.json" % (seq[0], sha[:12]))
        cmd = [sys.executable, os.path.join(HERE, "suite.py"), "run", "--repo", wt, "--machine", a.machine,
               "--out", out, "--only", section, "--bench", bench, "--repeats", str(a.repeats)]
        r = subprocess.run(cmd, capture_output=True, text=True)
        try:
            rec = json.load(open(out))
        except (OSError, ValueError):
            return None, "suite exit %d, no record: %s" % (r.returncode, r.stderr[-300:])
        if r.returncode != 0 or rec.get("sha") != sha or rec.get("status") != "ok":
            return None, "suite exit %d, record sha %s status %s" % (
                r.returncode, str(rec.get("sha"))[:12], rec.get("status"))
        if rec.get("comparable") is not True:
            return None, "probe counted %s, not instructions; not comparable" % rec.get("counter")
        e = rec["metrics"].get(a.metric)
        if e is None:
            return None, "metric absent"
        return e, None

    def measure(sha):
        sh(["git", "-C", wt, "checkout", "-q", "--force", "--detach", sha])
        e, err = measure_once(sha)
        probes_log.append({"sha": sha, "value": e["median"] if e else None, "note": err})
        if e is None:
            return None
        entries[sha] = e
        sys.stderr.write("[bisect] %s %s = %s\n" % (sha[:12], a.metric, e["median"]))
        return e["median"]

    try:
        gv = measure(commits[0])
        bv = measure(commits[-1])
        result.update({"good_value": gv, "bad_value": bv})
        if gv is None or bv is None:
            result["verdict"] = "unmeasurable"
        else:
            # The detector's rule, widened by the noise of these two fresh measurements, so a noisy
            # endpoint cannot manufacture a culprit.
            wn = max(perflib.within_run_noise(entries[commits[0]]),
                     perflib.within_run_noise(entries[commits[-1]]))
            band = max(a.band, perflib.DEFAULTS["noise_k"] * wn)
            limit = perflib.regression_limit(gv, band, a.threshold, unit)
            result.update({"band": round(band, 4), "limit": round(limit, 3)})
            if bv <= limit:
                result["verdict"] = "not_reproduced"
            else:
                # The endpoints are already measured; next_probe never re-picks them.
                culprit, cands, _ = perflib.bisect(commits, measure, limit)
                result.update({"verdict": "culprit" if culprit else "range", "culprit": culprit,
                               "candidates": cands})
                if culprit:
                    result["culprit_subject"] = sh(["git", "-C", repo, "log", "-1", "--format=%s",
                                                    culprit]).stdout.strip()
    finally:
        result["probes"] = probes_log
        sh(["git", "-C", repo, "worktree", "remove", "--force", wt], check=False)
        if not a.work:
            shutil.rmtree(work, ignore_errors=True)
        with open(a.out, "w") as f:
            json.dump(result, f, indent=1)
            f.write("\n")
    print(json.dumps({k: result.get(k) for k in ("verdict", "culprit", "culprit_subject", "good_value", "bad_value")}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
