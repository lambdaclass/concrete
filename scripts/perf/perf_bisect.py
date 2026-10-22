#!/usr/bin/env python3
"""Name the commit that introduced a performance regression.

  perf_bisect.py --repo CLONE --metric NAME --good SHA --bad SHA --out result.json
            [--machine M] [--work DIR] [--repeats N]

Walks the first-parent commits good..bad of CLONE with the same benchmark the
suite uses (suite.py run --bench <bench>), in one scratch worktree (WORK/bisect)
seeded with CLONE's .lake so each probe is an incremental build. Must run where
`lake` is on PATH (inside `nix develop`).

1. Re-measure both endpoints now. If bad is not slower than good by more than
   the detection threshold, the regression did not reproduce: verdict
   "not_reproduced", no culprit (a noisy alarm, or the machine drifted).
2. Otherwise binary-search with perflib.next_probe; a probe is bad when its
   value is nearer the bad endpoint than the good one (perflib.classify). A
   commit that does not build is skipped, which can widen the answer to a
   candidate range instead of one commit.
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

SECTION = {"compile": "compile", "check": "compile", "pass": "compile", "growth": "synthetic",
           "lake": "lake", "tests": "tests", "gate": "gates"}


def section_for(metric):
    group, bench, _ = metric.split(".", 2)
    if group in ("compile", "pass") and bench.startswith("syn_"):
        return "synthetic", bench
    return SECTION[group], bench


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
    a = ap.parse_args()

    repo = os.path.abspath(a.repo)
    section, bench = section_for(a.metric)
    commits = sh(["git", "-C", repo, "rev-list", "--first-parent", "--reverse",
                  "%s..%s" % (a.good, a.bad)]).stdout.split()
    good = sh(["git", "-C", repo, "rev-parse", a.good]).stdout.strip()
    commits = [good] + commits
    work = os.path.abspath(a.work or tempfile.mkdtemp(prefix="perf-bisect-"))
    wt = os.path.join(work, "bisect")
    if os.path.exists(wt):
        sh(["git", "-C", repo, "worktree", "remove", "--force", wt], check=False)
        shutil.rmtree(wt, ignore_errors=True)
    sh(["git", "-C", repo, "worktree", "add", "--detach", wt, commits[-1]])
    lake_src = os.path.join(repo, ".lake")
    if os.path.isdir(lake_src):
        sh(["cp", "-a", lake_src, os.path.join(wt, ".lake")])

    probes_log = []

    def measure(sha):
        sh(["git", "-C", wt, "checkout", "-q", "--force", "--detach", sha])
        if section != "lake":
            b = subprocess.run(["lake", "build"], cwd=wt, capture_output=True, text=True)
            if b.returncode != 0:
                probes_log.append({"sha": sha, "value": None, "note": "build failed"})
                return None
        out = os.path.join(work, "probe.json")
        cmd = [sys.executable, os.path.join(HERE, "suite.py"), "run", "--repo", wt, "--machine", a.machine,
               "--out", out, "--only", section, "--bench", bench, "--repeats", str(a.repeats)]
        r = subprocess.run(cmd, capture_output=True, text=True)
        try:
            rec = json.load(open(out))
        except (OSError, ValueError):
            probes_log.append({"sha": sha, "value": None, "note": "suite failed: %s" % r.stderr[-300:]})
            return None
        e = rec["metrics"].get(a.metric)
        v = e["median"] if e else None
        probes_log.append({"sha": sha, "value": v, "contaminated": rec.get("contaminated")})
        sys.stderr.write("[bisect] %s %s = %s\n" % (sha[:12], a.metric, v))
        return v

    result = {"schema": "concrete.perf.bisect.v1", "metric": a.metric, "machine": a.machine,
              "good": commits[0], "bad": commits[-1], "range": len(commits) - 1}
    try:
        gv = measure(commits[0])
        bv = measure(commits[-1])
        result.update({"good_value": gv, "bad_value": bv})
        if gv is None or bv is None:
            result["verdict"] = "unmeasurable"
        elif bv <= gv * (1 + a.threshold):
            result["verdict"] = "not_reproduced"
        else:
            # The endpoints are already measured; next_probe never re-picks them.
            culprit, cands, probes = perflib.bisect(commits, measure, gv, bv)
            result.update({"verdict": "culprit" if culprit else "range", "culprit": culprit,
                           "candidates": cands})
            if culprit:
                result["culprit_subject"] = sh(["git", "-C", repo, "log", "-1", "--format=%s", culprit]).stdout.strip()
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
