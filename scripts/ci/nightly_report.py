#!/usr/bin/env python3
"""The nightly qualification report for one completed CI campaign run on main.

Usage: nightly_report.py <run-id> [--repo R] [--work DIR] [--summary FILE] [--no-rerun]

Run by .github/workflows/nightly.yml when a CI `workflow_dispatch` run on main completes. A
landing-tier dispatch (no campaign jobs ran) is ignored. For a campaign run it writes, to the
summary (the job summary in CI):

  1. the stage (c) verdict, scripts/ci/qualification_record.py over this run at its head SHA;
  2. per-gate outcomes (flake_history.py record) and the gates that failed now but not in the
     previous nightly campaign run (`new-failures`);
  3. triage of the run when it did not succeed (scripts/ci/triage.py: infra and known flakes are
     rerun once, real failures get a report; a rerun never changes the verdict of step 1, which
     reads the first attempt);
  4. the mutation-coverage diff against the previous nightly campaign run (coverage_diff.py).

Exit 1 when triage reported a real failure, a new failure appeared or coverage regressed; 0
otherwise (including a landing-tier run). The work directory keeps the outcomes TSV and triage
reports for upload as artifacts.
"""
import argparse
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import ci_logs as L  # noqa: E402

PLAN_JOB = "Gate mutation plan (inventory, once)"


def is_campaign(run):
    return any(j.get("name") == PLAN_JOB and j.get("conclusion") not in (None, "skipped")
               for j in run.get("jobs") or [])


def previous_campaign(run, repo):
    runs = L.gh_json(["run", "list", "--workflow", "CI", "--branch", "main", "--event", "workflow_dispatch",
                      "--status", "completed", "--limit", "30", "--json", "databaseId,createdAt"], repo)
    for r in runs:
        if r.get("createdAt", "") >= run.get("createdAt", "") or r["databaseId"] == run["databaseId"]:
            continue
        try:
            if is_campaign(L.run_record(r["databaseId"], repo)):
                return str(r["databaseId"])
        except L.GhError:
            continue
    return None


def sh(args, summary=None):
    p = subprocess.run(args, capture_output=True, text=True)
    text = (p.stdout or "") + (p.stderr or "")
    if summary:
        with open(summary, "a", encoding="utf-8") as fh:
            fh.write(text + "\n")
    print(text, end="")
    return p.returncode


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument("run_id")
    ap.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY"))
    ap.add_argument("--work", default=os.path.join(os.environ.get("RUNNER_TEMP", "/tmp"), "nightly"))
    ap.add_argument("--summary", default=os.environ.get("GITHUB_STEP_SUMMARY"))
    ap.add_argument("--no-rerun", action="store_true")
    a = ap.parse_args(argv)
    if not a.repo:
        print("nightly_report: pass --repo", file=sys.stderr)
        return 2
    os.makedirs(a.work, exist_ok=True)
    summary = a.summary or os.path.join(a.work, "summary.md")
    try:
        run = L.run_record(a.run_id, a.repo)
    except L.GhError as e:
        print(f"nightly_report: {e}", file=sys.stderr)
        return 2
    if not is_campaign(run):
        print(f"nightly_report: run {a.run_id} ran no campaign jobs (landing-tier dispatch); nothing to report")
        return 0
    py = [sys.executable, "-I"]
    sha = run.get("headSha", "")
    with open(summary, "a", encoding="utf-8") as fh:
        fh.write(f"## Nightly qualification: run {a.run_id} at `{sha[:12]}` (attempt {run.get('attempt')}, "
                 f"{run.get('conclusion')})\n\n### Stage (c) verdict\n\n```\n")
    sh(py + [os.path.join(HERE, "qualification_record.py"), "--run", a.run_id, "--candidate", sha], summary)
    with open(summary, "a", encoding="utf-8") as fh:
        fh.write("```\n\n")

    bad = 0
    prev = None
    try:
        prev = previous_campaign(run, a.repo)
    except L.GhError as e:
        print(f"nightly_report: no previous campaign run: {e}")
    hist = os.path.join(a.work, f"outcomes-{a.run_id}.tsv")
    if os.path.exists(hist):
        os.remove(hist)
    rec = py + [os.path.join(HERE, "flake_history.py"), "record", "--repo", a.repo, "--out", hist, "--run", a.run_id]
    if prev:
        rec += ["--run", prev]
    if sh(rec) != 0:
        bad = 1
    with open(summary, "a", encoding="utf-8") as fh:
        fh.write("### New failures since the previous nightly\n\n```\n")
    if prev:
        bad |= sh(py + [os.path.join(HERE, "flake_history.py"), "new-failures", "--history", hist,
                        "--run", a.run_id, "--prev", prev], summary) != 0
    else:
        sh(["echo", "no previous nightly campaign run on main to compare against"], summary)
    with open(summary, "a", encoding="utf-8") as fh:
        fh.write("```\n\n### Gate outcomes\n\n")
    sh(py + [os.path.join(HERE, "flake_history.py"), "report", "--history", hist], summary)

    if run.get("conclusion") != "success":
        t = py + [os.path.join(HERE, "triage.py"), a.run_id, "--repo", a.repo, "--summary", summary,
                  "--out-dir", os.path.join(a.work, "triage")]
        if a.no_rerun:
            t.append("--no-rerun")
        rc = sh(t)
        bad |= rc != 0

    cur_dir, prev_dir = os.path.join(a.work, "artifacts-cur"), os.path.join(a.work, "artifacts-prev")
    for rid, d in ((a.run_id, cur_dir), (prev, prev_dir)):
        if not rid:
            continue
        for pat in ("mutation-campaign-summary", "mutation-shard-*"):
            try:
                L.gh(["run", "download", rid, "-D", d, "-p", pat], a.repo)
            except L.GhError as e:
                print(f"nightly_report: artifacts {pat} of run {rid}: {e}")
    rc = sh(py + [os.path.join(HERE, "coverage_diff.py"), prev_dir if prev else "", cur_dir], summary)
    bad |= rc == 1
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
