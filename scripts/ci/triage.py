#!/usr/bin/env python3
"""Triage one CI run: classify each failure, rerun infra and known flakes once, report the rest.

Usage: scripts/ci/triage.sh <run-id> [--repo OWNER/NAME] [--no-rerun] [--out-dir DIR]
                                     [--ledger PATH] [--summary FILE]

The classes and their order are documented in scripts/ci/ci_logs.py. What triage does with them:

  * every failed job is infra or known-flaky, attempt 1, run not cancelled as a whole:
        `gh run rerun <id> --failed`, once. A marker (<out-dir>/<run>.rerun) and the run's attempt
        number both stop a second rerun: an attempt >= 2 never reruns again.
  * the same, but the run is on attempt >= 2: the failure survived its one rerun, so it is
        escalated and reported like a real failure.
  * the run was cancelled as a whole (superseded by a newer push, or by hand): nothing is rerun;
        the cancellation is infra and the output says so.
  * any real or infra-defect failure: nothing is rerun (rerunning would spend a campaign on a
        known-red run and could hide the failure behind a green attempt), and a report is written
        to <out-dir>/<run>.md: the gate, its FAIL lines, a local reproduction, the impact-selected
        command when scripts/tests/select_gates.sh exists, the suspected commits, and the
        scripts/gen/new_bug.sh command when that script exists (printed, never run).

A rerun is diagnostic only. Qualification (scripts/ci/qualification_record.py) reads the FIRST
attempt of a run, so a green rerun never turns a red run into a qualified one.

Quarantine never turns red green: a quarantined gate still runs and its step still fails the job;
triage lists it in its own section as `failed (quarantined until <date>)` and reruns it once.

Exit: 0 nothing real (rerun requested, not needed, or a cancelled run); 1 a report was written;
2 usage, input or ledger error; 3 the run has not completed.
"""
import argparse
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_logs as L  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))


def default_repo():
    r = os.environ.get("GITHUB_REPOSITORY") or os.environ.get("GH_REPO")
    if r:
        return r
    try:
        return L.gh(["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]).strip()
    except L.GhError:
        return None


def suspected_commits(run, repo):
    """-> (base, base_note, commits[(sha, subject)], changed_files) or an error string."""
    head, branch = run.get("headSha", ""), run.get("headBranch", "")
    base, note = None, ""
    try:
        prior = L.gh_json(["run", "list", "--workflow", run.get("workflowName") or "CI",
                           "--branch", branch, "--status", "success", "--limit", "30",
                           "--json", "databaseId,headSha,createdAt"], repo)
        for p in prior:
            if p.get("headSha") != head and p.get("createdAt", "") < run.get("createdAt", ""):
                base, note = p["headSha"], f"last green run on {branch}: {p['databaseId']}"
                break
    except L.GhError as e:
        note = f"(no run list: {e})"
    if base is None:
        base, note = "main", f"no earlier green run on {branch}; commits on {branch} not on main"
    try:
        cmp_ = L.gh_json(["api", f"repos/{repo}/compare/{base}...{head}"])
    except L.GhError as e:
        return f"unavailable: {e}"
    commits = [(c.get("sha", "")[:12], (c.get("commit", {}).get("message", "").splitlines() or [""])[0])
               for c in cmp_.get("commits", [])]
    files = [f.get("filename", "") for f in cmp_.get("files", [])]
    return base, note, commits, files


def write_report(path, run, repo, results, escalated, sus):
    head = run.get("headSha", "")
    out = [f"# CI triage: run {run.get('databaseId')}", "",
           f"- run: {run.get('url', '-')}",
           f"- workflow: {run.get('workflowName', '-')}, event `{run.get('event', '-')}`, "
           f"branch `{run.get('headBranch', '-')}`, attempt {run.get('attempt', '-')}",
           f"- head: `{head}`",
           f"- conclusion: {run.get('conclusion', '-')}", ""]
    if escalated:
        out += ["Every failure was infra or a known flake, but this is attempt "
                f"{run.get('attempt')}: it already had its one rerun, so it is escalated.", ""]
    out += ["## Classification", "", "| job | gate | class | outcome | reason |", "|---|---|---|---|---|"]
    for r in results:
        out.append(f"| {r['job']} | `{r['gate']}` | {r['class']} | {r['outcome']} | {r['reason']} |")
    out.append("")
    select = os.path.exists(os.path.join(ROOT, "scripts", "tests", "select_gates.sh"))
    new_bug = os.path.exists(os.path.join(ROOT, "scripts", "gen", "new_bug.sh"))
    base = sus[0] if isinstance(sus, tuple) else None
    for r in results:
        if r["class"] not in ("real", "infra-defect") and not escalated:
            continue
        out += [f"## {r['job']}: `{r['gate']}`", "",
                f"- failing step: {r['step']}",
                f"- class: **{r['class']}** ({r['reason']})", ""]
        out += ["FAIL lines" + (f" (first {len(r['fail_lines'])} of {r['fail_total']})"
                                if r['fail_total'] > len(r['fail_lines']) else "") + ":", "", "```"]
        out += r["fail_lines"] or ["(no FAIL line found in the failing step; read the job log)"]
        out += ["```", "", "Reproduce locally:", "", "```sh", f"git checkout {head}"]
        repro = r["repro"] or f"# the step `{r['step']}` runs no scripts/ command; see the job log"
        out += [f"nix develop --command bash -c 'lake build && {repro}'", "```", ""]
        if select and base:
            out += ["Impact-selected gates for the suspected range:", "", "```sh",
                    f"bash scripts/tests/select_gates.sh --base {base} --head {head}", "```", ""]
        if new_bug:
            first = (r["fail_lines"] or [r["reason"]])[0]
            first = re.sub(r"^(FAIL|FAILED)\s+", "", first)[:100].replace('"', "'")
            out += ["File a bug skeleton (not run automatically; check its usage first):", "", "```sh",
                    f'bash scripts/gen/new_bug.sh "{r["gate"]}: {first}"', "```", ""]
    out += ["## Suspected commits", ""]
    if isinstance(sus, tuple):
        b, note, commits, files = sus
        out.append(f"Range `{b[:12]}...{head[:12]}` ({note}), {len(commits)} commit(s):")
        out.append("")
        for sha, subj in commits[-30:]:
            out.append(f"- `{sha}` {subj}")
        gates = {r["gate"].split()[0] for r in results if r["class"] in ("real", "infra-defect")}
        touched = [f for f in files if os.path.basename(f) in gates]
        if touched:
            out += ["", "The range changes the failing gate itself: " + ", ".join(f"`{t}`" for t in touched)]
    else:
        out.append(sus)
    out.append("")
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(out))
    return "\n".join(out)


def main(argv):
    ap = argparse.ArgumentParser(description="classify a CI run's failures; rerun once or report")
    ap.add_argument("run_id")
    ap.add_argument("--repo")
    ap.add_argument("--no-rerun", action="store_true", help="classify and report; never rerun")
    ap.add_argument("--out-dir", default=os.path.join(ROOT, ".triage"))
    ap.add_argument("--ledger", default=os.path.join(ROOT, "docs", "project", "flaky_gates.tsv"))
    ap.add_argument("--summary", help="append a markdown summary here (e.g. $GITHUB_STEP_SUMMARY)")
    a = ap.parse_args(argv)
    if not re.fullmatch(r"\d+", a.run_id):
        print(f"triage: run id must be numeric, got {a.run_id!r}", file=sys.stderr)
        return 2
    repo = a.repo or default_repo()
    if not repo:
        print("triage: cannot tell the repository; pass --repo OWNER/NAME", file=sys.stderr)
        return 2
    ledger, problems = L.load_ledger(a.ledger)
    if problems:
        print(f"triage: the flake ledger {a.ledger} is malformed; refusing to classify against it:",
              file=sys.stderr)
        for p in problems:
            print(f"  {p}", file=sys.stderr)
        return 2
    try:
        run = L.run_record(a.run_id, repo)
    except L.GhError as e:
        print(f"triage: {e}", file=sys.stderr)
        return 2
    rid = run.get("databaseId", a.run_id)
    if run.get("status") != "completed":
        print(f"triage: run {rid} is {run.get('status')}; triage a completed run")
        return 3
    jobs = run.get("jobs") or []
    bad = [j for j in jobs if j.get("conclusion") in ("failure", "cancelled", "timed_out", "startup_failure")]
    if not bad:
        print(f"triage: run {rid} has no failed or cancelled job (conclusion {run.get('conclusion')})")
        return 0

    results, aggregates = [], []
    for j in bad:
        if L.is_aggregate(j.get("name", "")):
            aggregates.append(j)
            continue
        results.append(L.classify_job(j, L.job_log(j.get("databaseId"), repo), ledger))
    # The mutation aggregate fails whenever a shard did: it is classified by its shards when any
    # shard failed or was cancelled, and on its own log only when every shard succeeded.
    shard_res = [r for r in results if L.is_shard(r["job"])]
    for j in aggregates:
        if shard_res:
            worst = max(shard_res, key=lambda r: L.SEVERITY[r["class"]])
            results.append({"job": j.get("name"), "job_id": j.get("databaseId"), "step": "-",
                            "gate": "aggregate_mutation_shards.py", "command": None, "fail_lines": [],
                            "fail_total": 0, "repro": None, "ledger": None, "outcome": worst["outcome"],
                            "class": worst["class"],
                            "reason": f"derived from {len(shard_res)} failed or cancelled shard job(s)"})
        else:
            results.append(L.classify_job(j, L.job_log(j.get("databaseId"), repo), ledger))

    counts = {c: sum(1 for r in results if r["class"] == c) for c in L.SEVERITY}
    print(f"triage: run {rid} ({run.get('headBranch')} @ {str(run.get('headSha'))[:12]}, attempt "
          f"{run.get('attempt')}, {run.get('conclusion')}): " +
          ", ".join(f"{c}={n}" for c, n in counts.items()))
    for r in results:
        print(f"  {r['class']:<12} {r['outcome']:<10} {r['job']} :: {r['gate']} -- {r['reason']}")
    quarantined = [r for r in results if r["ledger"] and r["ledger"]["status"] == "quarantined"]
    if quarantined:
        print("  quarantined (reported separately; still a failure, never a pass):")
        for r in quarantined:
            print(f"    {r['gate']}: {r['outcome']} (quarantined until {r['ledger']['expires']}, "
                  f"owner {r['ledger']['owner']})")

    serious = counts["real"] + counts["infra-defect"]
    attempt = int(run.get("attempt") or 1)
    marker = os.path.join(a.out_dir, f"{rid}.rerun")
    action, escalated = "", False
    if serious:
        action = "report"
    elif run.get("conclusion") == "cancelled":
        action = "none: the run was cancelled as a whole (superseded or by hand); not rerun"
    elif attempt >= 2:
        action, escalated = "report", True
    elif os.path.exists(marker):
        action = f"none: a rerun was already requested ({marker})"
    elif a.no_rerun:
        action = "none: --no-rerun (would rerun the failed jobs once)"
    else:
        try:
            L.gh(["run", "rerun", str(rid), "--failed"], repo)
        except L.GhError as e:
            print(f"triage: rerun failed: {e}", file=sys.stderr)
            return 2
        os.makedirs(a.out_dir, exist_ok=True)
        with open(marker, "w") as fh:
            fh.write(f"run={rid}\nattempt={attempt}\n")
        action = "rerun: requested `gh run rerun --failed` (once; attempt 2 never reruns)"

    report = None
    if action == "report":
        path = os.path.join(a.out_dir, f"{rid}.md")
        report = write_report(path, run, repo, results, escalated, suspected_commits(run, repo))
        action = f"report: {os.path.relpath(path, os.getcwd()) if path.startswith(os.getcwd()) else path}"
    print(f"triage: action: {action}")
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as fh:
            fh.write(f"### Triage of run {rid}\n\n")
            fh.write("| job | gate | class | outcome |\n|---|---|---|---|\n")
            for r in results:
                q = " (quarantined)" if r in quarantined else ""
                fh.write(f"| {r['job']} | `{r['gate']}` | {r['class']}{q} | {r['outcome']} |\n")
            fh.write(f"\nAction: {action}\n\n")
            if report:
                fh.write(report + "\n")
    return 1 if action.startswith("report") else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
