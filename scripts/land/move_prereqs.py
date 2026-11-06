"""Landing prerequisites P2 and P4 of the legacy/ move (scripts/land/legacy_move.sh), judged on content.

  move_prereqs.py workflow FILE...   exit 0 when some FILE is a workflow with a schedule trigger and,
                                     in a job with no `if`, `needs` or continue-on-error, a step with
                                     no `if`, `shell` or continue-on-error, under a bash shell that
                                     fails on error, whose whole run is the single line
                                     `bash scripts/oracle/build.sh` (rebuild from the tag and verify)
  move_prereqs.py site FILE          exit 0 when FILE holds the website takedown record (R-0565,
                                     decision row P9): every key below with a real value, the
                                     decision is 'P9 (owner, DATE)', build says 'book.yml stops
                                     with the move', deployment is https URLs, unpublish is one
                                     'gh api -X DELETE repos/OWNER/REPO/pages' per URL (data, never
                                     run here), confirmation is the fixed owner-confirmation sentence,
                                     status is 'pending' or 'done DATE'; no placeholder words
  move_prereqs.py self-test          the negative controls; exit 0 only when every control holds
Problems are printed one per line. Needs PyYAML (the gates shell has it).
"""
import re
import sys

import yaml

SITE_KEYS = ["decision", "build", "deployment", "unpublish", "confirmation", "status"]
SHELLS = {"bash", "bash --noprofile --norc -eo pipefail {0}",
          "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"}
PLACEHOLDER = re.compile(r"^(|pending|todo|tbd|unknown|n/?a|none|-|\?+|later)$", re.I)


def workflow_ok(text):
    try:
        doc = yaml.safe_load(text)
    except yaml.YAMLError as e:
        return [f"not YAML: {e}"]
    if not isinstance(doc, dict):
        return ["not a workflow"]
    on = doc.get("on", doc.get(True))
    if not (isinstance(on, dict) and on.get("schedule")):
        return ["no schedule trigger"]
    # One narrow form, so nothing can hide it: an unconditional job with an unconditional step whose
    # whole run script is the single line `bash scripts/oracle/build.sh`.
    # The job has no `needs` (a skipped dependency would skip it), and the step's effective shell is
    # bash failing on error: GitHub's default, or one of the SHELLS below set as a workflow or job
    # default; a step may not set its own shell.
    def shell_ok(d):
        sh = (((d.get("defaults") or {}).get("run") or {}).get("shell")) if isinstance(d, dict) else None
        return sh is None or sh in SHELLS
    if not shell_ok(doc):
        return ["the workflow's default shell is not one that runs the script and fails on error"]
    for job in (doc.get("jobs") or {}).values():
        if (not isinstance(job, dict) or "if" in job or job.get("continue-on-error") or "needs" in job
                or not shell_ok(job)):
            continue
        for step in job.get("steps") or []:
            if (not isinstance(step, dict) or "if" in step or step.get("continue-on-error")
                    or "shell" in step):
                continue
            run = step.get("run")
            if isinstance(run, str) and run.strip() == "bash scripts/oracle/build.sh":
                return []
    return ["no unconditional step whose run is exactly `bash scripts/oracle/build.sh` in an unconditional job"]


SITE_PLACEHOLDER = re.compile(r"(?i)\b(todo|tbd|fixme|xxx|placeholder|pending|unknown)\b")
SITE_BUILD = re.compile(r"^book\.yml stops with the move\b")
SITE_URL = re.compile(r"^https://[a-z0-9.-]+\.[a-z]{2,}/[A-Za-z0-9._/~-]*$")
SITE_UNPUBLISH = re.compile(r"^gh api -X DELETE repos/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/pages$")
SITE_CONFIRM = re.compile(r"^the owner confirms the moment before the unpublish step runs; nothing runs it automatically$")
# After the takedown the record says when the owner confirmed it; a done record must say so.
SITE_CONFIRMED = re.compile(r"^(?!.*automatic)the owner confirmed the moment on \d{4}-\d{2}-\d{2}( \([^()]*\))?$")


def site_ok(text):
    """The takedown record: structured values only; the unpublish commands are data, never run."""
    kv = {}
    for line in text.splitlines():
        m = re.match(r"^([a-z_0-9]+):\s*(.*?)\s*$", line)
        if m and m.group(1) in SITE_KEYS:
            if m.group(1) in kv:
                return [f"{m.group(1)}: given twice"]
            kv[m.group(1)] = m.group(2)
    out = []
    for k in SITE_KEYS:
        v = kv.get(k, "")
        if k != "status" and (PLACEHOLDER.match(v) or SITE_PLACEHOLDER.search(v)):
            out.append(f"{k}: missing or placeholder")
    if out:
        return out
    if not re.fullmatch(r"P9 \(owner, \d{4}-\d{2}-\d{2}\)", kv["decision"]):
        out.append("decision: not 'P9 (owner, YYYY-MM-DD)'")
    if not SITE_BUILD.match(kv["build"]):
        out.append("build: does not say 'book.yml stops with the move'")
    urls = [u.strip() for u in re.split(r"\s+and\s+|,\s*", re.sub(r"\s*\(.*\)\s*$", "", kv["deployment"]))]
    if not urls or not all(SITE_URL.match(u) for u in urls):
        out.append("deployment: not a list of https URLs")
    cmds = [c.strip() for c in kv["unpublish"].split(";")]
    if not cmds or not all(SITE_UNPUBLISH.match(c) for c in cmds):
        out.append("unpublish: not a ';'-separated list of 'gh api -X DELETE repos/OWNER/REPO/pages'")
    elif len(cmds) != len(urls):
        out.append("unpublish: one command per deployment URL")
    status = kv.get("status", "")
    if not re.fullmatch(r"pending|done \d{4}-\d{2}-\d{2}", status):
        out.append("status: neither 'pending' nor 'done YYYY-MM-DD'")
    confirmed = bool(SITE_CONFIRMED.match(kv["confirmation"]))
    if status.startswith("done") and not confirmed:
        out.append("confirmation: a done takedown must record the date the owner confirmed the moment")
    elif not confirmed and not SITE_CONFIRM.match(kv["confirmation"]):
        out.append("confirmation: the owner must confirm the moment, and nothing may run it automatically")
    return out


GOOD_WF = """on:\n  schedule:\n    - cron: "0 3 * * 1"\njobs:\n  r:\n    runs-on: x\n    steps:\n      - run: bash scripts/oracle/build.sh\n"""
GOOD_SITE = ("decision: P9 (owner, 2026-11-05)\nbuild: book.yml stops with the move (archived)\n"
             "deployment: https://a.example.io/x/ and https://b.example.io/y/ (GitHub Pages)\n"
             "unpublish: gh api -X DELETE repos/a/x/pages; gh api -X DELETE repos/b/y/pages\n"
             "confirmation: the owner confirms the moment before the unpublish step runs; nothing runs it automatically\n"
             "status: pending\n")


def self_test():
    bad = []
    def expect(name, problems, want_ok):
        if (not problems) != want_ok:
            bad.append(f"control failed: {name}: {problems}")
    expect("good workflow", workflow_ok(GOOD_WF), True)
    expect("comment-only reference", workflow_ok(GOOD_WF.replace("bash scripts", "# bash scripts")), False)
    expect("echo-only step", workflow_ok(GOOD_WF.replace("bash scripts/oracle/build.sh", "echo scripts/oracle/build.sh")), False)
    expect("--record only", workflow_ok(GOOD_WF.replace("build.sh", "build.sh --record")), False)
    expect("unscheduled", workflow_ok(GOOD_WF.replace("schedule", "workflow_dispatch")), False)
    expect("step if: false", workflow_ok(GOOD_WF.replace("      - run:", "      - if: false\n        run:")), False)
    expect("job if: false", workflow_ok(GOOD_WF.replace("    runs-on: x", "    if: false\n    runs-on: x")), False)
    expect("step continue-on-error", workflow_ok(GOOD_WF.replace("      - run:", "      - continue-on-error: true\n        run:")), False)
    expect("job needs a skipped job", workflow_ok(GOOD_WF.replace("  r:\n", "  d:\n    if: false\n    runs-on: x\n    steps:\n      - run: true\n  r:\n    needs: d\n")), False)
    expect("step shell echo", workflow_ok(GOOD_WF.replace("      - run:", "      - shell: 'echo {0}'\n        run:")), False)
    expect("job default shell echo", workflow_ok(GOOD_WF.replace("    runs-on: x", "    defaults:\n      run:\n        shell: 'echo {0}'\n    runs-on: x")), False)
    expect("workflow default shell echo", workflow_ok("defaults:\n  run:\n    shell: 'echo {0}'\n" + GOOD_WF), False)
    expect("gates-shell default", workflow_ok('defaults:\n  run:\n    shell: "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"\n' + GOOD_WF), True)
    expect("heredoc-only", workflow_ok(GOOD_WF.replace("      - run: bash scripts/oracle/build.sh",
        "      - run: |\n          cat <<X\n          bash scripts/oracle/build.sh\n          X")), False)
    expect("good site record", site_ok(GOOD_SITE), True)
    CONF = "the owner confirms the moment before the unpublish step runs; nothing runs it automatically"
    DONE = GOOD_SITE.replace(CONF, "the owner confirmed the moment on 2026-11-05 (P9: taken down)").replace("status: pending", "status: done 2026-11-06")
    expect("done record", site_ok(DONE), True)
    expect("done without a recorded confirmation", site_ok(GOOD_SITE.replace("status: pending", "status: done 2026-11-06")), False)
    expect("confirmed without a date", site_ok(DONE.replace("on 2026-11-05 ", "")), False)
    expect("confirmed, then automatic", site_ok(DONE.replace("(P9: taken down)", "(P9) and the move runs it automatically")), False)
    expect("automatic inside the note", site_ok(DONE.replace("(P9: taken down)", "(P9: the move runs it automatically)")), False)
    expect("pending after confirmation", site_ok(DONE.replace("status: done 2026-11-06", "status: pending")), True)
    expect("placeholder unpublish", site_ok(GOOD_SITE.replace("gh api -X DELETE repos/a/x/pages; gh api -X DELETE repos/b/y/pages", "TODO")), False)
    expect("TODO command", site_ok(GOOD_SITE.replace("gh api -X DELETE repos/a/x/pages;", "TODO command;")), False)
    expect("TODO URL", site_ok(GOOD_SITE.replace("https://a.example.io/x/", "TODO URL")), False)
    expect("not a URL", site_ok(GOOD_SITE.replace("https://a.example.io/x/", "the site")), False)
    expect("other command", site_ok(GOOD_SITE.replace("gh api -X DELETE repos/a/x/pages", "rm -rf /")), False)
    expect("one command for two sites", site_ok(GOOD_SITE.replace("; gh api -X DELETE repos/b/y/pages", "")), False)
    expect("another decision", site_ok(GOOD_SITE.replace("decision: P9", "decision: P8")), False)
    expect("build keeps running", site_ok(GOOD_SITE.replace("book.yml stops with the move (archived)", "book.yml keeps running")), False)
    expect("no owner confirmation", site_ok(GOOD_SITE.replace("the owner confirms the moment before the unpublish step runs; nothing runs it automatically", "no owner confirmation needed")), False)
    expect("automatic unpublish", site_ok(GOOD_SITE.replace("nothing runs it automatically", "the move runs it automatically")), False)
    expect("bad status", site_ok(GOOD_SITE.replace("status: pending", "status: maybe")), False)
    expect("missing status", site_ok(GOOD_SITE.replace("status: pending\n", "")), False)
    expect("duplicate key", site_ok(GOOD_SITE + "build: book.yml keeps running\n"), False)
    expect("four words only", site_ok("P9 book.yml unpublish owner"), False)
    return bad


def main(argv):
    if argv[:1] == ["self-test"]:
        out = self_test()
    elif argv[:1] == ["workflow"] and len(argv) > 1:
        results = [workflow_ok(open(f).read()) for f in argv[1:]]
        out = [] if any(not r for r in results) else [p for r in results for p in r]
    elif argv[:1] == ["site"] and len(argv) == 2:
        out = site_ok(open(argv[1]).read())
    else:
        print(__doc__, file=sys.stderr)
        return 2
    for p in out:
        print(p)
    return 1 if out else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
