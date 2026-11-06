#!/usr/bin/env python3
"""Reconcile a sharded gate-mutation campaign into ONE verdict, failing closed.

WHY THIS EXISTS. The full campaign (95 families) does not fit GitHub's 6-hour job limit: dispatched
runs on 8187a2f7 and 245cd51c were cancelled at families 30 and 31. So CI runs it as N shards, and a
shard is a PARTIAL run that can never qualify by itself (mode=shard). This is the only producer of
the sharded campaign's completed/integrity_ok/qualified, and it is deliberately the narrowest reader
that could do the job: it never recomputes shard membership or digests, it COMPARES what each shard
ran against what the plan said — one inventory, computed once, by the driver's `--shard-plan`.

WHAT IS REFUSED, each by name, and every one of them means qualified=0:

  - a shard that is missing, cancelled, timed out, skipped, or left no summary or status;
  - a shard summary from another commit, inventory, driver or family set than the plan's;
  - a shard labelled as a different shard than the slot it was downloaded into;
  - a family in no shard, a family in two shards, a family the plan does not declare;
  - a record that disagrees with its summary, or names a different family, commit or run;
  - a shard that did not complete (every selected family reported) or whose integrity failed.

completed=1 means every shard is present, complete, of the plan's identity, and the shards'
selections partition the plan exactly. integrity_ok=1 means nothing at all was refused. qualified=1
additionally requires every discovered family KILLED or PENDING, zero invalid/survived/could-not-apply,
and a baseline in every shard whose gates are all green or red exactly as declared. The exit status
follows qualified: 0 only when qualified.

PENDING is the disposition of a family whose gate the driver declares EXPECTED-RED (`expect_red`) and
whose shard measured that gate red on exactly its declared legs. It is accepted only for a family
whose PLANNED gate appears in the plan's `expected_red_gate=` lines, whose record names that same
gate, and whose shard published that gate's baseline transcript red on exactly the plan's
`expected_red_leg=` lines with a completed verdict counting them; anywhere else it is refused. It is
never counted as killed, and the aggregate names every pending family (`pending_families=`).

Inputs:
    --plan PATH           output of `check_gate_mutation_coverage.sh --shard-plan <n>`
    --shards-dir DIR      DIR/mutation-shard-<k>/{summary.txt,status.txt,evidence/<family>/verdict.txt}
    --expect-head SHA     the commit the workflow checked out (github.sha)
    --out PATH            where the aggregate record is written (atomically)
"""
import argparse
import os
import re
import sys
import tempfile

DISPOSITIONS = ("killed", "invalid", "survived", "could_not_apply", "pending")
# The shard-summary keys this reader consults. Each must appear EXACTLY once: a duplicated key is two
# answers to one question, and whichever a reader happened to take would win silently.
SUMMARY_KEYS = (
    "completed", "mode", "discovered", "selected", "executed", "reported", "killed", "invalid",
    "survived", "could_not_apply", "pending", "integrity_ok", "qualified", "killed_by_gate",
    "killed_by_build", "baseline_gates_green", "baseline_gates_expected_red", "run_id", "head", "executed_driver_sha", "repo_driver_sha",
    "inventory_sha", "families_digest", "refusals", "supervisor_refusals", "supervisor_child_exit",
    "candidate_incoherent", "secs_total",
)
PLAN_KEYS = ("shard_plan", "shards", "discovered", "head", "inventory_sha", "repo_driver_sha",
             "families_digest")
SHARD_DIR = re.compile(r"^mutation-shard-([1-9][0-9]*)$")
NUM = re.compile(r"^(0|[1-9][0-9]{0,5})$")


def read_kv(path):
    """-> (dict key -> [values]) or None when unreadable. Duplicates are KEPT so they can be refused."""
    try:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
    except (OSError, UnicodeDecodeError):
        return None
    out = {}
    for line in text.splitlines():
        if "=" not in line:
            continue
        k, v = line.split("=", 1)
        out.setdefault(k, []).append(v)
    return out


def one(kv, key):
    vals = kv.get(key, [])
    return vals[0] if len(vals) == 1 else None


def num(v):
    return int(v) if v is not None and NUM.match(v) else None


def parse_plan(path, refuse):
    kv = read_kv(path)
    if kv is None:
        refuse("plan_unreadable")
        return None
    hdr = {}
    for k in PLAN_KEYS:
        v = one(kv, k)
        if v is None or v == "":
            refuse(f"plan_header({k})")
        hdr[k] = v
    rows, gates = {}, {}
    for raw in kv.get("family", []):
        # `family=<name> shard=<k> gate=<g> spec=<digest>`; the name is the first token.
        toks = raw.split(" ")
        fields = dict(t.split("=", 1) for t in toks[1:] if "=" in t)
        name, k = toks[0], num(fields.get("shard"))
        if not name or k is None or not fields.get("gate"):
            refuse(f"plan_row_malformed({raw[:60]})")
            continue
        if name in rows:
            refuse(f"plan_duplicate_family({name})")
            continue
        rows[name] = k
        gates[name] = fields["gate"]
    n, d = num(hdr.get("shards")), num(hdr.get("discovered"))
    if n is None or n < 1:
        refuse(f"plan_shards({hdr.get('shards')})")
        return None
    if d is None or d < 1 or d != len(rows):
        refuse(f"plan_population({hdr.get('discovered')} declared vs {len(rows)} rows)")
    for name, k in rows.items():
        if not 1 <= k <= n:
            refuse(f"plan_row_out_of_range({name}:{k}/{n})")
    # THE DECLARED EXPECTED-RED GATES, as the driver's plan states them. A declaration naming no
    # planned gate is malformed: it could only ever license a pending record for a family that does
    # not exist.
    expected_red = {}
    for g in kv.get("expected_red_gate", []):
        if not g or g in expected_red or g not in set(gates.values()):
            refuse(f"plan_expected_red_malformed({g or '<empty>'})")
            continue
        expected_red[g] = []
    for raw in kv.get("expected_red_leg", []):
        g, _, leg = raw.partition(" ")
        if g not in expected_red or not leg or leg in expected_red[g]:
            refuse(f"plan_expected_red_leg_malformed({raw[:60]})")
            continue
        expected_red[g].append(leg)
    for g, legs in expected_red.items():
        if not legs:
            refuse(f"plan_expected_red_without_legs({g})")
    return hdr, rows, n, gates, expected_red


VERDICT_TAIL = re.compile(r"(^|[^A-Za-z0-9_])FAIL=([0-9]+)$")


def expected_red_transcript_ok(path, legs):
    """The shard's baseline transcript for an expected-red gate: red on EXACTLY the declared `  FAIL `
    legs, ending in a completed verdict line counting that many failures. A gate that printed the
    expected failure and then crashed never reaches that line. Mirrors campaign_supervise.sh's
    expected_red_transcript_ok, the rule the driver applied."""
    try:
        with open(path, encoding="utf-8") as fh:
            lines = fh.read().splitlines()
    except (OSError, UnicodeDecodeError):
        return False
    got = sorted(ln[len("  FAIL "):] for ln in lines if ln.startswith("  FAIL "))
    if not legs or got != sorted(legs):
        return False
    tail = [ln for ln in lines if ln.split()]
    m = VERDICT_TAIL.search(tail[-1]) if tail else None
    return bool(m) and int(m.group(2)) == len(legs)


def read_records(evdir, refuse, k, expect_head, run_id, rec_gates=None):
    """-> {family: disposition} for one shard's evidence; refusals for every malformed record.
    When `rec_gates` is a dict it is filled with each record's own `gate=` (or None)."""
    recs = {}
    if not os.path.isdir(evdir):
        return recs
    for name in sorted(os.listdir(evdir)):
        if name.startswith("_") or not os.path.isdir(os.path.join(evdir, name)):
            continue  # `_baseline` is run-level evidence, never a family
        kv = read_kv(os.path.join(evdir, name, "verdict.txt"))
        if kv is None:
            refuse(f"record_unreadable({k}:{name})")
            continue
        if one(kv, "family") != name:
            refuse(f"record_names_other_family({k}:{name}->{one(kv, 'family')})")
        if one(kv, "head") != expect_head:
            refuse(f"record_foreign_head({k}:{name})")
        if one(kv, "run_id") != run_id:
            refuse(f"record_foreign_run({k}:{name})")
        disp = one(kv, "disposition")
        if disp not in DISPOSITIONS:
            refuse(f"record_disposition({k}:{name}:{disp})")
            continue
        recs[name] = disp
        if rec_gates is not None:
            rec_gates[name] = one(kv, "gate")
    return recs


def aggregate(plan_path, shards_dir, expect_head):
    """-> (fields dict, completion refusals, integrity refusals). Pure apart from reading inputs."""
    incomplete, integrity = [], []
    planned = parse_plan(plan_path, integrity.append)
    fields = {"mode": "campaign-sharded", "head": expect_head}
    if planned is None:
        return fields, ["plan_unusable"], integrity
    hdr, rows, n, gates, expected_red = planned
    fields.update(shards=str(n), discovered=str(len(rows)), inventory_sha=hdr["inventory_sha"],
                  families_digest=hdr["families_digest"])
    if hdr["head"] != expect_head:
        incomplete.append(f"plan_foreign_head({hdr['head']} vs {expect_head})")

    # SLOTS THAT ARE NOT THE PLAN'S. An extra shard is not harmless surplus: it is evidence about a
    # split nobody planned, and silently ignoring it would let a stale artifact sit beside the set.
    present = {}
    try:
        entries = sorted(os.listdir(shards_dir))
    except OSError:
        entries = []
        incomplete.append("shards_dir_unreadable")
    for e in entries:
        m = SHARD_DIR.match(e)
        if not m or not 1 <= int(m.group(1)) <= n:
            integrity.append(f"unexpected_shard_artifact({e})")
            continue
        present[int(m.group(1))] = os.path.join(shards_dir, e)

    owner = {}  # family -> shard that reported it
    pending_names = []
    totals = dict.fromkeys(DISPOSITIONS + ("killed_by_gate", "killed_by_build", "executed",
                                           "reported", "selected", "baseline_gates_expected_red"), 0)
    baseline_ok = True
    driver_seen = set()
    for k in range(1, n + 1):
        expected = {f for f, s in rows.items() if s == k}
        path = present.get(k)
        if path is None:
            incomplete.append(f"shard_missing({k})")
            continue
        st = read_kv(os.path.join(path, "status.txt"))
        status = one(st, "job_status") if st else None
        if st is None or one(st, "shard") != f"{k}/{n}":
            incomplete.append(f"shard_status_missing_or_mislabelled({k})")
        elif status not in ("success", "failure"):
            # cancelled, timed out (GitHub reports a timeout as cancelled), skipped, or anything else:
            # the shard did not finish, whatever its summary file says.
            incomplete.append(f"shard_job_{status or 'unknown'}({k})")
        sm = read_kv(os.path.join(path, "summary.txt"))
        if sm is None:
            incomplete.append(f"shard_summary_missing({k})")
            continue
        bad_keys = [key for key in SUMMARY_KEYS if len(sm.get(key, [])) != 1]
        if bad_keys:
            incomplete.append(f"shard_summary_keys({k}:{','.join(bad_keys)})")
            continue
        g = {key: one(sm, key) for key in SUMMARY_KEYS}
        if g["mode"] != "shard":
            incomplete.append(f"shard_mode({k}:{g['mode']})")
        # THE SHARD NAMES ITSELF in its scope note; a record filed under another slot is refused.
        notes = g["refusals"].split()
        label = f"shard_selected({k}/{n})"
        if notes.count(label) != 1 or any(t.startswith("shard_selected(") and t != label for t in notes):
            incomplete.append(f"shard_label_mismatch({k}:{g['refusals'].strip() or 'none'})")
        others = [t for t in notes if not t.startswith("shard_selected(")]
        if others:
            integrity.append(f"shard_refusals({k}:{' '.join(others)})")
        # IDENTITY: the plan's commit, inventory, driver and family set, or the shard is foreign.
        for key, want in (("head", expect_head), ("inventory_sha", hdr["inventory_sha"]),
                          ("repo_driver_sha", hdr["repo_driver_sha"]),
                          ("families_digest", hdr["families_digest"])):
            if g[key] != want:
                incomplete.append(f"shard_foreign_{key}({k}:{g[key]} vs {want})")
        driver_seen.add(g["executed_driver_sha"])
        if g["discovered"] != str(len(rows)):
            incomplete.append(f"shard_discovered({k}:{g['discovered']} vs {len(rows)})")
        if g["completed"] != "1":
            incomplete.append(f"shard_incomplete({k})")
        if g["integrity_ok"] != "1":
            integrity.append(f"shard_integrity({k})")
        if g["candidate_incoherent"] != "none":
            integrity.append(f"shard_candidate_incoherent({k}:{g['candidate_incoherent']})")

        counts = {key: num(g[key]) for key in DISPOSITIONS + ("selected", "executed", "reported",
                                                              "killed_by_gate", "killed_by_build",
                                                              "baseline_gates_expected_red")}
        if None in counts.values():
            integrity.append(f"shard_nonnumeric_counts({k})")
            continue
        unkilled = counts["invalid"] + counts["survived"] + counts["could_not_apply"]
        # A NONZERO CHILD EXIT IS ACCEPTABLE ONLY WHEN THE RECORD EXPLAINS IT. A shard with a survivor
        # exits nonzero by design and is still a complete report; any other supervisor refusal is not.
        sup = g["supervisor_refusals"].split()
        if g["supervisor_refusals"] != "none":
            unexplained = [t for t in sup if not (t.startswith("child_exit(") and unkilled > 0)]
            if unexplained:
                integrity.append(f"shard_supervisor_refused({k}:{' '.join(unexplained)})")
        if g["supervisor_child_exit"] != "0" and unkilled == 0:
            integrity.append(f"shard_unexplained_exit({k}:{g['supervisor_child_exit']})")

        rec_gates = {}
        recs = read_records(os.path.join(path, "evidence"), integrity.append, k, expect_head,
                            g["run_id"], rec_gates)
        got = set(recs)
        if expected - got:
            incomplete.append(f"shard_missing_families({k}:{','.join(sorted(expected - got))})")
        if got - expected:
            integrity.append(f"shard_unexpected_families({k}:{','.join(sorted(got - expected))})")
        # A PENDING RECORD IS ONLY A PENDING RECORD ON A DECLARED EXPECTED-RED GATE. Anywhere else it is
        # a family that went unjudged under a label that excuses it.
        checked = {}
        for f, d in sorted(recs.items()):
            if d != "pending":
                continue
            pg = gates.get(f)
            if pg not in expected_red:
                integrity.append(f"pending_without_declaration({k}:{f}:{pg})")
                continue
            if rec_gates.get(f) != pg:
                integrity.append(f"pending_record_gate_mismatch({k}:{f}:{rec_gates.get(f)} vs {pg})")
                continue
            if pg not in checked:
                checked[pg] = expected_red_transcript_ok(
                    os.path.join(path, "evidence", "_baseline", pg + ".log"), expected_red[pg])
            if not checked[pg]:
                integrity.append(f"pending_without_matching_baseline({k}:{f}:{pg})")
                continue
            pending_names.append(f)
        # THE EXPECTED-RED COUNT IS RECONCILED, NOT TRUSTED. It must be exactly the number of distinct
        # declared gates this shard backed with validated pending evidence — a larger claim would let
        # a red gate pose as an expected-red one — and every family the shard ran on such a gate must
        # be pending, because an expected-red baseline is what made them so.
        backed = sorted(g2 for g2, good in checked.items() if good)
        if counts["baseline_gates_expected_red"] != len(backed):
            integrity.append(f"shard_expected_red_unbacked({k}:claimed={counts['baseline_gates_expected_red']} "
                             f"backed={len(backed)})")
        for f in sorted(expected):
            if gates.get(f) in backed and f in recs and recs[f] != "pending":
                integrity.append(f"shard_expected_red_gate_family_not_pending({k}:{f}:{recs[f]})")
        for f in got:
            if f in owner:
                incomplete.append(f"duplicate_family({f}:{owner[f]},{k})")
            else:
                owner[f] = k
        # THE RECORDS ARE THE EVIDENCE; THE SUMMARY IS A CLAIM ABOUT THEM.
        for d in DISPOSITIONS:
            have = sum(1 for v in recs.values() if v == d)
            if have != counts[d]:
                integrity.append(f"shard_record_disagreement({k}:{d} claimed={counts[d]} records={have})")
        for key in ("selected", "executed", "reported"):
            if counts[key] != len(expected):
                incomplete.append(f"shard_{key}({k}:{counts[key]} vs planned {len(expected)})")
        if counts["killed_by_gate"] + counts["killed_by_build"] != counts["killed"]:
            integrity.append(f"shard_kill_split({k})")
        # GREEN PLUS EXPECTED-RED MUST BE THE WHOLE BASELINE, and an expected-red gate is admissible
        # only with the pending families it exists for (and vice versa).
        # AND THE DENOMINATOR IS THE PLAN'S: the distinct gates of the families this shard owns. A
        # shard reporting fewer could leave an ordinary gate out of its baseline entirely while the
        # green + expected-red identity still held over what remained.
        m = re.match(r"^(\d+)/(\d+)$", g["baseline_gates_green"])
        xr = counts["baseline_gates_expected_red"]
        planned_gates = len({gates[f] for f in expected})
        if (not m or int(m.group(1)) + xr != int(m.group(2))
                or (expected and m.group(2) == "0")):
            baseline_ok = False
        if m and int(m.group(2)) != planned_gates:
            baseline_ok = False
            integrity.append(f"shard_baseline_population({k}:{m.group(2)} vs {planned_gates} planned gates)")
        if (xr > 0) != (counts["pending"] > 0):
            integrity.append(f"shard_pending_without_expected_red_baseline({k}:pending={counts['pending']} "
                             f"expected_red={xr})")
        for key in totals:
            totals[key] += counts[key]

    if len(driver_seen) > 1:
        incomplete.append(f"shard_driver_identity_differs({','.join(sorted(driver_seen))})")
    # THE PARTITION, CHECKED GLOBALLY AS WELL, so a family in no shard is named even when the shard
    # that should have held it is the one that is missing.
    nowhere = sorted(set(rows) - set(owner))
    if nowhere:
        incomplete.append(f"families_in_no_shard({','.join(nowhere)})")
    fields.update({key: str(v) for key, v in totals.items()})
    fields["pending_families"] = ",".join(sorted(pending_names)) or "none"
    fields["baseline_all_green"] = "1" if baseline_ok else "0"
    return fields, incomplete, integrity


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument("--plan", required=True)
    ap.add_argument("--shards-dir", required=True)
    ap.add_argument("--expect-head", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args(argv)
    if not re.match(r"^[0-9a-f]{40}$", a.expect_head):
        print(f"error: --expect-head is not a commit sha: {a.expect_head!r}", file=sys.stderr)
        return 2
    fields, incomplete, integrity = aggregate(a.plan, a.shards_dir, a.expect_head)
    completed = int(not incomplete and not integrity_identity_failures(integrity))
    integrity_ok = int(not incomplete and not integrity)
    d = num(fields.get("discovered"))
    qualified = int(bool(
        completed and integrity_ok and d
        and num(fields.get("killed")) is not None and num(fields.get("pending")) is not None
        and num(fields["killed"]) + num(fields["pending"]) == d and fields.get("reported") == str(d)
        and all(fields.get(x) == "0" for x in ("invalid", "survived", "could_not_apply"))
        and fields.get("baseline_all_green") == "1"))
    fields.update(completed=str(completed), integrity_ok=str(integrity_ok),
                  qualified=str(qualified), refusals=" ".join(incomplete + integrity) or "none")
    order = ("completed", "mode", "shards", "discovered", "selected", "executed", "reported",
             "killed", "invalid", "survived", "could_not_apply", "pending", "pending_families",
             "integrity_ok", "qualified",
             "killed_by_gate", "killed_by_build", "baseline_all_green", "head", "inventory_sha",
             "families_digest", "refusals")
    body = "".join(f"{key}={fields.get(key, 'unavailable')}\n" for key in order)
    # ATOMIC, beside the target, so a reader never sees a half-written verdict.
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(os.path.abspath(a.out)) or ".", prefix=".agg.")
    with os.fdopen(fd, "w", encoding="utf-8") as fh:
        fh.write(body)
    os.replace(tmp, a.out)
    word = "QUALIFIED" if qualified else ("COMPLETE-NOT-QUALIFIED" if completed else "REFUSED")
    print(f"GATE-MUTATION-COVERAGE (sharded): {word} completed={completed} "
          f"integrity_ok={integrity_ok} qualified={qualified} "
          f"killed={fields.get('killed', 0)} invalid={fields.get('invalid', 0)} "
          f"survived={fields.get('survived', 0)} could_not_apply={fields.get('could_not_apply', 0)} "
          f"pending={fields.get('pending', 0)} "
          f"(of {fields.get('discovered', '?')}, {fields.get('shards', '?')} shards)")
    if fields.get("pending_families", "none") != "none":
        print(f"  pending (declared expected-red gates, not judged): {fields['pending_families']}")
    if fields["refusals"] != "none":
        print(f"  refusals: {fields['refusals']}")
    return 0 if qualified else 1


def integrity_identity_failures(integrity):
    """Integrity refusals that also deny COMPLETION: a record that cannot be attributed to this run
    (other family, commit or run) or an unreadable record is not a report of the selected family."""
    return [r for r in integrity
            if r.startswith(("record_unreadable", "record_names_other_family", "record_foreign_",
                             "record_disposition", "plan_"))]


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
