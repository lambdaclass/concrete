#!/usr/bin/env python3
"""Compiler profiles from leanprof reports: per-stage time, hot spots, comparison (R-0551).

Driven by scripts/perf/leanprof_profile.sh, which runs leanprof on the fixed workloads; this file
only reads what leanprof wrote. Subcommands:

  validate REPORT                      exit 0 if REPORT is a leanprof report this file can read
  summarize --workload W --mode M [--top N] REPORT...
                                       one workload's profile (median over the repetitions) as JSON
  assemble --meta META.json --out PROFILE.json WORKLOAD.json...
                                       the profile of one commit (schema PROFILE_SCHEMA)
  compare BASE.json HEAD.json [--json OUT] [--md OUT] [threshold flags]
                                       findings: a hot spot or stage that grew, or a new hot spot;
                                       exit 1 when there is a finding, 2 when the two cannot be
                                       compared (different mode or schema)
  render PROFILE.json [--comparison CMP.json]
                                       the markdown summary
  gen-corpus N                         the built-in scaling program (until R-0550's corpus lands)

THE INPUT is leanprof's report, version 3, the same shape for `leanprof sample` (sampled CPU per
Lean declaration; `sampled: true`, calls 0) and `leanprof run --auto` (instrumented: exact calls,
times include what the probes could not subtract): {wall_ns, scopes: [{name, pos, total_ns,
self_ns, calls}]}. Shares are of wall_ns, the run's total as leanprof reports it.

SHARES, NOT MILLISECONDS, decide a finding. A uniform slowdown (a slower host, a lower clock) moves
every scope by the same factor and leaves the shares where they were; the instruction-count
tracker (docs/project/PERF_TRACKING.md) is the alarm for "this commit is slower". A share that
grows says WHERE: one stage or function now takes a larger part of the same build. Milliseconds
are reported, never compared.
"""
import argparse
import json
import os
import statistics
import sys

PROFILE_SCHEMA = "concrete-leanprof-profile/1"
COMPARISON_SCHEMA = "concrete-leanprof-comparison/1"
MODES = ("sample", "auto")

# The recorded thresholds (R-0551: "above a recorded threshold"), in percentage points of the run.
# Sampling at 2 kHz over a ~0.5 s build gives about 1,000 samples per repetition; a scope at 5% of
# them has a standard error of about 0.7 points, and the profile takes the median of 3
# repetitions. A finding needs BOTH the absolute and the relative growth, so a 0.3% scope that
# doubles is not a finding and a 30% scope that moves by 2 points is not either.
THRESHOLDS = {
    "grow_pts": 2.0,     # a hot spot or stage grew by at least this many points ...
    "grow_ratio": 1.3,   # ... and to at least this multiple of its base share
    "new_pts": 3.0,      # a hot spot at least this large ...
    "new_base_max": 0.75,  # ... whose base share was below this (absent counts as 0) is new
}
# Scopes kept for the "was it there before" lookup: everything at or above this share.
KEEP_ALL_PCT = 0.05

# Compiler stages: the first declaration of each list found in the report gives the stage's
# inclusive time. The lists are ordered by preference so that a rename or an inlined wrapper falls
# back to the next spelling; the declaration used is recorded, and two profiles that timed a stage
# through different declarations do not compare that stage. `load-project` ENCLOSES the front end
# of `concrete build` (parse through proof extraction), so the stages are not additive.
STAGES = [
    ("load-project", ["Concrete.loadProject"]),
    ("parse", ["Concrete.parse", "Concrete.Pipeline.parse"]),
    ("resolve-files", ["Concrete.resolveAllModules", "Concrete.Pipeline.resolveFiles"]),
    ("resolve", ["Concrete.Pipeline.resolve"]),
    ("check", ["Concrete.checkProgram", "Concrete.Pipeline.check"]),
    ("elaborate", ["Concrete.Pipeline.elaborate", "Concrete.elabProgram"]),
    ("core-check", ["Concrete.Pipeline.coreCheck", "Concrete.coreCheckProgram"]),
    ("proof-core", ["Concrete.extractProofCore?", "Concrete.extractProofCore"]),
    ("monomorphize", ["Concrete.Pipeline.monomorphize"]),
    ("lower", ["Concrete.Pipeline.lower"]),
    ("emit", ["Concrete.Pipeline.emit", "Concrete.emitSSAProgram"]),
]


class ReportError(Exception):
    pass


def die(msg, code=2):
    print("leanprof_profile: " + msg, file=sys.stderr)
    sys.exit(code)


def load_json(path):
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError) as e:
        raise ReportError("%s: %s" % (path, e))


def check_report(rep, path="report"):
    """Raise ReportError unless rep is a leanprof v3 report with a positive total and scopes."""
    if not isinstance(rep, dict):
        raise ReportError("%s: not a JSON object" % path)
    if rep.get("version") != 3:
        raise ReportError("%s: leanprof report version %r (this reader knows 3)" % (path, rep.get("version")))
    wall = rep.get("wall_ns")
    if not isinstance(wall, (int, float)) or isinstance(wall, bool) or wall <= 0:
        raise ReportError("%s: wall_ns must be a positive number" % path)
    scopes = rep.get("scopes")
    if not isinstance(scopes, list) or not scopes:
        raise ReportError("%s: no scopes (was leanprof run from the compiler's source tree? "
                          "sample keeps only declarations it can place)" % path)
    for i, s in enumerate(scopes):
        if not isinstance(s, dict) or not isinstance(s.get("name"), str) or not s["name"]:
            raise ReportError("%s: scope %d has no name" % (path, i))
        for k in ("total_ns", "self_ns"):
            v = s.get(k)
            if not isinstance(v, (int, float)) or isinstance(v, bool) or v < 0:
                raise ReportError("%s: scope %r: %s must be a number >= 0" % (path, s["name"], k))
        if s["self_ns"] > s["total_ns"] * 1.000001 + 1:
            raise ReportError("%s: scope %r: self_ns above total_ns" % (path, s["name"]))
    return rep


def report_mode(rep):
    return "sample" if rep.get("sampled") is True else "auto"


def per_name(rep):
    """name -> (total_ns, self_ns, calls, pos), summed over duplicate rows of one name."""
    out = {}
    for s in rep["scopes"]:
        t, sf, c, p = out.get(s["name"], (0.0, 0.0, 0, s.get("pos", "")))
        out[s["name"]] = (t + s["total_ns"], sf + s["self_ns"], c + int(s.get("calls") or 0), p)
    return out


def r2(x):
    return round(x, 2)


def summarize(workload, mode, paths, top):
    reps = []
    for p in paths:
        rep = check_report(load_json(p), p)
        if report_mode(rep) != mode:
            raise ReportError("%s: a %s report in a %s profile" % (p, report_mode(rep), mode))
        reps.append(rep)
    if not reps:
        raise ReportError("no reports")
    tables = [per_name(r) for r in reps]
    walls = [r["wall_ns"] for r in reps]
    med = statistics.median

    def med_share(name, idx):
        return med([(t.get(name, (0, 0, 0, ""))[idx] / w) * 100 for t, w in zip(tables, walls)])

    def med_ns(name, idx):
        return med([t.get(name, (0, 0, 0, ""))[idx] for t in tables])

    names = set()
    for t in tables:
        names.update(t)
    pos = {}
    for t in tables:
        for n, v in t.items():
            pos.setdefault(n, v[3])
    self_pct = {n: med_share(n, 1) for n in names}
    ranked = sorted(names, key=lambda n: (-self_pct[n], n))
    hotspots = []
    for n in ranked[:top]:
        hotspots.append({"name": n, "pos": pos[n], "self_pct": r2(self_pct[n]),
                         "self_ms": r2(med_ns(n, 1) / 1e6), "total_pct": r2(med_share(n, 0)),
                         "calls": int(med_ns(n, 2))})
    stages = []
    for stage, decls in STAGES:
        decl = next((d for d in decls if all(d in t for t in tables)), None)
        if decl is None:
            stages.append({"stage": stage, "decl": None, "pct": None, "ms": None})
        else:
            stages.append({"stage": stage, "decl": decl, "pct": r2(med_share(decl, 0)),
                           "ms": r2(med_ns(decl, 0) / 1e6)})
    return {
        "name": workload, "status": "ok", "reps": len(reps),
        "wall_ms": r2(med(walls) / 1e6), "wall_ms_reps": [r2(w / 1e6) for w in walls],
        "stages": stages, "hotspots": hotspots,
        "self_pct_all": {n: r2(self_pct[n]) for n in ranked if self_pct[n] >= KEEP_ALL_PCT},
    }


def check_profile(p, path):
    if not isinstance(p, dict) or p.get("schema") != PROFILE_SCHEMA:
        raise ReportError("%s: not a %s profile" % (path, PROFILE_SCHEMA))
    if p.get("mode") not in MODES:
        raise ReportError("%s: unknown mode %r" % (path, p.get("mode")))
    if not isinstance(p.get("workloads"), list):
        raise ReportError("%s: no workloads" % path)
    for w in p["workloads"]:
        if not isinstance(w, dict) or not w.get("name") or w.get("status") not in ("ok", "failed", "incomplete"):
            raise ReportError("%s: malformed workload %r" % (path, w))
    return p


EPS = 1e-6  # shares are rounded to hundredths; 2.1 -> 4.1 is +2.0 points, not 1.9999999999999996


def grew(base, head, t):
    return head - base >= t["grow_pts"] - EPS and head >= base * t["grow_ratio"] - EPS


def compare(base, head, t):
    findings, notes = [], []
    bw = {w["name"]: w for w in base["workloads"]}
    for w in head["workloads"]:
        b = bw.get(w["name"])
        if b is None:
            notes.append({"workload": w["name"], "note": "no baseline for this workload"})
            continue
        if w["status"] != "ok" or b["status"] != "ok":
            notes.append({"workload": w["name"],
                          "note": "not compared: base %s, head %s" % (b["status"], w["status"])})
            continue
        ball = b.get("self_pct_all", {})
        for h in w["hotspots"]:
            bp = ball.get(h["name"], 0.0)
            if h["name"] in ball and grew(bp, h["self_pct"], t):
                findings.append({"workload": w["name"], "kind": "grown", "name": h["name"],
                                 "pos": h.get("pos", ""), "base_pct": bp, "head_pct": h["self_pct"]})
            elif h["self_pct"] >= t["new_pts"] - EPS and bp < t["new_base_max"]:
                findings.append({"workload": w["name"], "kind": "new", "name": h["name"],
                                 "pos": h.get("pos", ""), "base_pct": bp, "head_pct": h["self_pct"]})
        bst = {s["stage"]: s for s in b["stages"]}
        for s in w["stages"]:
            o = bst.get(s["stage"])
            if o is None or s["pct"] is None or o["pct"] is None:
                continue
            if s["decl"] != o["decl"]:
                notes.append({"workload": w["name"], "note": "stage %s timed through %s, base through %s; not compared"
                              % (s["stage"], s["decl"], o["decl"])})
                continue
            if grew(o["pct"], s["pct"], t):
                findings.append({"workload": w["name"], "kind": "stage-grown", "name": s["stage"],
                                 "pos": s["decl"], "base_pct": o["pct"], "head_pct": s["pct"]})
    hn = {w["name"] for w in head["workloads"]}
    for n in bw:
        if n not in hn:
            notes.append({"workload": n, "note": "in the base profile, not profiled at head"})
    return {"schema": COMPARISON_SCHEMA, "base_sha": base.get("sha"), "head_sha": head.get("sha"),
            "mode": head["mode"], "thresholds": t, "findings": findings, "notes": notes}


def render(p, cmp=None):
    L = ["# Compiler profile %s" % (p.get("sha") or "")[:12], ""]
    L.append("leanprof %s (%s mode%s), %s, %s. Shares are of each build's total; stages nest "
             "(`load-project` holds the front end)." % (
                 p.get("leanprof", "?"), p["mode"],
                 ", %s reps at %s Hz" % (p.get("reps"), p.get("rate")) if p["mode"] == "sample" else "",
                 p.get("machine", "?"), p.get("date", "?")))
    if cmp is not None:
        L.append("")
        fs = cmp["findings"]
        if fs:
            L.append("**%d finding(s)** against %s:" % (len(fs), (cmp.get("base_sha") or "?")[:12]))
            L.append("")
            L.append("| workload | kind | what | base % | head % |")
            L.append("|---|---|---|---:|---:|")
            for f in fs:
                L.append("| %s | %s | `%s` %s | %.2f | %.2f |" % (
                    f["workload"], f["kind"], f["name"], f.get("pos", ""), f["base_pct"], f["head_pct"]))
        else:
            L.append("No finding against %s (thresholds: +%.1f points and x%.2f to grow; %.1f%% to be new)." % (
                (cmp.get("base_sha") or "?")[:12], cmp["thresholds"]["grow_pts"],
                cmp["thresholds"]["grow_ratio"], cmp["thresholds"]["new_pts"]))
        for n in cmp["notes"]:
            L.append("- %s: %s" % (n["workload"], n["note"]))
    for w in p["workloads"]:
        L.append("")
        if w["status"] != "ok":
            L.append("## %s: %s" % (w["name"], w["status"].upper()))
            if w.get("detail"):
                L.append("")
                L.append(w["detail"])
            continue
        L.append("## %s (%.1f ms, median of %d)" % (w["name"], w["wall_ms"], w["reps"]))
        L.append("")
        L.append("| stage | ms | % | declaration |")
        L.append("|---|---:|---:|---|")
        for s in w["stages"]:
            if s["pct"] is None:
                L.append("| %s | - | - | not seen |" % s["stage"])
            else:
                L.append("| %s | %.1f | %.1f | `%s` |" % (s["stage"], s["ms"], s["pct"], s["decl"]))
        L.append("")
        L.append("| self % | self ms | function | where |")
        L.append("|---:|---:|---|---|")
        for h in w["hotspots"][:10]:
            L.append("| %.1f | %.1f | `%s` | %s |" % (h["self_pct"], h["self_ms"], h["name"], h.get("pos", "")))
    return "\n".join(L) + "\n"


def gen_corpus(n):
    """One module with the R-0550 shapes at size n: many functions and declarations, a long
    function of chained lets, a long array literal and deep nesting. Exits 42."""
    if n < 1:
        raise ValueError("size must be >= 1")
    nf, ns, depth = max(1, n // 4), max(1, n // 8), max(1, min(n // 10, 200))
    L = ["fn f%d(x: i64) -> i64 { return x + %d; }" % (i, i % 3) for i in range(nf)]
    L += ["struct S%d { a: i64, b: i64 }" % i for i in range(ns)]
    L += ["fn long_fn() -> i64 {", "    let s0: i64 = 0;"]
    L += ["    let s%d: i64 = s%d + 1;" % (i, i - 1) for i in range(1, n + 1)]
    L += ["    return s%d - %d;" % (n, n), "}"]
    L += ["fn array_lit() -> i64 {",
          "    let a: [i64; %d] = [%s];" % (n, ", ".join(str(i % 10) for i in range(n))),
          "    return a[%d] - %d;" % (n - 1, (n - 1) % 10), "}"]
    L += ["fn nested() -> i64 {", "    let mut s: i64 = 0;", "    let t: i64 = 1;",
          "    " + "if t > 0 { s = s + 1; " * depth + "}" * depth, "    return s - %d;" % depth, "}"]
    L += ["fn main() -> Int {",
          "    let st: S%d = S%d { a: 40, b: 2 };" % (ns - 1, ns - 1),
          "    let S%d { a, b } = st;" % (ns - 1),
          "    return (a + b + long_fn() + array_lit() + nested() + f0(0) - f%d(0) + %d) as Int;"
          % (nf - 1, (nf - 1) % 3), "}"]
    return "\n".join(L) + "\n"


def write_json(path, obj):
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        json.dump(obj, f, indent=1, sort_keys=True)
        f.write("\n")
    os.replace(tmp, path)


def thresholds_from(a):
    t = dict(THRESHOLDS)
    for k in THRESHOLDS:
        v = getattr(a, k, None)
        if v is not None:
            if v < 0:
                die("--%s must be >= 0" % k.replace("_", "-"))
            t[k] = v
    if t["grow_ratio"] < 1:
        die("--grow-ratio must be >= 1")
    return t


def main(argv):
    ap = argparse.ArgumentParser(prog="leanprof_profile.py")
    sub = ap.add_subparsers(dest="cmd", required=True)
    v = sub.add_parser("validate"); v.add_argument("report")
    s = sub.add_parser("summarize")
    s.add_argument("--workload", required=True); s.add_argument("--mode", required=True, choices=MODES)
    s.add_argument("--top", type=int, default=15); s.add_argument("reports", nargs="+")
    a = sub.add_parser("assemble")
    a.add_argument("--meta", required=True); a.add_argument("--out", required=True)
    a.add_argument("workloads", nargs="*")
    c = sub.add_parser("compare")
    c.add_argument("base"); c.add_argument("head")
    c.add_argument("--json"); c.add_argument("--md")
    for k in THRESHOLDS:
        c.add_argument("--" + k.replace("_", "-"), dest=k, type=float)
    r = sub.add_parser("render"); r.add_argument("profile"); r.add_argument("--comparison")
    g = sub.add_parser("gen-corpus"); g.add_argument("size", type=int)
    o = ap.parse_args(argv)
    try:
        if o.cmd == "validate":
            check_report(load_json(o.report), o.report)
            return 0
        if o.cmd == "summarize":
            if o.top < 1:
                die("--top must be >= 1")
            json.dump(summarize(o.workload, o.mode, o.reports, o.top), sys.stdout, indent=1, sort_keys=True)
            print()
            return 0
        if o.cmd == "assemble":
            meta = load_json(o.meta)
            if meta.get("mode") not in MODES:
                die("meta: unknown mode %r" % meta.get("mode"))
            p = dict(meta)
            p["schema"] = PROFILE_SCHEMA
            p["workloads"] = [load_json(w) for w in o.workloads]
            check_profile(p, o.out)
            write_json(o.out, p)
            return 0
        if o.cmd == "compare":
            base = check_profile(load_json(o.base), o.base)
            head = check_profile(load_json(o.head), o.head)
            if base["mode"] != head["mode"]:
                die("cannot compare a %s profile with a %s profile" % (base["mode"], head["mode"]))
            cmp = compare(base, head, thresholds_from(o))
            if o.json:
                write_json(o.json, cmp)
            if o.md:
                with open(o.md, "w") as f:
                    f.write(render(head, cmp))
            for f in cmp["findings"]:
                print("FINDING %s %s %s: %.2f%% -> %.2f%%" % (f["workload"], f["kind"], f["name"],
                                                            f["base_pct"], f["head_pct"]))
            return 1 if cmp["findings"] else 0
        if o.cmd == "render":
            p = check_profile(load_json(o.profile), o.profile)
            cmp = load_json(o.comparison) if o.comparison else None
            if cmp is not None and cmp.get("schema") != COMPARISON_SCHEMA:
                die("%s: not a %s comparison" % (o.comparison, COMPARISON_SCHEMA))
            sys.stdout.write(render(p, cmp))
            return 0
        if o.cmd == "gen-corpus":
            sys.stdout.write(gen_corpus(o.size))
            return 0
    except (ReportError, ValueError) as e:
        die(str(e))
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
