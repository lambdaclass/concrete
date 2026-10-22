#!/usr/bin/env python3
"""Regression verdict for one measured commit.

  detect.py --records data/<machine>/records.jsonl [--sha SHA] --out summary.json
            [--window N] [--threshold X] [--noise-k K] [--min-history H]

Compares the record for SHA (default: the newest) with the median of the last
N comparable records (same machine, same suite_version, not contaminated) and
writes a concrete.perf.summary.v1 JSON for the status page. Exit status:
0 no regression, 1 regression, 2 usage or data error. A contaminated record is
never a regression (exit 0, verdict "contaminated").
"""

import argparse
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--records", required=True)
    ap.add_argument("--sha")
    ap.add_argument("--out", required=True)
    ap.add_argument("--window", type=int)
    ap.add_argument("--threshold", type=float)
    ap.add_argument("--noise-k", type=float)
    ap.add_argument("--min-history", type=int)
    ap.add_argument("--quiet", action="store_true")
    a = ap.parse_args()
    try:
        recs = perflib.parse_records(open(a.records).read())
    except (OSError, ValueError) as ex:
        sys.stderr.write("detect: %s\n" % ex)
        return 2
    full = [r for r in recs if r.get("bench") is None]
    if not full:
        sys.stderr.write("detect: no records\n")
        return 2
    if a.sha:
        cur = [r for r in full if r["sha"].startswith(a.sha)]
        if not cur:
            sys.stderr.write("detect: no record for %s\n" % a.sha)
            return 2
        cur = cur[-1]
        recs_before = full[:full.index(cur)]
    else:
        cur = full[-1]
        recs_before = full[:-1]
    cfg = {}
    for k in ("window", "threshold", "noise_k", "min_history"):
        v = getattr(a, k)
        if v is not None:
            cfg[k] = v
    summary = perflib.detect(recs_before, cur, cfg)
    perflib.annotate_bisect_targets(recs_before, cur, summary)
    summary["subject"] = cur.get("subject")
    with open(a.out, "w") as f:
        json.dump(summary, f, indent=1, sort_keys=True)
        f.write("\n")
    if not a.quiet:
        print("perf verdict for %s on %s: %s (window %d)" % (cur["sha"][:12], cur["machine"],
                                                          summary["verdict"], summary["window"]))
        for name, e in sorted(summary["metrics"].items()):
            if e["verdict"] in ("regression", "improvement", "too_noisy"):
                print("  %-11s %-48s %10.3f -> %10.3f %s (x%.3f, band %.1f%%)" % (
                    e["verdict"], name, e["baseline"], e["current"], e["unit"], e["ratio"], 100 * e["band"]))
    return 1 if summary["verdict"] == "regression" else 0


if __name__ == "__main__":
    sys.exit(main())
