#!/usr/bin/env python3
"""Trends over the last N measured commits.

  report.py --records data/<machine>/records.jsonl [--last 30] [--filter SUBSTR]
            [--html out.html] [--summary summary.json]

Prints one line per metric: a sparkline over the last N comparable records,
first and last median, change, and min/max. --html writes a static page with an
inline SVG line chart per metric (no external assets, light and dark themes).
"""

import argparse
import html
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

BARS = "▁▂▃▄▅▆▇█"


def spark(vals):
    v = [x for x in vals if x is not None]
    if not v:
        return ""
    lo, hi = min(v), max(v)
    out = []
    for x in vals:
        if x is None:
            out.append(" ")
        elif hi == lo:
            out.append(BARS[3])
        else:
            out.append(BARS[int((x - lo) / (hi - lo) * (len(BARS) - 1))])
    return "".join(out)


def series(recs):
    names = sorted({n for r in recs for n in r["metrics"]})
    return {n: [r["metrics"].get(n, {}).get("median") for r in recs] for n in names}


def svg_chart(vals, shas, unit, w=300, h=70):
    pts = [(i, v) for i, v in enumerate(vals) if v is not None]
    if not pts:
        return ""
    lo = min(v for _, v in pts)
    hi = max(v for _, v in pts)
    span = (hi - lo) or (abs(hi) or 1.0)
    n = max(len(vals) - 1, 1)

    def xy(i, v):
        return (6 + (w - 12) * i / n, h - 8 - (h - 16) * (v - lo) / span)
    path = " ".join(("M" if k == 0 else "L") + "%.1f,%.1f" % xy(i, v) for k, (i, v) in enumerate(pts))
    dots = "".join('<circle cx="%.1f" cy="%.1f" r="2.5"><title>%s: %s %s</title></circle>'
                   % (xy(i, v) + (html.escape(shas[i][:12]), v, unit)) for i, v in pts)
    return ('<svg viewBox="0 0 %d %d" width="100%%" height="%d" role="img">'
            '<path d="%s" fill="none" stroke="var(--line)" stroke-width="1.5"/>%s</svg>' % (w, h, h, path, dots))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--records", required=True)
    ap.add_argument("--last", type=int, default=30)
    ap.add_argument("--filter")
    ap.add_argument("--html")
    ap.add_argument("--summary")
    a = ap.parse_args()
    recs = [r for r in perflib.parse_records(open(a.records).read()) if r.get("bench") is None]
    if not recs:
        print("no records")
        return 0
    machine, sv = recs[-1]["machine"], recs[-1]["suite_version"]
    recs = [r for r in recs if r["machine"] == machine and r["suite_version"] == sv][-a.last:]
    shas = [r["sha"] for r in recs]
    s = series(recs)
    if a.filter:
        s = {k: v for k, v in s.items() if a.filter in k}
    summary = json.load(open(a.summary)) if a.summary and os.path.exists(a.summary) else None
    verdicts = {k: e.get("verdict") for k, e in (summary or {}).get("metrics", {}).items()}
    print("machine %s, suite v%d, %d commits %s..%s (%d contaminated)" % (
        machine, sv, len(recs), shas[0][:12], shas[-1][:12], sum(1 for r in recs if r.get("contaminated"))))
    for name, vals in s.items():
        v = [x for x in vals if x is not None]
        if not v:
            continue
        unit = next(r["metrics"][name]["unit"] for r in recs if name in r["metrics"])
        ch = (v[-1] / v[0] - 1) * 100 if v[0] else 0.0
        flag = verdicts.get(name, "")
        flag = "" if flag in ("ok", "insufficient_history", "") else "  <" + flag
        print("%-50s %-*s %10.3g -> %10.3g %-3s %+6.1f%%  [%g..%g]%s" % (
            name, len(vals), spark(vals), v[0], v[-1], unit, ch, min(v), max(v), flag))
    if a.html:
        groups = {}
        for name, vals in s.items():
            groups.setdefault(name.split(".", 1)[0], []).append(name)
        cards = []
        for g in sorted(groups):
            items = []
            for name in groups[g]:
                vals = s[name]
                v = [x for x in vals if x is not None]
                if not v:
                    continue
                unit = next(r["metrics"][name]["unit"] for r in recs if name in r["metrics"])
                ver = verdicts.get(name, "")
                cls = " bad" if ver == "regression" else (" good" if ver == "improvement" else "")
                items.append('<div class="card%s"><div class="t">%s</div><div class="v">%.4g %s</div>%s</div>'
                             % (cls, html.escape(name), v[-1], unit, svg_chart(vals, shas, unit)))
            cards.append("<h2>%s</h2><div class=\"grid\">%s</div>" % (html.escape(g), "".join(items)))
        head = ("<p>Machine <b>%s</b>, suite v%d, last %d commits, %s to %s.</p>"
                % (html.escape(machine), sv, len(recs), shas[0][:12], shas[-1][:12]))
        if summary:
            head += "<p>Newest verdict: <b>%s</b>%s</p>" % (
                html.escape(summary.get("verdict", "")),
                "" if not summary.get("regressions") else ": " + html.escape(", ".join(summary["regressions"])))
        page = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Compiler performance</title>
<style>
:root{--bg:#fbfbfa;--fg:#1d1d1b;--muted:#6b6b66;--card:#fff;--edge:#e3e2dd;--line:#3b6fb6;--bad:#b3261e;--good:#2e7d32}
@media (prefers-color-scheme:dark){:root:not([data-theme=light]){--bg:#161615;--fg:#ecebe6;--muted:#9a9993;--card:#1f1f1d;--edge:#33322f;--line:#7aa7e6;--bad:#f2867c;--good:#7fcf86}}
:root[data-theme=dark]{--bg:#161615;--fg:#ecebe6;--muted:#9a9993;--card:#1f1f1d;--edge:#33322f;--line:#7aa7e6;--bad:#f2867c;--good:#7fcf86}
body{background:var(--bg);color:var(--fg);font:14px/1.45 system-ui,sans-serif;margin:0 auto;max-width:1100px;padding:16px}
h1{font-size:20px}h2{font-size:15px;margin-top:24px;color:var(--muted);text-transform:uppercase;letter-spacing:.05em}
.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(240px,1fr));gap:10px}
.card{background:var(--card);border:1px solid var(--edge);border-radius:8px;padding:8px 10px}
.card.bad{border-color:var(--bad)}.card.good{border-color:var(--good)}
.t{font:12px ui-monospace,monospace;color:var(--muted);overflow-wrap:anywhere}.v{font-weight:600}
circle{fill:var(--line)}
</style></head><body><h1>Compiler performance</h1>%s%s</body></html>
""" % (head, "".join(cards))
        with open(a.html, "w") as f:
            f.write(page)
    return 0


if __name__ == "__main__":
    sys.exit(main())
