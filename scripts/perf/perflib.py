"""Shared logic for the compiler performance tracker (scripts/perf/).

Pure functions only: statistics, the record format, regression detection and
bisect probe selection. Everything that touches a process, a clock or git lives
in the scripts that import this module, so check_perf_tracking.sh can drive the
decisions with fixtures.

RECORD FORMAT (one JSON object per line in data/<machine>/records.jsonl on the
`perf-data` branch, appended in the order origin/main commits were measured):

  {"schema": "concrete.perf.record.v1",
   "suite_version": <int>,           # bump when benchmark definitions change
   "harness": "<sha of the scripts/perf tooling that measured>",
   "machine": "<machine id>",         # results are never compared across machines
   "sha": "<40-hex measured commit>", "subject": "...", "commit_time": "<iso>",
   "measured_at": "<iso>", "status": "ok" | "build_failed" | "partial",
   "contaminated": <bool>,            # foreign heavy work ran during the measurement
   "host": {...}, "notes": [...],
   "metrics": {"<name>": {"unit": "ms"|"kb"|"s"|"exp", "median": <num>,
                          "lo": <num>, "hi": <num>, "n": <int>, "samples": [...]}}}

A metric name is "<group>.<bench>.<quantity>", e.g. compile.lox.wall_ms,
compile.lox.rss_kb, pass.hmac_sha256.lower_ms, lake.cold.wall_s,
tests.run_tests.wall_s, gate.check_compiler_complexity.wall_s,
growth.syn_match.time_exp. For every metric larger is worse.
"""

import json
import math
import re

RECORD_SCHEMA = "concrete.perf.record.v1"
SUMMARY_SCHEMA = "concrete.perf.summary.v1"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
METRIC_RE = re.compile(r"^[a-z]+\.[A-Za-z0-9_\-]+\.[a-z0-9_]+$")
UNITS = {"ms", "s", "kb", "exp", "count"}


# ---------------------------------------------------------------- statistics

def median(xs):
    xs = sorted(x for x in xs if x is not None)
    if not xs:
        return None
    n = len(xs)
    mid = n // 2
    return xs[mid] if n % 2 else (xs[mid - 1] + xs[mid]) / 2.0


def mad(xs):
    """Median absolute deviation (unscaled)."""
    m = median(xs)
    if m is None:
        return None
    return median([abs(x - m) for x in xs])


def summarize(samples, unit):
    """Collapse repeated samples into a metric entry: median plus the range."""
    s = [float(x) for x in samples if x is not None]
    if not s:
        return None
    return {"unit": unit, "median": round(median(s), 3), "lo": round(min(s), 3),
            "hi": round(max(s), 3), "n": len(s), "samples": [round(x, 3) for x in s]}


def within_run_noise(entry):
    """Robust relative spread of one record's repeated samples, 1.4826 * MAD /
    median (0 with fewer than 3 samples). MAD, not the range: one slow sample
    out of five must not widen the band enough to hide a real slowdown."""
    if not entry or len(entry.get("samples") or []) < 3 or not entry.get("median"):
        return 0.0
    return 1.4826 * mad(entry["samples"]) / entry["median"]


def history_noise(values):
    """Robust relative spread of a metric's medians across earlier commits:
    1.4826 * MAD / median (the MAD estimate of one standard deviation)."""
    vals = [v for v in values if v is not None]
    if len(vals) < 3:
        return None
    m = median(vals)
    if not m:
        return None
    return 1.4826 * mad(vals) / m


# ---------------------------------------------------------------- validation

def validate_record(rec):
    """Return a list of format errors (empty when the record is well formed)."""
    errs = []
    if not isinstance(rec, dict):
        return ["record is not an object"]
    if rec.get("schema") != RECORD_SCHEMA:
        errs.append("schema is %r, expected %r" % (rec.get("schema"), RECORD_SCHEMA))
    for k in ("suite_version", "machine", "sha", "measured_at", "status", "metrics"):
        if k not in rec:
            errs.append("missing key %s" % k)
    if "sha" in rec and not SHA_RE.match(str(rec["sha"])):
        errs.append("sha %r is not 40 lowercase hex" % rec.get("sha"))
    if not isinstance(rec.get("suite_version"), int):
        errs.append("suite_version is not an int")
    if not rec.get("machine") or not re.match(r"^[A-Za-z0-9_.\-]+$", str(rec.get("machine"))):
        errs.append("machine id %r is empty or not path-safe" % rec.get("machine"))
    if rec.get("status") not in ("ok", "build_failed", "partial"):
        errs.append("status %r unknown" % rec.get("status"))
    if not isinstance(rec.get("contaminated", False), bool):
        errs.append("contaminated is not a bool")
    metrics = rec.get("metrics")
    if not isinstance(metrics, dict):
        errs.append("metrics is not an object")
        return errs
    for name, e in metrics.items():
        if not METRIC_RE.match(name):
            errs.append("metric name %r is not <group>.<bench>.<quantity>" % name)
        if not isinstance(e, dict):
            errs.append("metric %s is not an object" % name)
            continue
        if e.get("unit") not in UNITS:
            errs.append("metric %s unit %r unknown" % (name, e.get("unit")))
        num = lambda x: isinstance(x, (int, float)) and not isinstance(x, bool) and math.isfinite(x)
        bad = [k for k in ("median", "lo", "hi") if not num(e.get(k))]
        if bad:
            errs.append("metric %s field(s) %s missing or not finite numbers" % (name, ",".join(bad)))
            continue
        n = e.get("n")
        if not isinstance(n, int) or isinstance(n, bool) or n < 1:
            errs.append("metric %s n %r is not a positive integer" % (name, n))
            continue
        samples = e.get("samples")
        if not isinstance(samples, list) or len(samples) != n or not all(num(x) for x in samples):
            errs.append("metric %s samples are not %d finite numbers" % (name, n))
            continue
        if not (e["lo"] <= e["median"] <= e["hi"]):
            errs.append("metric %s violates lo <= median <= hi" % name)
        tol = 2e-3 + 1e-9 * max(abs(x) for x in samples)
        if abs(min(samples) - e["lo"]) > tol or abs(max(samples) - e["hi"]) > tol \
                or abs(median(samples) - e["median"]) > tol:
            errs.append("metric %s lo/median/hi disagree with its samples" % name)
    return errs


def parse_records(text):
    """Parse a records.jsonl body. Raises ValueError naming the first bad line."""
    out = []
    for i, line in enumerate(text.splitlines(), 1):
        if not line.strip():
            continue
        try:
            rec = json.loads(line)
        except ValueError as ex:
            raise ValueError("line %d: not JSON (%s)" % (i, ex))
        errs = validate_record(rec)
        if errs:
            raise ValueError("line %d: %s" % (i, "; ".join(errs)))
        out.append(rec)
    return out


def dump_record(rec):
    errs = validate_record(rec)
    if errs:
        raise ValueError("refusing to write a malformed record: " + "; ".join(errs))
    return json.dumps(rec, sort_keys=True, separators=(",", ":"))


# ---------------------------------------------------------------- detection

DEFAULTS = {
    "window": 10,          # baseline = median of the last N comparable records
    "min_history": 3,      # no verdict until this many comparable records exist
    "noise_k": 3.0,        # noise band = noise_k * robust relative spread
    "threshold": 0.05,     # X: flag when slower than baseline * (1 + band) * (1 + X)
    "min_band": 0.02,      # a band is never assumed tighter than 2%
    "max_band": 0.50,      # a metric noisier than this is reported, never alarmed on
    # Absolute floors below which a change is never a regression (clock and
    # allocator granularity): 25 ms, 0.5 s, 4 MB, 0.15 of a growth exponent.
    "min_abs": {"ms": 25.0, "s": 0.5, "kb": 4096.0, "exp": 0.15, "count": 1.0},
}


def comparable(rec, machine, suite_version):
    return (rec.get("machine") == machine and rec.get("suite_version") == suite_version
            and rec.get("status") in ("ok", "partial") and not rec.get("contaminated", False))


def detect(records, current, cfg=None):
    """Compare `current` with the comparable records that precede it.

    Returns a summary dict (SUMMARY_SCHEMA) with one entry per metric and a
    list of regressions. A metric regresses when its median exceeds
        baseline * (1 + band) * (1 + threshold)   and   baseline + min_abs
    where baseline is the median of the last `window` comparable medians and
    band = clamp(noise_k * max(history spread, current within-run spread)).
    """
    c = dict(DEFAULTS)
    c.update(cfg or {})
    machine, sv = current["machine"], current["suite_version"]
    prior = [r for r in records if r is not current and r.get("sha") != current["sha"]
             and comparable(r, machine, sv)]
    window = prior[-c["window"]:]
    entries = {}
    regressions = []
    improvements = []
    for name, e in sorted(current.get("metrics", {}).items()):
        hist = [r["metrics"][name]["median"] for r in window if name in r.get("metrics", {})]
        ent = {"current": e["median"], "unit": e["unit"], "history_n": len(hist)}
        if len(hist) < c["min_history"]:
            ent["verdict"] = "insufficient_history"
            entries[name] = ent
            continue
        base = median(hist)
        hn = history_noise(hist) or 0.0
        wn = within_run_noise(e)
        band = max(c["min_band"], c["noise_k"] * max(hn, wn))
        ent.update({"baseline": round(base, 3), "noise": round(max(hn, wn), 4),
                    "band": round(band, 4)})
        floor = c["min_abs"].get(e["unit"], 0.0)
        # One rule for every baseline, including zero and negative ones (a pass rounded to 0 ms, a
        # growth exponent): slower than the relative band AND than the absolute floor.
        delta = max(abs(base) * ((1 + band) * (1 + c["threshold"]) - 1), floor)
        limit = base + delta
        ent["limit"] = round(limit, 3)
        if base > 0:
            ent["ratio"] = round(e["median"] / base, 4)
        if band > c["max_band"]:
            ent["verdict"] = "too_noisy"
        elif e["median"] > limit:
            # A contaminated record is reported, never alarmed on.
            ent["verdict"] = "slower_contaminated" if current.get("contaminated") else "regression"
            if not current.get("contaminated"):
                regressions.append(name)
        elif e["median"] < base - delta:
            ent["verdict"] = "improvement"
            improvements.append(name)
        else:
            ent["verdict"] = "ok"
        entries[name] = ent
    last_good = prior[-1]["sha"] if prior else None
    # Metrics the newest comparable record had and this one lacks: a benchmark that stopped
    # producing a number is a signal, not silence.
    missing = sorted(set(window[-1]["metrics"]) - set(current.get("metrics", {}))) if window else []
    if current.get("contaminated"):
        verdict = "contaminated"
    elif regressions:
        verdict = "regression"
    elif current.get("status") == "build_failed":
        verdict = "build_failed"
    elif current.get("status") == "partial" or missing:
        verdict = "incomplete"
    elif not window or len(window) < c["min_history"]:
        verdict = "insufficient_history"
    else:
        verdict = "ok"
    return {
        "schema": SUMMARY_SCHEMA,
        "machine": machine,
        "suite_version": sv,
        "sha": current["sha"],
        "measured_at": current.get("measured_at"),
        "contaminated": bool(current.get("contaminated", False)),
        "status": current.get("status"),
        "window": len(window),
        "config": {k: c[k] for k in ("window", "min_history", "noise_k", "threshold",
                                     "min_band", "max_band")},
        "regressions": regressions,
        "improvements": improvements,
        "last_measured_before": last_good,
        "missing": missing,
        "failed": list(current.get("failed", [])),
        "metrics": entries,
        "verdict": verdict,
    }


def regression_limit(good_value, band, threshold, unit):
    """The alarm line above a good value, as detect() draws it."""
    floor = DEFAULTS["min_abs"].get(unit, 0.0)
    return good_value + max(abs(good_value) * ((1 + band) * (1 + threshold) - 1), floor)


def last_good(records, current, name, ceiling):
    """The newest comparable record before `current` whose `name` median is at
    or below `ceiling` (the baseline plus its noise band): the good end of a
    bisect. None when no such record exists."""
    machine, sv = current["machine"], current["suite_version"]
    for r in reversed(records):
        if r is current or r.get("sha") == current["sha"] or not comparable(r, machine, sv):
            continue
        e = r.get("metrics", {}).get(name)
        if e is not None and e["median"] <= ceiling:
            return r["sha"]
    return None


def annotate_bisect_targets(records, current, summary):
    """Add a `good` sha to every regressed metric in a detect() summary."""
    for name in summary["regressions"]:
        ent = summary["metrics"][name]
        ent["good"] = last_good(records, current, name, ent["baseline"] * (1 + ent["band"]))
    return summary


# ---------------------------------------------------------------- bisect

def classify(value, limit):
    """A probe is 'bad' when it is slower than `limit`, the same criterion the
    detector applies to the good end: good * (1 + band) * (1 + threshold),
    and at least the absolute floor. So a partial step (1000 -> 1300 -> 2000)
    is named where it first crosses the alarm line, not at the midpoint."""
    return "bad" if value > limit else "good"


def next_probe(commits, verdicts):
    """Binary-search step over first-parent `commits` (oldest first), where
    commits[0] is known good and commits[-1] is known bad. `verdicts` maps sha
    -> 'good' | 'bad' | 'skip' (a commit that did not build). Returns
    ("probe", sha) or ("done", culprit_sha_or_None, candidates)."""
    if len(commits) < 2:
        return ("done", commits[-1] if commits else None, list(commits[-1:]))
    v = dict(verdicts)
    v[commits[0]] = "good"
    v[commits[-1]] = "bad"
    lo = max(i for i, s in enumerate(commits) if v.get(s) == "good"
             and all(v.get(t) != "bad" for t in commits[:i]))
    hi = min(i for i, s in enumerate(commits) if v.get(s) == "bad" and i > lo)
    # Every untested, non-skipped commit strictly between lo and hi is a candidate.
    inner = [i for i in range(lo + 1, hi) if v.get(commits[i]) != "skip"]
    if not inner:
        cands = [commits[i] for i in range(lo + 1, hi + 1)]
        return ("done", commits[hi] if len(cands) == 1 else None, cands)
    mid_target = (lo + hi) / 2.0
    pick = min(inner, key=lambda i: (abs(i - mid_target), i))
    return ("probe", commits[pick])


def bisect(commits, measure, limit, max_probes=32):
    """Drive next_probe with `measure(sha) -> value | None` (None = skip).
    Returns (culprit_or_None, candidates, probes) where probes lists
    (sha, value, verdict) in probe order."""
    verdicts = {}
    probes = []
    for _ in range(max_probes):
        step = next_probe(commits, verdicts)
        if step[0] == "done":
            return step[1], step[2], probes
        sha = step[1]
        val = measure(sha)
        verdict = "skip" if val is None else classify(val, limit)
        verdicts[sha] = verdict
        probes.append((sha, val, verdict))
    step = next_probe(commits, verdicts)
    return (step[1] if step[0] == "done" else None,
            step[2] if step[0] == "done" else [], probes)
