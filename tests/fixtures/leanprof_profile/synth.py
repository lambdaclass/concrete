#!/usr/bin/env python3
"""Synthetic leanprof reports (version 3) for check_leanprof_profile.sh. Never a measurement.

  synth.py --mode sample|auto --variant V [--seed N] [--rep N] OUT.json

A build of about 500 ms CPU with the compiler's stage structure: each stage's inclusive time is
the sum of its leaf functions plus a little self time, so the stages nest the way a real
`concrete build` does (load-project holds the front end). Variants:
  base          the reference profile
  noise         every leaf moved by up to +-6% (deterministic per seed and rep): no finding
  uniform       every scope 1.5x slower (a slower host or clock): no finding, shares unchanged
  grown         one existing hot spot (emitSModule's search, ~9%) about doubles: a `grown` finding
  quadratic     the R-0551 control: a deliberate quadratic loop in the lower stage, a function
                absent from the base, becomes ~20% of the build: a `new` finding and lower's
                `stage-grown`
  small-double  a 0.3% function doubles: below the absolute threshold, no finding
  renamed-stage the emit stage is timed through its fallback declaration (Pipeline.emit absent)
"""
import argparse, json, random

# (stage, function, self ms, calls)
LEAVES = [
    ("parse", "Concrete.parseProgramPartial", 30.0, 41),
    ("resolve-files", "Concrete.resolveModules (in List.forIn'.loop)", 30.0, 41),
    ("resolve", "Concrete.resolveBodies", 12.0, 1),
    ("check", "Concrete.checkModule", 35.0, 41),
    ("elaborate", "Concrete.elabModule (in List.filterMapTR.go)", 30.0, 37385),
    ("elaborate", "Concrete.elabModule (in List.filterTR.loop)", 14.0, 11157),
    ("elaborate", "Concrete.Layout.tySize_namedOrGeneric", 1.5, 3714),
    ("core-check", "Concrete.ccCheckModule (in List.foldl)", 10.0, 164),
    ("proof-core", "Concrete.Sha256Fast.compress (in Std.Legacy.Range.forIn'.loop)", 12.0, 49566),
    ("proof-core", "Concrete.generateObligations (in List.mapTR.loop)", 40.0, 1266),
    ("monomorphize", "Concrete.monoModule", 15.0, 41),
    ("lower", "Concrete.cleanupFixpoint", 13.0, 667),
    ("lower", "Concrete.lowerFn", 30.0, 667),
    ("emit", "Concrete.Symbols.isRuntimeHelperName (in List.elem)", 75.0, 448186),
    ("emit", "Concrete.moduleCallContext (in List.filterTR.loop)", 45.0, 272),
    ("emit", "Concrete.emitSInst", 20.0, 9978),
]
STAGE_DECL = {"parse": "Concrete.parse", "resolve-files": "Concrete.resolveAllModules",
              "resolve": "Concrete.Pipeline.resolve", "check": "Concrete.checkProgram",
              "elaborate": "Concrete.Pipeline.elaborate", "core-check": "Concrete.Pipeline.coreCheck",
              "proof-core": "Concrete.extractProofCore?", "monomorphize": "Concrete.Pipeline.monomorphize",
              "lower": "Concrete.Pipeline.lower", "emit": "Concrete.Pipeline.emit"}
FRONT = ["parse", "resolve-files", "resolve", "check", "elaborate", "core-check", "proof-core"]


def build(mode, variant, seed, rep):
    rnd = random.Random(seed * 1000 + rep)
    leaves = [list(x) for x in LEAVES]
    if variant == "noise":
        for l in leaves:
            l[2] *= 1 + rnd.uniform(-0.06, 0.06)
    if variant == "grown":
        for l in leaves:
            if l[1].startswith("Concrete.moduleCallContext"):
                l[2] *= 2.2
    if variant == "quadratic":
        leaves.append(["lower", "Concrete.lowerQuadraticControl", 120.0, 667])
    leaves.append(["emit", "Concrete.emitSmallHelper", 3.0 if variant == "small-double" else 1.5, 10])
    scale = 1.5 if variant == "uniform" else 1.0
    ns = lambda ms: int(ms * scale * 1e6)
    stage_total = {}
    scopes = []
    for st, fn, ms, calls in leaves:
        stage_total[st] = stage_total.get(st, 0.0) + ms
        scopes.append({"name": fn, "pos": "Concrete/X.lean:1", "total_ns": ns(ms), "self_ns": ns(ms), "calls": calls})
    for st, decl in STAGE_DECL.items():
        if variant == "renamed-stage" and st == "emit":
            decl = "Concrete.emitSSAProgram"
        t = stage_total.get(st, 0.0) + 0.5
        scopes.append({"name": decl, "pos": "Concrete/Pipeline/Pipeline.lean:1", "total_ns": ns(t), "self_ns": ns(0.5), "calls": 1})
    front = sum(stage_total.get(s, 0.0) + 0.5 for s in FRONT) + 0.4
    scopes.append({"name": "Concrete.loadProject", "pos": "Concrete/Resolve/Project.lean:873", "total_ns": ns(front), "self_ns": ns(0.4), "calls": 1})
    wall = sum(s["self_ns"] for s in scopes) + ns(5.0)
    scopes.append({"name": "compileBuild", "pos": "Main.lean:3663", "total_ns": wall, "self_ns": ns(5.0), "calls": 1})
    if mode == "sample":
        for s in scopes:
            s["calls"] = 0
        return {"version": 3, "sampled": True, "wall_ns": wall, "scopes": scopes, "copies": []}
    for s in scopes:
        s["auto"] = True
    return {"version": 3, "wall_ns": wall, "probe_ns": 1000, "scopes": scopes, "copies": [], "allocations": []}


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--mode", choices=("sample", "auto"), default="sample")
    ap.add_argument("--variant", default="base")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--rep", type=int, default=1)
    ap.add_argument("out")
    a = ap.parse_args()
    with open(a.out, "w") as f:
        json.dump(build(a.mode, a.variant, a.seed, a.rep), f)
