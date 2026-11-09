# N2 A3: round-5 cost regression and round-6 deadline starvation

Diagnosis date: 2026-11-09. Design/diagnosis only. Structural owner: R-0219,
phase-B discharge solver (slice 2a), with accounting design in the
[A3 amendment](N2-discharge-solver.md#a3-amendment-per-analysis-accounting-and-the-round-5-regression).
Reviewers: **do the best long-term thing rather than cut corners**.
No vacuity.lean, spec, ROADMAP or other branch changes accompany this record.

## Inputs and bounded measurement method

Source archive: `6efdce6136752d51fc74265d61278d90937f24cd`.
Checker revisions:

- `550fab85e40c2d75a450edc4bb63498fedff3cd3` (before round 5).
- `cc21095890e11195106179ab46b7ed14c48d498e` (round 5).
- `8061f277b63e99339fc7f102988783a9a1c6c166` (round-6 isolation diagnosis).

Extracted `/home/unbalancedparen/projects/gates-briefs/NBIS-outputs.tgz`
into private scratch `/tmp/n2-a3-693jd72v/outputs`; source was extracted there
into `source/` using `git archive`, without changing any checkout. The supplied
RESULTS.md NBIS bisection is whole-scope evidence, not the focused measurements.

The local focused probe used `nix develop`, Lean 4.34.1, and the driver's private
source elaboration and compiled consumer. It substituted historical consumer
text and replaced only the audit entry with a probe of the selected retained
Instance's proposition arguments. It uses the same law telescope, argument order,
`isProp` test, `falseDischarge` and fresh `{}` state per argument as those commits.
Each root ran in a separate Lean invocation with the existing 2,000,000-heartbeat
command option and default 20,000 work budget. Projection is not invoked by this
focused probe. Current population code supplies enumeration/retained names only;
this is not a historical whole-driver replay. No full suite was run.

The probe prints work on normal return and at the existing fuel-zero throw.
A runtime catch reports elapsed milliseconds and raw `IO.getNumHeartbeats`
difference; Lean's user units are raw/1,000 (CoreM reports `max/1000`). The
2,000,000 command ceiling still includes probe setup; no analysis ran with an
increased limit. Wall watchdog: `bounded.sh 240` around each nix invocation;
the driver's own subprocess bounds also remain. All completed on the first
attempt. Host elapsed time is observational, not an acceptance budget.

## Measurements: two roots, two revisions

| checker | retained source position / law | work per proposition argument | result | analysis ms | raw heartbeat delta | user heartbeat delta (raw/1000) | complete invocation seconds |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 550fab85e | Grant.lean:1478 / acquire_bounded | 68 | bad=true | 18 | 505382 | 505.382 | 29.310 |
| cc2109589 | Grant.lean:1478 / acquire_bounded | 20000, exhausted | false-discharge-budget | 30876 | 1029962503 | 1029962.503 | 59.611 |
| 550fab85e | Own.lean:5732 / downward_reborrow_protocol | 135, 1, 5, 1, 3 (total 145) | true, false, false, false, false | 64 | 1942286 | 1942.286 | 28.514 |
| cc2109589 | Own.lean:5732 / downward_reborrow_protocol | 20000, exhausted in first proof argument | false-discharge-budget | 27073 | 807818825 | 807818.825 | 56.536 |

A second instrumented round-5 Own run confirmed exactly 20,000-work exhaustion:
26,358 ms, 807,818,861 raw heartbeats (807,818.861 user), invocation 55.685 s.
The additional instrumentation catches `whnfD` exceptions at entry/applied sites;
no such exception occurred before the work throw. The tiny heartbeat difference
is instrumentation overhead, not a new solver budget.

The probe intentionally returns an empty finding array after printing its
observation, so its driver's `N2 PASS` / rc=0 is **not** an audit verdict. Only
the MEASURE records above describe the root. Historical `bad=true` is the old
algorithm's answer, not evidence that round 5 should reproduce that answer:
round 5 changed the supplied/hypothetical distinction for soundness.

**The fresh-budget experiment falsifies the premise that Own individually needs
2,000,000 heartbeats before stopping.** It exhausts work first. The archived
`whnf` timeout is real, but its 2,000,000 maximum is measured from the shared
command baseline, not Own entry. These observations must not be conflated.
The work-exhausted figures are cut-short costs, never completed solver costs.

## Source-level regression: what changed and why it costs

All source references below are revision-relative `scripts/proofs/vacuity.lean`
line numbers, not current-file links.

| mechanism | 550fab85e | cc2109589 |
| --- | --- | --- |
| Visited graph identity | :333-336, :344-349: expression set plus openBodies declaration set | :349-351, :358-366: expression × full FVarId-array context; no openBodies |
| Open helper expansion | :359-368: open declarations expanded once, generic body for open calls; actual application only for closed calls | :377-382: every body instantiated at all actual arguments, headBeta then whnfD |
| Immediate impossible inhabitant | :350, :353-358: refutable type / eliminator immediately bad | :367-387: closed cases consult context refutation; open derivations followed to actual suppliers; refuted axiom producers tested |
| Hypothetical context | :375-378: refutes individual binder types only | :334-347 and :394-398: joint contradictionCore, optional simpAll, instantiate/check candidate proof |

The expansion change at cc2109589:379-382 is the structural regression:
substituting every open call materializes specialized proof machinery instead of
reusing a declaration template. Its recursive traversal opens fresh locals; the
new key at :362 distinguishes their entire local contexts, so syntactically
similar calls in fresh contexts no longer share visited entries. The combination
removes the old finite once-per-open-declaration traversal bound. Normalization
runs at :369 and :381 before recursive traversal charges the next node; one
charged visit can encompass substantial `whnf` work. Context contradiction search
and kernel checks at :334-347 also cost heartbeats outside the visit counter.
The new conditional treatment of refutable expressions removes old early exits.

This explains both observed 20,000-visit exhaustions and why heartbeat exhaustion
can happen *inside* whnf rather than at the next worklist visit. It does not
attribute all elapsed time to simpAll or claim an unmeasured exclusive timing
breakdown between inference, normalization and context search. The direct
revision diff plus focused runs identify the changed algorithm, not an ablation
study proving one changed line alone accounts for the entire regression.

Restoring the old openBodies optimization would erase actual argument provenance
and reintroduce round 5's supplied-contradiction hole. Context-free caching would
also let one hypothetical unreachable application mask another supplied call.
The repair is the symbolic application-sensitive summary/SCC solver, not either
rollback. The A3 amendment assigns it to the existing slice-2a phase-B design.

## Why isolated starves everything later

At 8061f277b:553 the checker saves one `Core.Context` before running the audit.
At :531-537 every `isolated` installs exactly that saved context and explicitly
checks `vacuity-analysis` before doing any work. At :791-794 the outer scheduler
sets maxHeartbeats=0 for bookkeeping; it does not reset the saved context's
`initHeartbeats`. Meta save/restore at :533/:540 does not rewind the global
heartbeat counter. Thus one exception can be caught while the deadline remains
spent forever. The catch emits a name (:541-543), but isolation of exceptions is
not isolation of resource allowance.

Archived 8061f277b (and later through 6fe932293) has this sequence:
Grant :1478 work exhaustion; Own :5732 timeout inside whnf; then every subsequent
analysis times out at vacuity-analysis entry. The accumulated counter includes
all earlier analyses and bookkeeping; adding just the two focused deltas is not
an exact reconstruction of that command. In particular their combined round-5
cost is already about 1,837,781 user heartbeats before other audit work.

Historical whole-scope results, from the supplied ci-2 archive/summary:

- 550fab85e: 39 s, 259 FAIL (5 instance-application, 60 instance-false-discharge,
  64 missing-instance, 65 planted-proposition, 65 unpaired-planted), 0 INCOMPLETE.
- cc2109589: 70 s, one anonymous false-discharge-budget INCOMPLETE, 0 FAIL.
  The uncaught root exception discards the audit's accumulated findings.
- 8061f277b: 111 s, 220 INCOMPLETE, 0 FAIL: one work exhaustion and 219 heartbeat
  exceptions, including the real in-progress Own timeout and subsequent starved
  entries. Later bisection revisions reproduce this at about 112 s.

Findings changing to INCOMPLETE do not demonstrate repaired proofs. There are two
structural owners within R-0219: the application-sensitive solver removes the
cost regression; the per-analysis scheduler prevents it hiding unrelated results.
The owner must still resolve the whole-scope/per-analysis exit-criterion question
in A3. This diagnosis chooses neither interpretation.

## Focused document validation

On this document slice, under `nix develop` and bounded runners:
`issues.sh` PASS=5 FAIL=0; `doc_links.sh` PASS=12 FAIL=0 INCOMPLETE=0
(repository CHECKED=1162 ANCHORS=898). `git diff --check` was clean.
No review tooling, heavy gates or pushes were run. The coordinator owns review,
server acceptance replays and publication.

## Reproducing the focused probe

Create a private scratch directory, extract `git archive 6efdce61` into its
`source/`, and run the script below from this clone, once for each revision and
each target suffix, under `bash scripts/checks/lib/bounded.sh 240 nix develop
--command python3 <scratch>/measure.py <scratch> <revision> <target>`.
Targets are `ConcreteSpec/Proofs/Grant.lean:1478` and
`ConcreteSpec/Proofs/Own.lean:5732`. The printed heartbeat delta excludes source
compilation and measures only the selected Instance analysis. The full invocation
wall time includes private elaboration. The script makes no tracked changes.

```python
import sys,subprocess,time
from pathlib import Path
sys.path.insert(0,str(Path.cwd()/'scripts/proofs'))
import vacuity_driver as d
p=Path(sys.argv[1]); rev=sys.argv[2]; target=sys.argv[3]
c=subprocess.check_output(['git','show',rev+':scripts/proofs/vacuity.lean'],text=True)
c=c.replace('def check (population', 'def originalCheck (population')
c=c.replace('if fuel == 0 then throwError "false-discharge-budget"','if fuel == 0 then\n    IO.println "MEASURE work=20000 exhausted"\n    throwError "false-discharge-budget"')
c += r"""
namespace Vacuity
def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  for (n, ci) in population do
    if (toString n).endsWith "TARGET»" then
      let start ← IO.monoMsNow
      let hb ← IO.getNumHeartbeats
      tryCatchRuntimeEx (do
        let some value := ci.value? | throwError "no value"
        let .const law levels := (cleanSpine value).getAppFn' | throwError "no law"
        let lawInfo ← getConstInfo law
        let mut ty := lawInfo.type.instantiateLevelParams lawInfo.levelParams levels
        for arg in (cleanSpine value).getAppArgs do
          if let .forallE _ domain body _ := ty.consumeMData then
            if ← isProp domain then
              let (bad, state) ← (falseDischarge arg).run {}
              IO.println s!"MEASURE bad={bad} work={20000-state.remaining}"
            ty := body.instantiate1 arg
        IO.println s!"MEASURE OK ms={(← IO.monoMsNow)-start} heartbeatsRaw={(← IO.getNumHeartbeats)-hb}")
        (fun e => do
          IO.println s!"MEASURE ERROR ms={(← IO.monoMsNow)-start} heartbeatsRaw={(← IO.getNumHeartbeats)-hb} {← e.toMessageData.toString}")
  return #[]
end Vacuity
""".replace('TARGET',target)
t=time.monotonic()
with d.compiled_consumer(consumer_text=c) as meta:
 code,out=d.run(p/'source','ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own',meta_path=meta)
 print(rev,target,'wall',round(time.monotonic()-t,3), 'rc',code); print(out)
```
