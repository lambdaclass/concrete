# Bug 065: `--report stack-depth` drops recursive callees from the call graph, so a caller of unbounded code reports a finite stack bound

**Status:** FIXED 2026-10-18 (per-function propagation 3d9b63a5; summary and consumers this fix)
**Discovered:** 2026-07-31, while auditing the user-exposed surface for
under-gated features (`--report stack-depth` had zero references in
`scripts/tests/`).

## Resolution (2026-10-18)

The fix landed in two parts.

**Per-function propagation** (3d9b63a5, gated by `check_loop_control.sh` leg 6):
`closeUnbounded` closes the unbounded set backwards over the call graph, so a
caller of recursive or indirect-call code is itself `stack: unbounded`.

**The summary was still fail-OPEN after that.** `Max stack bound:` maximised over
`bounded` only, so it kept printing a number whenever at least one function was
bounded, and printed `0 bytes ((none))` when none was. On the regression fixture
(`depth` recurses, `main` calls it, `leaf` is a bounded helper) it read
`Max stack bound: 20 bytes (leaf)` while `main` was unbounded, and all three
consumers below took `20` as the program's maximum: a `max_stack_bytes = 100000`
policy and a `stack_max_bytes = 100000` assumption both PASSED, and a release
bundle would have published `"max_stack_bytes": 20`.

**Mechanism of the fix.**

- *One producer.* `stackSummaryLine` (`Concrete/Report/Report.lean`) builds the
  line. It covers every function the report lists — the report has no
  entry-point notion, so the summary is over all functions, not those reachable
  from `main` — and reads `Max stack bound: unbounded (K of N functions have no
  bound: a, b, c)` whenever any of them is unbounded. Only when all are bounded
  does it print `N bytes (fn)`; an empty program prints `0 bytes (no functions)`.
- *One reader.* `scripts/tests/lib/stack_bound.sh` is the only parser of that
  line (`stack_max_bound`: number / `unbounded` / nothing, with distinct exit
  codes). `check_policy.sh` (`max_stack_bytes`) and `check_assumptions.sh`
  (`stack_max_bytes`) call `stack_budget_check`, which FAILS on `unbounded`
  ("but the stack is UNBOUNDED: …"), on an unparseable report, and on a
  non-integer budget. `capture_release_bundle.sh` writes
  `evidence.max_stack_bytes` via `stack_bound_json`: a number, `"unbounded"`, or
  `"unknown"`.
- *Adjacent bundle defects found by the gate.* The old `|| echo "?"` fallback never
  fired (the pipeline's status was `head`'s), so a missing report would have
  written `"max_stack_bytes": ,`; and `grep -c … || echo 0` wrote `0\n0` for
  `proved_functions` on any project with no proved function. Both produced
  invalid JSON; both are fixed, and the capture now refuses a manifest that does
  not parse.

**Controls.** `scripts/tests/check_stack_budget.sh` over
`tests/regressions/bug065_unbounded_stack/{unbounded,bounded}` (14 legs):

1. the unbounded program's summary says `unbounded` and names `main.depth` and
   `main.main`; no numeric maximum is printed; the reader returns rc 3; `leaf`
   keeps its own `stack: 20 bytes` row (propagation goes to callers, not callees);
2. `check_policy.sh` and `check_assumptions.sh` REFUSE budgets of 100000, naming
   unboundedness; the release bundle publishes `"unbounded"` as valid JSON; a
   missing or unparseable report yields `"unknown"`;
3. positive control: the bounded program reports exactly
   `Max stack bound: 40 bytes (main)`, passes a budget of exactly 40 in both
   gates, FAILS a policy budget of 39, and its bundle publishes `40`;
4. no script other than `lib/stack_bound.sh` scrapes a number from the line.

Reverting `stackSummaryLine` to maximise over the bounded subset reproduces the
symptom (`20 bytes (leaf)`) and fails 6 of the 14 legs (all of legs 1–2 except
the leaf row and the `"unknown"` check). The five budgeted examples are
unchanged (all bounded; their snapshots and budgets still pass).

What this does not do: the class-level reachability assertion cross-checked
against `--report recursion` (R-0469) is still unbuilt.

The sections below are the original filing.

## Symptom

A non-recursive function that calls recursive functions is reported with a
finite stack bound, and the report's summary presents that bound as the
program's maximum.

```concrete
mod rec {
  fn is_even(n: Int) -> Bool { if n == 0 { return true; } return is_odd(n - 1); }
  fn is_odd(n: Int) -> Bool { if n == 0 { return false; } return is_even(n - 1); }
  fn fact(n: Int) -> Int { if n <= 1 { return 1; } return n * fact(n - 1); }
  fn main() -> Int { if is_even(4) { return fact(3); } return 0; }
}
```

```
$ concrete rec.con --report stack-depth
  is_even
    frame: 16 bytes  depth: unbounded (recursive)  stack: unbounded
  is_odd
    frame: 16 bytes  depth: unbounded (recursive)  stack: unbounded
  fact
    frame: 16 bytes  depth: unbounded (recursive)  stack: unbounded
  main
    frame: 8 bytes  depth: 0  stack: 8 bytes

Totals: 4 functions, 1 bounded, 3 recursive (unbounded)
Max stack bound: 8 bytes (main)
```

`main` calls `is_even` (mutual cycle) and `fact` (direct recursion). Its stack
usage is unbounded. It is reported as `depth: 0  stack: 8 bytes` — its own frame
only — and the summary line then reads `Max stack bound: 8 bytes (main)`, which
states a bound for the program that the program does not have.

`--report recursion` on the same input is correct: it names `rec.fact` as direct
recursion and `rec.is_odd -> rec.is_even -> rec.is_odd` as a mutual cycle. The
two reports disagree, and only the recursion one is right.

## Root cause

`computeCallDepths` filters recursive callees out of every caller's callee set
(`Concrete/Report/Report.lean:496`):

```lean
let callees := getCallees fn |>.filter (fun c =>
  !isRecursive c && frameSizes.any (fun (n, _) => n == c))
```

The filter's purpose is to stop the DFS from descending into a cycle, which it
does. But removing the edge also removes the *fact* of the call, so the caller's
fold never sees a contributor it cannot bound, and returns its own frame as the
answer. Unboundedness is a property that must propagate up the call graph; here
it is discarded at exactly the edge that carries it.

The same shape appears in the guard one line earlier
(`if isRecursive fn || visited.contains fn then ((0, getFrame fn), memo)`): a
cycle discovered through `visited` also yields a finite pair rather than an
unbounded one. `recMap` normally classifies real cycles first, so this is the
second door to the same wrong answer, not an independent defect.

The summary then compounds it. `maxStack` folds over `bounded` only
(`:578`), so recursive functions are excluded from the maximum by construction —
correct in isolation, since they have no bound to contribute, but it means the
printed maximum is the largest bound among functions whose bounds are themselves
computed with the missing edges.

## Severity

**Corrected 2026-07-31, same day as filing: an earlier draft of this section
called the defect "contained to the report". That was wrong.** It rested on

```
$ grep -rn 'stackBound\|stack_bound' Concrete/ Main.lean
Main.lean:2299:      IO.println (Report.stackDepthReport validCore.coreModules locMap pc)
```

which scoped the search to Lean sources. Enforcement of this number lives in
**shell**, and there are three consumers:

| Consumer | What it does with the max |
|---|---|
| `scripts/tests/check_policy.sh:161` | greps `Max stack bound:\s*[0-9]+` and enforces `[policy] max_stack_bytes = N` against it |
| `scripts/tests/check_assumptions.sh:128` | same, for `[allocation] stack_max_bytes = N` |
| `scripts/tests/capture_release_bundle.sh:128` | greps the same line into the release bundle as `evidence.max_stack_bytes` |

So the number is a **policy verdict** and a **published evidence field**, not
report-only. `docs/project/POLICY_FILES.md:74` and `docs/verification/ASSUMPTION_FILES.md:95` both
list it under "Enforced fields" — correctly, since the gates implement it; the
key is simply absent from `Concrete/`, which is what the first grep saw.

The guard that should catch this cannot fire. `check_policy.sh` fails closed when
the report has **no** max bound:

```bash
if [ -z "$actual_max" ]; then
  echo "  FAIL max_stack_bytes=$max_stack but stack-depth report has no max bound"
```

but the defect's effect is that a finite `Max stack bound:` is *always* printed —
the summary maximizes over the bounded subset, which is never empty. So the
fail-closed branch is unreachable on precisely the programs that need it, and a
budget would pass on a program with unbounded stack rather than refuse it.

Not live today, and the reason matters: all five projects that set the budget —
`crypto_verify`, `parse_validate`, `fixed_capacity`, `hmac_sha256`,
`constant_time_tag`, three of them flagships — are recursion-free
(`--report recursion` reports 0 direct and 0 mutual for each), so every enforced
number is currently correct. But that is a property of the corpus, not of the
mechanism: the first recursive function to enter a budgeted project converts a
silent pass into a published `evidence.max_stack_bytes` that is false.

Still not filed in `docs/verification/KNOWN_HOLES.md`, because no shipped claim is false
today. The judgement call is narrow and worth stating plainly rather than
burying: if a recursive function ever lands in one of those five projects before
this is fixed, this becomes an H-numbered evidence hole with no further
investigation required.

The profile gate is unaffected for the same corpus reason: `--check predictable`
refuses recursion outright (`Report.lean:585` onward), so a predictable-profile
program has no dropped edges. That is also why the defect survived — the report
is exercised most where it happens to be right.

What makes it worth a number rather than a silent fix is that `stack:` is
evidence-shaped output on a project whose stated rule is that every construct is
`proved`, `enforced`, `reported`, `assumed` or `trusted`. `8 bytes` reads as a
measured bound. `unbounded` is the honest value and the compiler has everything
it needs to say so.

## Candidate fix

Make the DFS return an unboundedness flag rather than a number, and let it
propagate: a function is unbounded if it is itself recursive **or** if any
transitive callee is. Render `stack: unbounded (calls unbounded: fact)` — naming
the reached callee, since that is the fact the reader needs. The summary must
then report `Max stack bound: unbounded` (or omit the maximum) whenever any
reachable function is unbounded, instead of maximizing over the bounded subset.

Gate: `scripts/tests/check_stack_depth.sh` does not exist — this report has no
gate at all, which is how the defect shipped. It needs the fixture above
(direct recursion, mutual recursion, and a bounded caller of each), plus a
class-level assertion that no function reported with a finite `stack:` value can
reach a function reported `unbounded`. That assertion is checkable from the
report text against `--report recursion` output, and it catches the whole class
rather than this one call shape. A mutation restoring the `!isRecursive c`
filter must make the gate fail.

The policy path needs its own leg, because fixing the report alone would trip it:
once an unbounded program prints no numeric max, `check_policy.sh`'s
`-z "$actual_max"` branch fires and the budget fails closed — which is the
correct behaviour and must be asserted deliberately rather than discovered by a
red release gate. Add a negative project that sets `max_stack_bytes` and contains
recursion, and assert `check_policy.sh` REFUSES it. Assert the same for
`check_assumptions.sh`, and that `capture_release_bundle.sh` writes something
other than a number for `evidence.max_stack_bytes` — its fallback is already
`"?"`, so confirm consumers of the bundle tolerate it, or give the field an
explicit `"unbounded"` value rather than a punctuation mark.

## Follow-up: frameless direct edges (Codex review of 19e2ab9a)

The first fix propagated recursion and indirect calls, but `computeCallDepths` still
FILTERED any callee with no frame in the report. An extern, or a direct callee the call
graph left bare because it resolved to no unique definition (an unknown or ambiguous
spelling; in the pre-mono report, a trait-method call on a type parameter), therefore read
as a leaf, and a function calling an externally implemented recursive routine published a
finite bound that all three budget consumers accepted. `stackDepthReport` now seeds every
caller of such an edge as "no bound established" and closes it backwards like recursion.
No stack bound is ever validated for those targets; compiler intrinsics are the only
exception, since the compiler lowers them and they never call back into program code.

Controls in `check_stack_budget.sh` section 5: `extern.con` and `unresolved.con` report
`unbounded` naming exactly their callers (the resolved leaf keeps its row), and
`intrinsic.con` (a `print_int` edge) keeps its exact 24-byte bound. The two
`assumptions.toml` fixtures the gate reads are now committed; a `.gitignore` rule for
compiled outputs under `tests/regressions/*/*/` had hidden them.

## Codex round 2

* **An extern spelled like an intrinsic kept a finite bound (P1).** The frameless-edge rule
  exempted every callee `resolveIntrinsic` recognizes, by SPELLING, so an explicitly declared
  `trusted extern fn wrapping_add(..)` — which Elab calls instead of the intrinsic, because a
  user/extern signature wins — published `Max stack bound: 24 bytes (main)`. Only a resolved
  intrinsic identity is exempt now: a recognized spelling that is not a declared extern, or an
  extern declared AS an intrinsic (`intrinsicDecls`). Fixture
  `tests/regressions/bug065_unbounded_stack/extern_intrinsic_name.con`, a leg of
  `check_stack_budget.sh` section 5 (unbounded, naming main.wrap and main.main); the
  `print_int` positive control keeps its 24 bytes.
