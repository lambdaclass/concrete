# Reducer / minimizer workflow

Status: contract. Operative tools:
`scripts/tests/minimize_wrong_code.sh`, `scripts/reduce/expect-*.sh`,
`Concrete.Reduce` (in-tree shrinker).

This is the path from "I just hit a compiler bug" to "the bug is a
manifest entry in `tests/wrong-code/`." Roadmap reference: Phase D
items D.14 (reducer/minimizer workflow) and D.16 (named regression
corpus).

## When to reduce

Reduce when **any** of these is true:

- The bug surfaced from a real program — a flagship example, a
  stdlib test, a workload — and the program is too big for a future
  reader to load into one screen.
- The bug surfaced from generated code (fuzzer, adversarial sweep,
  metamorphic tester) and the input has no narrative obligation to
  remain large.
- The same symptom keeps appearing in slightly different programs
  and you suspect a single underlying cause.
- A future fix is going to land in the corpus as a regression and
  the manifest case will be read by humans.

Don't reduce when:

- The bug already has a one-screen repro that exercises the right
  language feature.
- The "bug" is actually a feature interaction that is interesting
  precisely because of the combination — over-minimizing collapses
  it into a different bug.
- The cost of reducing exceeds the cost of fixing. Some bugs are
  obvious from the first occurrence and minimization is busywork.

## What counts as a good minimized repro

A reduced case is good when **a future reader can read it end-to-end
in one sitting and know exactly which language features the bug
needs to express itself**. Concretely:

- Smaller is better, but not at the cost of clarity. A 50-line repro
  that uses only the relevant constructs beats a 30-line repro that
  mixes five unrelated features to hit a smaller character count.
- Names should be neutral (`x`, `y`, `Resource`, `Pair`) unless the
  domain matters to the bug.
- Comments at the top should state: the symptom (one sentence), the
  expected behavior, and what was reduced from.
- Every construct in the repro should be necessary. If you can
  delete a `let`, a function, a struct field, or a match arm and the
  predicate still holds, do it.

## When NOT to over-minimize

Stop reducing when:

- The remaining program no longer demonstrates the bug at the level
  you care about. Example: a miscompile in match-arm scoping that
  reduces to "match with one arm" loses the cross-arm shape that
  was the entire point.
- The repro becomes simpler than the surface language guarantee
  it's testing. A `let`-shadowing bug that reduces to a no-shadow
  program is not the same bug.
- You have to introduce dead code or no-op constructs to keep the
  bug alive. That's a sign the reducer escaped the bug's actual
  cause.

For ambiguous cases, keep the larger form and add a `## Why this
size` paragraph to the case notes.

## How to preserve expected behavior

The reducer drives shrinking with a **predicate**: a function that
returns true iff the candidate still demonstrates the bug. A
correctly-stated predicate is what lets the reducer hill-climb safely.

Four high-level predicate kinds, all callable through
`scripts/tests/minimize_wrong_code.sh`:

- `error-code:<E####>` — the candidate must compile-fail with a
  diagnostic containing that code. Use for verifier failures, type
  errors, lint rejections.
- `runtime-output:<EXPECTED>` — the candidate must compile AND its
  binary's trimmed stdout must equal `<EXPECTED>` (use `\n` for
  multi-line). Use for miscompiles whose symptom is a wrong runtime
  value.
- `oracle-mismatch` — the candidate must compile, run, and produce
  stdout that differs from `--interp` stdout (with neither side
  raising a PENDING `interp:` diagnostic). Use when the Phase A
  oracle harness surfaces a real semantic disagreement.
- `report-contains:<KIND>:<SUB>` — `concrete <candidate> --report
  <KIND>` output must contain `<SUB>`. Use for fact / report
  contradictions and authority / proof / capability surface bugs.

Predicate scripts live under `scripts/reduce/`. Each takes the
candidate path as the final argument and returns 0 (still triggers)
or 1 (no longer triggers).

A weaker predicate gives a smaller repro but risks over-minimization
(the reduced form triggers a *different* bug). A stronger predicate
keeps the bug intact but converges slower. When in doubt, start
strict: `runtime-output:<the exact wrong value>`, not just
`runtime-output:0`.

## How to drive the reducer

```sh
# Compile-error reduction
scripts/tests/minimize_wrong_code.sh foo.con \
    --predicate error-code:E0708 \
    -o foo.reduced.con \
    --verbose

# Wrong-runtime reduction
scripts/tests/minimize_wrong_code.sh foo.con \
    --predicate runtime-output:42

# Oracle-mismatch reduction
scripts/tests/minimize_wrong_code.sh foo.con \
    --predicate oracle-mismatch

# Report-contradiction reduction
scripts/tests/minimize_wrong_code.sh foo.con \
    --predicate report-contains:caps:Alloc
```

The wrapper:

1. Verifies the predicate holds on the original source. If it
   doesn't, refuses to reduce — the predicate is wrong, not the
   program.
2. Translates the high-level predicate into a `concrete reduce
   --predicate external:<scripts/reduce/...>` invocation.
3. Writes the reduced source to `<source>.reduced` (or `-o
   <output>`).

The shrinking engine itself is `Concrete.Reduce`: syntax-aware
passes that remove top-level items, then statements, then match
arms, then else branches, in a fixpoint loop.

## How reduced cases enter the corpus

Once the reducer has produced a stable minimal form:

1. Decide where the repro lives. New bugs that have no other home
   land under `tests/wrong-code/cases/WC-NNNN/program.con`. Bugs
   that fit an existing test convention may go under
   `tests/programs/bug_*.con` or
   `tests/programs/adversarial/<area>/<name>.con`.
2. Append a manifest entry to `tests/wrong-code/manifest.toml`. See
   `docs/WRONG_CODE_CORPUS.md` for the schema. `kind` follows the
   predicate that drove the reduction:
   - `error-code:E####` predicate → `kind = "compile-error"`,
     `expected = "E####"`.
   - `runtime-output:V` predicate → `kind = "runtime"`,
     `expected = "V"`.
   - `oracle-mismatch` and `report-contains` predicates currently
     have no manifest kind; until they do, register the case as
     either `runtime` (against the expected value the bug should
     produce after fix) or `compile-error`, and mention the
     predicate in the notes.
3. Write the per-case notes file. Include the **reducer command**
   so the next person can replay the reduction:
   ```
   ## Reduction
   Reduced from `<original>` with:
       scripts/tests/minimize_wrong_code.sh <original> \
           --predicate <KIND>:<ARG>
   Original was N lines, reduced to M lines.
   ```
4. Run `make test-wrong-code` and confirm the new entry behaves as
   expected.

## Smoke test

`scripts/tests/test_reducer_smoke.sh` exercises each predicate
against known inputs and verifies they distinguish pass / fail. It
does not run a long minimization — the reducer engine itself is
exercised indirectly by the wrong-code corpus.

## See also

- `docs/WRONG_CODE_CORPUS.md` — the corpus contract.
- `tests/wrong-code/README.md` — operations quickstart.
- `Concrete/Reduce.lean` — the in-tree shrinker.
- `scripts/reduce/expect-*.sh` — predicate scripts.
- `scripts/tests/minimize_wrong_code.sh` — the wrapper.
