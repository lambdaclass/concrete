# parse_validate oracle (bar #5)

The 8 hand-written test cases in `src/main.con` cover one example
per error variant. They are a *floor*, not an oracle — they catch
catastrophic regressions but not subtle ones.

This directory adds a real oracle: a Python reference implementation
of the same spec, plus a differential harness that runs hundreds of
randomized cases through both implementations and asserts they
agree. The "oracle" is the independent re-derivation of the spec.

## Files

- `reference.py` — independent re-implementation of `parse_header`
  and all its validators. Reads no Concrete source; reads the spec
  from `src/main.con`'s docstring directly. Mirrors i32 wrapping
  via `& 0xFFFFFFFF`. Generates 200 cases per seed, biased toward
  every error variant boundary.
- `run_oracle.sh` — the differential harness. For each test case it
  generates a tiny Concrete driver that calls `parse_header` on the
  same input, compiles + runs, and asserts the result matches the
  reference.
- `ORACLE.md` — this file.

## What it caught

Today: nothing. The Concrete implementation and the reference agree
across 600 cases (three seeds: 0, 42, 999). That is the result you
want; it is also the result that gives you the *most* signal when a
future change breaks one of them.

## What disagreement means

If the harness fails, one of three things is true:

1. The Concrete source has a bug. Fix it.
2. The reference has a bug. Fix it.
3. The spec is ambiguous. Either tighten the spec in
   `src/main.con`'s docstring or refine the reference.

Whichever it is, the disagreement is the signal.

## Running

```sh
make test-pv-oracle
# or, for a specific seed:
bash examples/parse_validate/oracle/run_oracle.sh 42
```

## Limits and honest framing

- The reference is in **Python**, not Lean. It is not kernel-checked.
  The cleanest oracle would be a Lean reference; that is bar #2
  (composition theorem) territory and waits on ProofCore enum/struct
  extraction. Until then, two independent implementations of the
  same spec are the best oracle we have.
- 200 cases per seed is enough to surface boundary-condition bugs
  but not exhaustive. Adversarial cases bias toward variant
  boundaries; uniform random would give weaker coverage.
- The harness uses a one-shot Concrete driver per case (compile each
  case, run, throw away the binary). This is slow but mirrors a real
  use of the language. A long-running test runner that accepts inputs
  on stdin is a future improvement.
- A reference-implementation oracle is *weaker* than a Lean theorem.
  When bar #2 lands, the equivalence proof itself becomes a stronger
  oracle on the proof-relevant subset.

## See also

- `examples/parse_validate/AUDIT.md` — graduation bar #5.
- `tests/oracle/` — the Phase A semantic oracle (compiled vs
  `--interp`) at the project scope.
- Phase 1 D.6 / D.9 in `ROADMAP.md` — property-based and metamorphic
  testing as broader infrastructure.
