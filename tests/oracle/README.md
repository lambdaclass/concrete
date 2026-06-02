# Semantic-oracle differential corpus (Phase A.1)

This directory holds the test vectors for the compiled-vs-interpreted
differential harness. The harness lives in
`scripts/tests/test_oracle.sh`.

## Why

Concrete has a source-level interpreter (`Concrete/Interp.lean`,
invoked via `concrete <file> --interp`). It is intentionally smaller
than the full compiler pipeline and serves as a semantic oracle for
the predictable/core subset.

Phase A.1 of the roadmap requires that this oracle be a *regression
surface*, not a one-off prototype: every supported program should run
through both paths and produce the same observable result on every
build.

Phase A.2 — the trust boundary of the interpreter — lives in
[`docs/INTERPRETER_TRUST.md`](../../docs/INTERPRETER_TRUST.md).
It enumerates supported constructs, excluded constructs, the
arbitrary-precision-integer arithmetic model, and the explicit-failure
contract that lets the harness distinguish PENDING from FAIL.

## Contract

The harness only handles `fn main() -> Int`. For an int-returning
main:

- The compiled binary's main wrapper formats the return value as
  `%lld\n` and prints it to stdout. See `Concrete/EmitSSA.lean`'s
  `emitMainWrapper`.
- `--interp` calls `Interp.interpret` and `IO.println`s the int
  return value (`Main.lean`).

So both paths print `<value>\n` for the same program, and the harness
compares trimmed stdout exactly.

Bool, void, struct, and other return types are *not* on the contract
yet — both paths print different shapes (`true`/`false` vs `0/1`,
empty vs `0`, etc.). Add them only when the contract is widened
deliberately.

## Vectors

`vectors.txt` is the corpus. One vector per non-blank,
non-comment line. Add a vector by appending its path; the harness
picks it up on the next run.

A vector that the interpreter does not yet support produces an
explicit `interp: <reason>` diagnostic. The harness records these as
**PENDING**, not failures. Each PENDING line is a Phase A.3
work item: extend the interpreter until the gap is gone.

## Running

```sh
make test-oracle           # standalone
make test-full             # includes the oracle inside the interp section
```

The harness exits non-zero only on real mismatches or missing files.
PENDING entries do not fail the run.

## Adding a vector

1. Pick a `.con` whose `fn main() -> Int` is observable (compiled
   binary prints the integer).
2. Append the path to `vectors.txt`.
3. Run `make test-oracle`. If the program lands as PENDING, that is
   fine — it now tracks an interpreter gap as a regression target.
4. If the program produces a real mismatch, **do not paper over it**.
   The mismatch is a semantic regression. Either the compiler or the
   interpreter has the wrong answer; fix the wrong one.

## Failure modes

- `FAIL ... compilation failed` — frontend rejected the source. Fix
  the program or remove the vector.
- `FAIL ... compiled='X' interp='Y'` — semantic disagreement. This is
  what the harness exists to catch. Triage:
  1. Decide which value is correct from the language spec / source.
  2. Fix the compiler or the interpreter, not the test.
  3. Land a small named regression .con in `tests/oracle/` if the
     mismatch came from a new construct not previously covered.
- `FAIL ... --interp exited N` — interpreter crashed. File a bug; do
  not silence with PENDING.
