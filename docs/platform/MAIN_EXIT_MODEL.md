# Main return & process exit model

**Status:** Stage 1 SHIPPED (2026-07-14). Stage 2 designed, workload-gated.
**Decision record** — pulled by Phase 7 workload 1 (`examples/base64_cli`,
FRICTION #4): the old runtime echoed `main`'s return value to stdout and always
exited 0, so a compiled CLI could neither keep its stdout clean when piped nor
signal failure to the shell (`&&` / `set -e` never saw errors).

## Decision of 2026-10-21: no parameters now; the return type stays `-> Int`, exit codes are an open question

**Adopted (normative).** `main` takes **no parameters**. Command-line arguments are read only
through `std.args` (`std/src/args.con`, `with(Env)`). A parameterized `main` is a compile error.
Status on main: **enforced** (bug 111, fixed 2026-10-21). Before the fix `fn main(argc: i64)`
compiled and the parameter read an undefined register (silent wrong code). Enforcing site:
`checkModule` in `Concrete/Check/Check.lean` refuses parameters on every module-level `main`
(E0310, `CheckError.mainHasParams`, whose hint names `std.args`); fixtures
`tests/programs/error_111_*.con`. One invalid fixture (`tests/invalid_programs/type.con`) moved its
parameter to a helper.

**Kept.** `fn main() -> Int` remains the entry signature, with the Stage 1 semantics below (the
value masked to 0–255; Unit exits 0; the narrower integer widths accepted). The process-exit
mapping is a separately specified operation of the entry wrapper, not an `as` cast, so the cast
decision of the same date (ARITHMETIC_POLICY §8) does not change it.

**Open decision, not adopted.** Whether to narrow `main`'s return type to the end state below
(`fn main()` and `fn main() -> u8`, so truncation is unrepresentable) is deferred to a later
surface pass. Until it is decided, the end state below is a recorded preference, not the rule, and
EXECUTION_MODEL states `-> Int` as the signature. The corpus today: `-> Int` / `-> i64` 973
signatures, `-> i32` 190, Unit 107, `-> u8` 20, `-> u64` 5, `-> u32` 2 (occurrences, 2026-10-21).

## The model (end state, Zig-shaped; a recorded preference, open since 2026-10-21)

Two accepted `main` forms, value = process exit status, runtime writes nothing:

| form | exit status |
|------|-------------|
| `fn main()` (Unit) | 0 |
| `fn main() -> u8` | the returned byte |

The OS exit-status contract is 8 bits; `u8` encodes it in the signature so
truncation is unrepresentable, not silently applied (this is Zig's `main() u8`
insight — no `ExitCode` wrapper type needed in a language that has `u8`).

**Deliberately absent: `std.process.exit()`.** Zig ships it with a "defers
don't run" warning; in a LINEAR language early exit would not be a footgun but
a hole in a checked guarantee (every non-Copy value provably consumed). Status
flows back through `main`'s return; destructors always run on the exit path.
If a workload ever genuinely cannot be written this way, that is the pull
evidence to revisit — with the cleanup-bypass semantics stated in the doc.

**Future (workload-gated): `fn main() -> Result<Unit, E>`** — Ok → 0, Err →
variant NAME to stderr + exit 1. Zig demonstrates nominal errors need no
user-supplied display machinery for this default (the compiler knows the
variant name). Do not build until error-display conventions are exercised by
real programs; the 13t buckets already tell CLI authors what to do by hand
(match, `eprintln`, return nonzero) — see `examples/base64_cli`.

## Stage 1 (SHIPPED)

`fn main() -> Int` (and the narrower int widths) now maps to the process exit
code, masked to 0–255 (`-1` exits 255, `257` exits 1 — POSIX masking, stated
here so it is documented behavior, not a surprise). Unit exits 0. Bool exits
1/0 — a deprecated corner that dies when the signature narrows. Non-scalar
returns exit 0. Stdout belongs to the program.

**Harness compatibility knob (HISTORICAL — deleted in stage 2, below):**
`CONCRETE_ECHO_RESULT=1` restored the legacy
wrapper (print result at full i64 width, exit 0). The `run_ok` fixture corpus
and the interp-vs-compiled differential fuzzer compare values wider than
8 bits, so every harness script under `scripts/tests/` exports the knob (one
grep-able line each). It is an env var rather than a CLI flag because the CLI
dispatch is exact pattern matching and a transitional knob should not multiply
its arms. **The knob is not a supported user surface** — it exists to die.

`--interp` still prints the evaluated result: the interpreter is an evaluator,
printing the value is its job. The differential harness relies on this
matching the echoed compiled output (under the knob) at full width.

**Gate:** `scripts/tests/check_exit_codes.sh` (`make test-exit-codes`) —
status propagation, 8-bit masking (negative + wide), Unit=0, clean stdout,
knob works, and the negative (no echo without the knob).

## Stage 2 (designed, not started — ROADMAP old Phase 7 follow-up)

1. Accept `fn main() -> u8` and `fn main()` as THE two entry signatures.
2. Migrate the differential fuzzer to self-printing wrappers (generated
   programs print their own result and return success) — removes the last
   full-width consumer of the echo.
3. Mechanical fixture sweep: `-> Int` mains returning 0–255 become `-> u8`;
   value-computing fixtures print instead of return.
4. Retire Int-main (deprecation diagnostic, then removal) and DELETE the
   `CONCRETE_ECHO_RESULT` knob + the per-script exports (grep for the
   marker comment).

Sequencing rationale: stage 1 is a strict prefix — the exit-code plumbing in
the main wrapper is identical; stage 2 only narrows the accepted signature and
deletes the echo. Nothing built in stage 1 is thrown away.

## Stage 2 status: COMPLETE (2026-07-17)

The knob is DELETED. Every consumer was converted:
- 119 gates de-knobbed by the remove→run→keep-if-green sweep (2026-07-16);
- the remaining 44 marker files (wide/negative-value gates, oracle
  harnesses, reducer predicates, the fuzz wrapper, meta-runners, and
  run_tests.sh itself) converted 2026-07-16/17 via the self-printing
  wrapper (`scripts/tests/lib/selfprint.sh` gate_selfprint_wrap): rename
  the fixture main, add a `with(Console, <inherited caps>)` main that
  print_int's the result at full i64 width + newline and returns 0 —
  stdout byte-identical to the legacy echo, so expectations survived
  unchanged. Unit mains (already self-reporting) pass through unwrapped.
  Every conversion was verified under BOTH env states while the knob
  still existed (run_tests exported it globally until the last flip).
- The compiler-side path (Main.echoResultEnv, Pipeline.emit's echoResult,
  EmitSSA's echo main-wrapper branch) is removed; the exit-code semantics
  above are the ONLY main-wrapper behavior.

En route the flip exposed pre-existing rot in dark (never-in-CI) runners:
test_fuzz's valid-program generator still emitted the removed `Shape#`
variant syntax, and nested_field_write's canonical-fixture check read the
test's own status on empty output. Both fixed.

Int-main deprecation is deliberately decoupled: Int-main-as-exit-code is
valid semantics, not scaffolding — it waits for a later surface pass.

## Test-harness inventory (stage 2 checklist)

Consumers of the legacy echo, all currently opted in via the exported knob:
`run_tests.sh` (`run_ok` corpus + stdlib collection checks),
`fuzz_differential.py` / `fuzz_linearity.py` (full-width interp-vs-compiled
compare), `test_golden.sh` (golden stdout includes echoed values),
`test_oracle.sh`, `check_ssa_verify_agreement.sh`, and the per-feature
`check_*.sh` gates that execute compiled fixtures. Each carries the same
one-line export with the marker comment `MAIN_EXIT_MODEL stage 1` — the
stage-2 sweep greps for it.
