# Testing Strategy

**Status:** Open research direction

This note describes how Concrete's testing strategy should evolve beyond the current mix of end-to-end `lean_tests`, SSA/golden tests, and module-local `#[test]` functions.

The main shift is:

- from "does it compile / fail with the right error?"
- toward "does it preserve the right invariants under stress, malformed input, and cross-checking against simpler or external oracles?"

Concrete is now far enough along that the biggest remaining testing gains come from:

- stronger runtime and stdlib invariants
- parser and CLI robustness
- report/audit consistency
- small differential and property-style checks

## Current Shape

Concrete already has several useful layers:

- end-to-end language tests in `lean_tests/`
- SSA/golden-style backend checks
- module-local `#[test]` support for stdlib and user code
- many negative tests for type checking, borrowing, linearity, and diagnostics

This is a good base, but it still leaves important gaps.

## Main Remaining Gaps

### 1. Property-style container testing

`Vec`, `HashMap`, and `HashSet` are too important to rely mainly on example tests.

Useful properties:

- `Vec`:
  - push/pop length invariants
  - set/get round-trips
  - growth preserves existing elements
  - checked access matches bounds
- `HashMap` / `HashSet`:
  - insert/get/remove/contains invariants
  - overwrite behavior
  - collision behavior
  - tombstone/growth behavior
  - non-`Copy` value handling

These should eventually be driven by generated operation traces, not just hand-written examples.

### 2. Parser robustness and fuzzing

Now that strict LL(1) is a real language rule, parser robustness matters even more.

High-value goals:

- random malformed input should not crash the parser
- parser should not hang on strange punctuation/token combinations
- diagnostics should stay phase-correct and well-formed

The parser should be treated as a robustness surface, not just a correctness surface.

### 3. `fmt` / `parse` round-trip properties

This is one of the cleanest property-testing targets in the stdlib.

Examples:

- `parse_int(format_int(x)) == Some(x)`
- `parse_uint(format_uint(x)) == Some(x)`
- `parse_hex(format_hex(x)) == Some(x)`
- `parse_bin(format_bin(x)) == Some(x)`
- `parse_oct(format_oct(x)) == Some(x)`
- `parse_bool(format_bool(x)) == Some(x)`

Use edge values heavily:

- `0`
- negative values
- min/max values
- values around radix boundaries

### 4. Differential/codegen testing

Concrete should increasingly test not only internal consistency, but agreement with external low-level behavior.

Useful targets:

- arithmetic and comparisons
- layout-sensitive structs and enums
- `Option<T>` / `Result<T, E>` runtime behavior
- pointer and FFI edges

Possible oracles:

- C reference programs
- small Rust programs for safe cases
- simple interpreter/model code where writing one is cheap

The goal is not to build a giant differential framework immediately. The goal is to add a few high-value cross-checks where the implementation risk is highest.

### 5. Report consistency testing

Audit/report outputs are strategically important to Concrete.

That means they should be tested as first-class outputs, not treated as best-effort text.

Examples:

- if a function is trusted, the trusted boundary appears in the report
- if a function requires `Alloc`, capability reports reflect that
- if a type layout says size/alignment/offsets are X/Y/Z, emitted LLVM agrees
- if a function uses `Unsafe`, the report categorizes it correctly

These tests help keep reports aligned with semantics instead of drifting into documentation-only features.

### 6. Failure-path stdlib tests

The stdlib now has a real systems layer, so negative and failure-path tests matter more.

Examples:

- missing file
- bad path
- connection refused
- bad address
- spawn failure
- EOF behavior
- checked vs unchecked access behavior

The main principle:

- happy paths are not enough for system APIs

### 7. CLI / mode coverage

The CLI has several distinct modes and outputs now:

- normal compile/run flow
- `--emit-core`
- `--emit-ssa`
- `--report ...`
- `--test`

These should be tested as interfaces in their own right:

- correct exit status
- correct phase-specific errors
- stable/expected output shape
- bad flag combinations handled cleanly

Where report output is tested, prefer stable semantic assertions over brittle raw string snapshots. Good report tests should fail because reported meaning changed, not because harmless formatting moved a line break or heading.

## Strong Next Additions

If testing grows in phases, the best next layers are:

1. parser fuzzing
2. `fmt` / `parse` property tests
3. container trace/property tests
4. report consistency tests
5. selected codegen differential tests

That gives much better confidence than only adding more hand-written example programs.

## "Crazy" But High-Leverage Ideas

### Grammar fuzzing

Generate random token/program sequences.

Required property:

- parser never crashes or hangs

Nice-to-have property:

- diagnostics stay structured and phase-correct

### Operation-trace testing for containers

Generate random operation sequences over:

- `Vec`
- `HashMap`
- `HashSet`

Then compare final state and observable behavior against a simple model implementation.

### Metamorphic testing

Good candidates:

- inserting dead code should not change behavior
- reordering independent definitions should not change behavior
- `fmt` then `parse` preserves value
- monomorphized generic behavior matches a hand-written concrete equivalent

### Historical bug corpus

Every real bug should become a permanent regression case.

Useful classes:

- parser bugs
- layout bugs
- trusted/capability bugs
- stdlib bug repros
- diagnostics misreporting bugs

This should become a named corpus instead of an accidental pile of old tests.

## Recommended Structure

Over time, Concrete's tests should be thought of in layers:

1. parser robustness
2. checker / diagnostics behavior
3. IR / SSA / codegen correctness
4. stdlib unit and integration behavior
5. report/audit correctness
6. differential/property testing

Not every layer needs a giant framework immediately, but each layer should eventually have a clear owner and purpose.

## Recommended Order

The most useful near-term order is:

1. parser fuzzing
2. `fmt` / `parse` property tests
3. `Vec` / `HashMap` trace tests
4. report consistency tests
5. selected codegen differential tests

These fit Concrete's current risks better than simply adding many more one-off happy-path examples.
