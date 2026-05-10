# Verify gates

Status: contract. Operative tools:
`Concrete.Pipeline.runVerifyGates`, `concrete <file> --report verify`,
`make test-verify-gates`.

This document defines the **named pipeline boundaries** where the
compiler self-checks its own work, the structural invariant each
boundary enforces, the small set of documented exceptions, and the
never-delete rule.

Verify gates exist because the WC-0004 saga proved their value: a
Lower-stage bug emitted wrong SSA, the post-Lower verifier caught
the dominator violation before it could miscompile, and the manifest
case for it (`tests/wrong-code/cases/WC-0004`) carries the regression
forward. Pass-by-pass gates extend that discipline to every named
boundary — the goal is for the compiler to catch "it lied to itself"
bugs as early as possible, with a clear pass label, before they
become silent miscompiles.

## Named boundaries

The pipeline produces these artifacts in order:

```
parse → resolve → check → elaborate → coreCheck →
  monomorphize → lowerModule → ssaCleanupProgram → emitSSAProgram
```

A gate is a structural check at the **transition between two passes**,
verifying that the output of the upstream pass is well-formed for
the downstream pass to consume. The four currently-gated boundaries:

| Boundary | Verifier | Severity | Surface |
|---|---|---|---|
| `post-elab`    | `verifyPostElab` — no `Ty.placeholder` leaks | warning | `--report verify` (always); never blocks compile |
| `post-mono`    | `verifyPostMono` — no `Ty.typeVar`; Copy fields all-Copy | error | `Pipeline.monomorphize`; blocks compile |
| `post-lower`   | `ssaVerifyProgram` on raw `lowerModule` output | error | `Pipeline.lower`; blocks compile |
| `post-cleanup` | `ssaVerifyProgram` after `ssaCleanupProgram` | error | `Pipeline.lower`; blocks compile (catches cleanup regressions) |

`Pipeline.runVerifyGates` runs all four against a `ValidatedCore`
input and returns a `VerifyReport` with one diagnostic list per gate.
`renderVerifyGates` produces the human banner.

Earlier-pass failures (parse / resolve / check / elaborate /
coreCheck / mono refusing to even run / lowerModule erroring outright)
short-circuit the gate runner. They're caught by the standard
`Pipeline.*` error path; reporting them at this layer would
duplicate the diagnostics the frontend already produces. The gate
report covers structural invariants of artifacts that were produced
successfully — "did the pass corrupt the thing it should have built?"

## Documented exceptions

### post-elab placeholder leak via `?` and `defer`

`Ty.placeholder` is the sentinel type used during elaboration. After
elab, a placeholder leak normally means a type silently dropped
through the cracks. The verifier reports any such leak as a
**warning** (not an error) because two specific cases legitimately
survive elaboration:

1. `try_` (the `?` operator) — the error-branch expression type is
   not resolved until monomorphization instantiates the concrete
   Result/Option enum.
2. `defer` — deferred cleanup expressions can carry a placeholder
   when the deferred value's type is inferred from later context.

Both are resolved during lowering. The `--report verify` banner
classifies these as warnings, and `make test-verify-gates` tolerates
them without failing.

Promoting these from warnings to errors requires fixing `try_` /
`defer` elaboration to resolve types eagerly. Until then, the
warning surface stays visible so any *new* placeholder leak from an
unrelated pass shows up clearly against the documented baseline.

### post-mono Copy field permissibility

`verifyCopyFieldsPostMono` rejects any monomorphized `Copy` struct
whose fields are not all Copy. This is intentionally strict — there
is no documented exception. A failure means the generic struct was
instantiated with a non-Copy type at a use site that the Check pass
didn't refuse. That is a Check bug, not a Mono bug; the gate's
purpose is to catch it before Lower assumes Copy semantics.

### post-lower / post-cleanup

No documented exceptions. SSA invariants are mechanical.

## Surfaces

### Single program: `concrete <file> --report verify`

Runs all four gates on a single source. Output format:

```
VERIFY-GATES: ok (all 4 gates clean)
  post-elab    ok
  post-mono    ok
  post-lower   ok
  post-cleanup ok
```

When warnings are present:

```
VERIFY-GATES: warn (1 warning(s))
  post-elab    warn (1 warning(s))
      warning: [post-elab] try_stuff: Ty.placeholder found in expression type: _
  post-mono    ok
  post-lower   ok
  post-cleanup ok
```

When errors are present, the banner reads `VERIFY-GATES: FAIL` and
the per-gate detail includes the diagnostic lines and codes. The
process exits non-zero only on errors.

### Corpus: `make test-verify-gates`

Runs the gates across the curated corpora — the oracle vectors
(`tests/oracle/vectors.txt`) plus the wrong-code corpus's
`kind = "runtime"` entries. Asserts zero errors across all
programs. Warnings are tallied but not fatal.

Current state at first landing: 78 PASS / 0 FAIL / 0 SKIP / 2
warnings (both warnings are the documented `try_` exception in
`result_ok.con` and `result_err.con`).

## Never-delete rule

A verify gate is **never deleted**. If a gate becomes a permanent
warning warehouse (an exception class grows beyond the documented
list), the answer is to either:

- Fix the upstream pass so the leak stops happening, then tighten
  the gate from warning to error.
- Document the new exception explicitly in this file *before*
  silencing the warning surface.

Removing a gate to make the corpus "clean" hides regression risk
without measuring it. The compiler is allowed to have known
warnings; it is not allowed to forget what its own contract is.

## Adding a new gate

When a future pass joins the pipeline, the contract requires:

1. Define the structural invariant the pass's output must satisfy.
2. Implement a verifier in `Concrete/Verify.lean` (or a sibling
   module) that returns `Diagnostics`.
3. Add a field to `VerifyReport`. Extend `runVerifyGates`,
   `isClean`, `errorCount`, `warningCount`, `allDiagnostics`, and
   `renderVerifyGates` symmetrically.
4. Update `make test-verify-gates` if the gate runs across the
   corpus.
5. Document the boundary, the invariant, severity, and any
   exceptions in this file.

## See also

- `Concrete/Verify.lean` — the per-gate verifiers.
- `Concrete/SSAVerify.lean` — the SSA-level structural verifier
  invoked at the `post-lower` and `post-cleanup` boundaries.
- `Concrete/Pipeline.lean` — `runVerifyGates`, `VerifyReport`.
- `docs/WRONG_CODE_CORPUS.md` — `category = "verifier-trigger"`
  cases live here when a verify gate fires on real code.
- WC-0004 (`tests/wrong-code/cases/WC-0004.md`) — the canonical
  example of a Lower bug that the post-lower gate caught.
