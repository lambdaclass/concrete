# Diagnostic UX

**Status:** Open

Concrete should have diagnostics that are clear enough for normal compiler errors and for evidence-oriented workflows. The bar is closer to Elm-style helpfulness than to backend dumps.

## Why This Matters

Concrete's core claims become confusing if errors are terse:

- capability errors must explain what authority was requested and which caller lacks it
- predictable-profile failures must explain which gate failed and which operation caused it
- proof evidence must distinguish proved, missing, stale, body mismatch, obligation failure, unsupported target, and trusted assumption
- reports must point back to source, not only to compiler-internal names

## Diagnostic Shape

Prefer diagnostics with:

1. a short stable error name
2. file / line / column when known
3. a source snippet when useful
4. the violated rule
5. the immediate cause
6. why the rule exists when the rule is thesis-facing
7. one or two likely fixes

## Near-Term Targets

Start with the high-value thesis diagnostics:

1. `--check predictable` failures
2. stale proof / proof body mismatch
3. proof missing for an expected obligation
4. unbounded-loop classification
5. direct and mutual recursion cycles
6. allocation and `Alloc` capability violations
7. blocking host-operation violations
8. FFI/trusted boundary reporting
9. capability escalation errors

## Implementation Order

Do not start by rewriting every error string.

1. thread source file / line / column / span into the facts consumed by `--report effects` and `--check predictable`
2. make the predictable-profile checker point at the function and, where practical, the offending call / loop / capability declaration / extern call
3. upgrade predictable-profile errors to explain the gate, cause, profile rule, and likely fix
4. make proof-evidence reports distinguish missing, stale, body mismatch, qualified-identity mismatch, unsupported target, and obligation failure
5. add source snippets after spans are reliable
6. add machine-readable diagnostic records in parallel with human text, not after all human prose is finished

## Non-Goals For Now

- do not invent a broad diagnostic framework before the first source locations are threaded through
- do not hide precise compiler terms if they are the only accurate explanation
- do not make friendly text replace machine-readable reports; both are needed
