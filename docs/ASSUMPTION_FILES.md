# Assumption files

Status: contract. Operative tools: `scripts/tests/check_assumptions.sh`,
`make test-assumptions`.

An **assumption file** is a machine-readable declaration of what an
example (or, later, a package) assumes about its target, runtime,
arithmetic, allocation, capability, FFI, and trust surfaces. The
example's evidence — its proofs, its reports, its release evidence
bundle — is valid **only** under those assumptions; other targets
need their own files.

The file is the source of truth for the assumption surface, not
the source code. The CI gate (`make test-assumptions`) compiles the
example, runs the relevant reports, and asserts the assumption
file's declared values match the compiler's actual output. If the
source drifts outside the assumed surface (e.g. someone adds an
allocation, or pulls in a capability), the gate fails.

Roadmap reference: Phase 2 E.24.

## Where the file lives

`<example>/assumptions.toml` at the project root (sibling of
`Concrete.toml`). One file per example. Future packages may carry
their own at the package root.

## Schema (v1)

```toml
schema_version = 1

[target]
# Host the assumptions are claimed against. The flagship's evidence
# is valid under these assumptions only. A different target needs
# its own file (or a multi-target schema, later).
host       = "x86_64-linux"     # or "any" if the example is host-independent
toolchain  = "llvm"             # llvm | (future: qbe | wasm)
runtime    = "hosted-libc"      # hosted-libc | freestanding | (future: wasi)

[arithmetic]
# Concrete's frozen arithmetic policy. See docs/ARITHMETIC_POLICY.md.
# CI does not yet enforce these (the policy reaches reports only in
# Phase 2 E.4 work). Declared here so reviewers can see what is
# assumed without inferring from source.
overflow         = "wrapping"   # wrapping | trap | undefined
divide_by_zero   = "trap"
shift_oversize   = "wrap"

[allocation]
# What the example assumes about its allocation surface.
# `heap` is enforced against `--report alloc`. If `none`, the report
# must say "No allocation activity found." If `bounded`, the report
# must show only structurally-bounded allocations (Phase 3 work).
heap             = "none"        # none | bounded | unrestricted
stack_max_bytes  = 400           # enforced against --report stack-depth max

[authority]
# Capabilities the example requires. CI asserts every function in
# `--report caps` is either pure (no caps) or its cap set is a
# subset of `required`. Capabilities in `forbidden` must not appear
# in any function.
required  = []                   # capabilities granted at entry
forbidden = ["Alloc", "File", "Net", "Unsafe", "Console", "Clock"]

[ffi]
# FFI surface. CI asserts the example's externs match this list.
# Empty means no externs are allowed.
externs   = []                   # names of foreign functions

[trusted]
# Trusted regions: explicit lists of trusted functions and shells.
# CI asserts `--report unsafe` lists exactly these.
functions = []
shells    = []

[proof]
# Proof / evidence anchors. Declarative only; no CI enforcement yet.
provable_subset = "predictable-pure-int"   # named subset, see Phase 4
trust_anchor    = "Lean kernel + Concrete.Proof.*"
```

Unknown / unspecified fields are not allowed in v1. Future schemas
bump `schema_version` and the CI gate version-checks before reading.

## Enforced fields (CI gate)

`scripts/tests/check_assumptions.sh` compiles the example and
checks the following at first landing:

| Field | Compiler surface | Failure means |
|---|---|---|
| `allocation.heap = "none"` | `--report alloc` says "No allocation activity found." | Source added an allocation. |
| `allocation.stack_max_bytes = N` | `--report stack-depth` max ≤ N | Source grew the stack past the budget. |
| `authority.required = [...]` | Every function's `--report caps` cap set ⊆ `required`. | Source widened authority beyond the budget. |
| `authority.forbidden = [...]` | No function in `--report caps` uses any forbidden cap. | Source pulled in a forbidden capability. |
| `ffi.externs = [...]` | Externs in the program match this list. | Source added or removed an FFI declaration. |
| `trusted.functions / shells = [...]` | `--report unsafe` matches. | Source added or removed a trusted boundary. |

Fields under `[target]`, `[arithmetic]`, and `[proof]` are
declarative-only at v1. They become enforced when the corresponding
compiler surfaces land (Phase 2 E.4 for arithmetic visibility in
reports, Phase 5 for target/toolchain checks, Phase 4 for
provable-subset naming).

## How to add an assumption file for a new example

1. Compile the example and run the audit reports (`caps`, `alloc`,
   `unsafe`, `stack-depth`).
2. Copy the schema above, fill in the actual values.
3. Run `make test-assumptions`. Iterate until it passes.
4. Commit the assumption file with the example. Treat any future
   drift as a real change — either update the assumption file
   deliberately or revert the source.

## How to widen an assumption

Widening an assumption (e.g. adding a capability, raising the stack
budget, allowing an extern) is a **deliberate** action. The change
shows up in `git diff` as an explicit edit to the assumption file.
That is the audit trail — reviewers can see what changed in the
trust surface, separate from what changed in the source.

Tightening an assumption (forbidding a capability, dropping the
budget) is the same: the assumption file is the contract.

## Drift behavior

A drift between the assumption file and the actual compiler reports
is treated as a real bug:

- If the source grew a capability the file forbids, the CI gate
  fails. Fix the source or widen the assumption deliberately.
- If the source dropped a capability the file required, the CI gate
  fails. Tighten the assumption.
- If the stack budget shrinks below current usage, the gate fails.
  Either reduce the stack or raise the budget.

Drift is never silent. The assumption file is the
diff-enforced contract; the source is the implementation.

## See also

- `docs/WRONG_CODE_CORPUS.md` — drift in wrong-code regressions
  follows a similar contract.
- `docs/VERIFY_GATES.md` — pass-by-pass verify gates apply the
  never-delete rule; assumption files follow the same discipline.
- `examples/parse_validate/assumptions.toml` — the first instance.
