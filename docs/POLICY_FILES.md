# Policy files

Status: contract. Operative tools: `scripts/tests/check_policy.sh`,
`make test-policy`.

A **policy** is an enforceable budget or forbid that the project
commits to. Policies live in `Concrete.toml`'s `[policy]` section.
The CI gate (`make test-policy`) compiles the project, runs the
relevant reports, and asserts every policy is met. Violating a
policy fails CI.

Roadmap reference: Phase 2 E.25.

## Policy vs. assumption

Policy files and assumption files (`docs/ASSUMPTION_FILES.md`) look
similar but mean different things:

- An **assumption file** is declarative: "I assume X about my
  environment." The CI gate checks the example matches the
  assumption. The audience is a reviewer who wants to know what
  surface the example's evidence is valid under.
- A **policy** is prescriptive: "I commit to never doing Y." The
  CI gate checks the example never does Y. The audience is the
  release / merge process.

For pure examples like parse_validate, the two overlap heavily. The
distinction shows up in allocating flagships where the assumption
file might say `heap = "bounded"` (descriptive — I assume a bounded
allocator exists) while the policy stays silent on allocation
(prescriptive — I do not forbid it).

Practically: assumption files describe the *surface*, policy files
describe the *budget*.

## Where the policy lives

The `[policy]` section of the project's `Concrete.toml` at the
project root. One section per project. Workspace-level policy is a
later question (see Phase 9 packaging items).

## Schema (v1)

```toml
[policy]
# Predictable profile — the existing convention. Required to be
# truthy for any example claiming the predictable subset.
predictable = true

# Forbids — boolean policies that reject specific surfaces.
no_alloc      = true       # any --report alloc activity fails CI
no_unsafe     = true       # any unsafe signature fails CI
no_trusted    = true       # any trusted function or shell fails CI
no_externs    = true       # any extern declaration fails CI

# Budgets — numeric / list ceilings.
max_stack_bytes        = 400      # actual max stack must be ≤ this
forbidden_capabilities = ["Alloc", "File", "Net", "Unsafe", "Console", "Clock"]
allowed_capabilities   = []       # every function's caps ⊆ this; [] means pure-only
```

All fields are optional. An unset field is not enforced. A `false`
boolean is enforced as "not forbidden" (no-op).

## Enforced fields

| Policy | Compiler surface | Failure means |
|---|---|---|
| `predictable = true` | `concrete --check predictable` exits 0 | Predictable check refuses the project. |
| `no_alloc = true` | `--report alloc` says "No allocation activity found." | Source added an allocation. |
| `no_unsafe = true` | `--report unsafe` says "No unsafe signatures found." | Source added an unsafe signature. |
| `no_trusted = true` | `--report unsafe` lists no trusted functions or shells. | Source added a trusted boundary. |
| `no_externs = true` | Source has no extern declarations. | Source added an FFI declaration. |
| `max_stack_bytes = N` | `--report stack-depth` max ≤ N. | Source grew the stack past the budget. |
| `forbidden_capabilities = [...]` | No function in `--report caps` uses any forbidden capability. | Source pulled in a forbidden cap. |
| `allowed_capabilities = [...]` | Every function's cap set ⊆ `allowed`. Empty list means pure. | Source widened authority past the budget. |

## How to add a policy to an example

1. Compile the example. Run the audit reports
   (`alloc`, `caps`, `unsafe`, `stack-depth`).
2. Decide which budgets are real commitments vs incidental. The
   policy should say what you commit to *not* changing.
3. Add the policy lines to `Concrete.toml`'s `[policy]` section.
4. Run `make test-policy`. Iterate.
5. Commit the change. Future drift fails the gate.

## How to widen or tighten a policy

Both widening and tightening are explicit edits to the policy. They
show up in `git diff` as deliberate changes, separate from source
changes. That is the audit trail. A reviewer can see "this PR
widened the stack budget from 400 to 800 bytes" or "this PR added
`no_externs = true`" as discrete decisions.

## Drift behavior

A drift between a policy and the compiler's actual reports is a
real bug:

- If the source grew a stack frame past `max_stack_bytes`, the gate
  fails. Either reduce the frame or raise the budget deliberately.
- If the source added a capability the policy forbids, the gate
  fails. Drop the capability or remove the forbid.
- If `no_alloc = true` is set and the source allocates, the gate
  fails. Same reasoning.

Drift is never silent.

## See also

- `docs/ASSUMPTION_FILES.md` — the descriptive sibling of this file.
  Same enforcement shape, different intent.
- `docs/VERIFY_GATES.md` — pass-by-pass verify gates apply the same
  never-delete discipline.
- `docs/WRONG_CODE_CORPUS.md` — drift in wrong-code regressions
  follows a similar contract.
- `examples/parse_validate/Concrete.toml` — the first project with a
  full `[policy]` section.
