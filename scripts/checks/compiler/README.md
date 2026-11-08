# Compiler gate 9: trusted base

`Concrete/Premises.lean` owns the only component and premise list. Its rendered
component/premise rows are approved together; approval metadata never repeats the
list. Observed declarations, example closures, executable identities and pins are
separate evidence and cannot authorize themselves by updating a golden.

The structural causes of the round-2 defects were lossy text framing, basename
identity, a warm build standing in for a fresh build, source-lexer guesses about
Lean syntax, declaration-shape guesses about compiler generation, and conflation
of an editable inventory with approval authority. Gate 9 owns their removal.

## Approval authority (T1)

The coordinator must obtain a real owner signature and provision the owner's
public key as the **protected** `CONCRETE_OWNER_ALLOWED_SIGNERS` CI variable
(allowed-signers format, principal `owner`). Local runs take the path of an
external allowed-signers file in the environment variable of the same name.
Do not take this file or its authority from the branch under test.

The owner signs the exact bytes of `tests/trusted-base-approval.json` with
`ssh-keygen -Y sign -n concrete-trusted-base -f OWNER_KEY RECORD`. Commit the
resulting `.json.sig` alongside the record. The JSON schema is:

```json
{"schema":1,"registry_revision":"FULL_40_HEX_SHA","registry_sha256":"SHA256_OF_RENDERED_COMPONENT_AND_PREMISE_ROWS_WITH_NEWLINES","approval":"Owner statement authorizing this registry revision"}
```

The signature is checked against the protected signer before the revision is
used. The revision must be an ancestor available in the full CI checkout; its
registry source, flake/lock, Lean toolchain and Lake manifest must equal the
consumed inputs. Every component and premise is thereby covered by the one
revision approval, including its why/removal text. The report digest must match.
Neither `update` nor a change to the approval JSON grants authority. A missing
record/key/signature fails explicitly. No approval or signature has been invented
for this fix round: actual owner provisioning remains required before landing.
The coordinator must file the sweep row **Gate D / T1: protected owner signer and
initial registry approval**; no ROADMAP entry is edited by this worker.

## PR and scheduled evidence (X2 / N8)

The common helper accepts a build callback; gate 9 installs tracing **before**
sourcing it. There is no untraced initial build. A separate tiny, empty Lake
project compiles Lean to C/object code and links a real executable on every PR.
Its negative twin runs `objcopy` only during Lake configuration; rejection must
name that tool. The full manifest emitter/native traces follow the initial build.
Each individual syscall log is parsed before aggregation, preventing unresolved
PID state from crossing trace sessions. Failed split PATH probes admit no tool.
Relative exec/execveat targets are rejected explicitly because this tracer does
not record cwd/fd state; they can only be supported with real cwd/fd tracking.

The flake exposes exact roots for the pipeline, including Lean's fixed-output
release archive. Gate 9 evaluates those roots from the checked-in flake/lock,
queries their closure and NAR hashes, checks the consumed Lean/Lake canonical
paths and version, checks that git and the approval verifier are the lock-derived
executables, and requires every Lake checkout clean at its manifest rev.
Successful execs preserve the executed path, canonical path and store hash.
A different executable called `clang` outside that closure is refused. Local LCB
is bound to its pinned source checkout, the initial Lake build and a binary hash
which must remain unchanged through the audit. PR store identity relies on Nix's
immutable store/content-address records; it does not claim a full store rebuild
or independence from cached compiler artifacts.

`trusted_base_scheduled.yml` runs the full gate daily through
`slice0_trusted_fresh.sh`: private source clone, empty `.lake`, disabled artifact
cache, all Concrete and LCB compilation under the initial trace. On Sundays
(and manual dispatch) `slice0_trusted_store.sh` also verifies store contents and
independently rebuilds every pinned tool root with Nix's reproducibility check.
These are executable scheduled legs, not promises to add them later. Heavy
validation belongs to CI/concrete-ci-2, never a local full run on the shared host.
The coordinator's sweep row is **Gate D / N8: qualify scheduled fresh/store legs**.
The per-program `concrete facts` integration remains owned by R-0575 and the
package `[assumptions]` interface by R-0589.

## Trusted declarations and examples

`matches` names executable tools, axioms, and explanatory source/proof boundaries.
`declarations` exclusively names actual trusted Lean escape hatches. Each such
entry must resolve back to a discovered trusted declaration, with exactly one
owner of kind implementation/backend/runtime. The registry's current declaration
list is empty; future lexer/parser boundary names do not preauthorize escape
hatches. All ConstantInfo variants are checked for unsafe status, plus csimp and
partial-loop use. Lean's recorded recursion tag licenses total recursive runtime
twins; a hand-written name/shape alone does not. Partial runtime twins are audited
rather than silently exempted.

The frontend parses candidate files in their own header/import context. A raw
substring filter may only add candidates; no lexer can hide an `example` or
`partial` token. Actual example AST commands are retained as named declarations;
quoted command syntax inside ordinary declaration bodies is left intact. Both
empty/nonempty axiom closures use JSON arrays with a checked schema, and all
new example auxiliaries are checked for trusted code too. Explicit partial
modifiers are checked even when Lean optimizes a non-recursive partial definition
to a safe kernel constant. Well-founded and mutual recursion have positive controls.

Focused controls are `scripts/checks/tests/test_execve_names.py`,
`trusted_base_controls.py`, and `trusted_policy_controls.py`. The gate runs the
semantic/policy controls too. Every negative case requires its planted diagnostic,
including the fake clang identity, signature failure, trust growth, pin mismatch,
lexer-bypass example, partial loop, unsafe inductive, csimp, stale declaration and
non-code owner. Reviewers must choose the best long-term design rather than cut
corners for speed, diff size, or review-round budgets.
