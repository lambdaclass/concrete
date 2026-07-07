# SPARK-Class Assurance For Concrete

Status: design target / agent guide, not a claim that every feature exists today

This document records the assurance layer Concrete should grow toward if it
wants SPARK-class review value while staying Concrete: explicit authority,
linear/value semantics, no semantically dark constructs, and compiler-produced
evidence artifacts.

It is also an AI-facing coding guide. Claude, Codex, and other code agents
should use this file to decide which assurance annotations to suggest, which
ones are future-only, and which evidence checks must be run before claiming that
a program is proved.

For the planned LLM-guided proof-synthesis loop, see
[PROOF_SYNTHESIS.md](PROOF_SYNTHESIS.md). Agents may search for invariants,
lemmas, and proof scripts, but their output is never evidence until Concrete's
ordinary replay path and Lean's kernel check it.

## Current Foundation

Concrete already has several SPARK-adjacent ingredients:

- source contracts with `#[requires]` / `#[ensures]`;
- runtime-safety obligations for bounds, division/modulo by zero, and related
  checks;
- an obligation/evidence ledger consumed by reports and policy;
- visible capabilities such as `with(Alloc)`, `with(File)`, `with(Unsafe)`;
- explicit `trusted` boundaries;
- linear/resource-aware ownership and second-class references;
- named claim classes: proved, enforced, reported, assumed, trusted, partial,
  stale, missing, blocked.

These are the base. SPARK-class assurance adds the missing review vocabulary for
flow, frame, loop, ghost, package, and release-bundle facts.

## The Target Annotation Families

The exact syntax is not frozen. Agents must not emit these as real Concrete code
until the corresponding roadmap item is implemented and gated. They may suggest
them in design notes, comments, or migration plans as future annotations.

### 1. Preconditions And Postconditions

Current shape:

```concrete
#[requires(n > 0)]
#[ensures(result >= 0)]
fn f(n: i64) -> i64 { ... }
```

Use when a caller obligation or returned result property is local to one
function.

Do not use contracts to hide trust. If the function body cannot establish the
contract, the status must remain missing, assumed, trusted, or blocked.

### 2. Loop Invariants And Variants

Target shape:

```concrete
#[invariant(i <= len)]
#[invariant(sum == sum_prefix(xs, i))]
#[decreases(len - i)]
while i < len {
    ...
}
```

Use when a loop is the reason a bounds, overflow, cast, or postcondition
obligation cannot be discharged from local facts alone.

Agent guidance:

- First identify the loop-carried variables.
- State bounds invariants before semantic invariants.
- Add a variant/decreases clause for loops that need termination or boundedness
  evidence.
- Do not mark an invariant proved until the generated obligation checks.

### 3. Frame / Reads / Writes Contracts

Target shape:

```concrete
#[reads(input, table)]
#[writes(out)]
#[modifies(out)]
fn transform(input: &Bytes, out: &mut Buffer) { ... }
```

Use when review needs to know which state may be read or mutated. This is the
Concrete analogue of SPARK's data-flow discipline, adapted to explicit
ownership and capabilities.

Agent guidance:

- Prefer the smallest read/write set that matches the implementation.
- Do not infer permission from a capability alone. `with(File)` says the code may
  use file authority; it does not say which memory locations are read/written.
- Mutation through a scoped callback or container API must name the affected
  receiver or element range once the feature exists.

### 4. Dependency / Information-Flow Contracts

Target shape:

```concrete
#[depends(result <- input, key)]
#[depends(log <- input)]
fn verify(input: &Bytes, key: &Key) -> bool { ... }
```

Use when the important claim is not just "what is read" but "which outputs may
depend on which inputs." This is separate from secret-flow/constant-time work:
dependency contracts are a review fact; secret-flow adds security labels and
rejection rules.

Agent guidance:

- Suggest dependency facts for parsers, validators, crypto wrappers, policy
  engines, and serialization code.
- Keep dependencies source-visible. Do not invent hidden whole-program
  inference as the only explanation.
- If a dependency cannot be checked yet, classify it as reported or assumed, not
  proved.

### 5. Ghost And Spec Code

Target shape:

```concrete
ghost let old_len = len(buf);
spec fn valid_header(view: ByteView) -> bool { ... }
```

Use ghost/spec code only to express proof facts that do not affect runtime
behavior.

Agent guidance:

- Ghost code must be erased from runtime.
- Spec functions must be pure and total, or their partiality must become an
  explicit obligation.
- Never let ghost state influence a runtime branch, allocation, capability use,
  or emitted value.

### 6. Package And Import Evidence

Target shape:

```concrete
import hmac.compute requires(proved_by_lean, no Unsafe)
```

Use when a package boundary needs an evidence or authority floor. Imports do not
grant capabilities; they constrain facts about the imported interface.

Agent guidance:

- Check package/interface artifacts, not private source guesses.
- A dependency evidence downgrade must break the importing package until the
  requirement is changed or accepted.
- Treat inherited assumptions and trusted dependencies as visible facts.

## Agent Coding Rules

Agents should follow these rules when writing or reviewing Concrete code:

1. Prefer existing, implemented annotations first: `#[requires]`, `#[ensures]`,
   capabilities, `trusted`, policy files, and current reports.
2. When a proof fails because of a loop, propose an invariant/variant plan, but
   do not emit future syntax unless the roadmap item has landed.
3. When a mutation proof needs "everything else is unchanged," propose a frame
   or `modifies` fact instead of weakening the postcondition.
4. When reviewing authority, use capabilities. When reviewing state mutation,
   use future reads/writes/modifies facts. Do not conflate them.
5. When reviewing data influence, use future `depends` facts or the existing
   secret-flow roadmap item. Do not present informal prose as proof.
6. Any generated proof or assurance claim must be followed by a replay command:
   `concrete prove`, `--report contracts`, `--report obligation-ledger`,
   `check-proofs`, or the relevant gate.
7. If the compiler reports `assumed`, `trusted`, `partial`, `stale`, `missing`,
   or `blocked`, preserve that class. Do not rewrite it as "proved" in comments,
   docs, PR descriptions, or release notes.

## What To Suggest By Code Shape

| Code shape | Agent should suggest | Why |
| --- | --- | --- |
| Function rejects invalid input | `#[requires]` or `Result` return | Makes caller obligation or recoverable failure explicit |
| Function promises output property | `#[ensures]` | Ties result claim to obligation ledger |
| Loop over array/slice/buffer | invariant for index bounds, variant for progress | Discharges OOB and boundedness obligations |
| Loop accumulates result | semantic invariant over accumulator prefix | Connects loop state to postcondition |
| Function mutates a buffer/container | future `writes` / `modifies` fact | Makes frame boundary reviewable |
| Function reads globals/config/tables | future `reads` fact | Makes dependency surface reviewable |
| Output must depend only on selected inputs | future `depends` fact | Supports SPARK-style data-flow review |
| Proof helper needed but no runtime behavior | ghost/spec code | Keeps proof state erased and explicit |
| Dependency must not widen authority | import fact constraint | Keeps package trust/authority drift visible |

## Non-Goals

- Do not copy Ada syntax or package semantics.
- Do not add broad dependent types or refinement types just to look more
  formal.
- Do not trust AI-generated proofs or annotations. They are proposals until the
  compiler, Lean, solver, or gate checks them.
- Do not add hidden whole-program inference whose facts cannot be explained in
  reports.
- Do not turn Concrete into a proof assistant. The proof layer must remain an
  engineering workflow over systems code.

## Roadmap Hooks

- Phase 9: loop invariant/variant authoring UX, proof stubs, and proof repair
  plans.
- Phase 12: named SPARK-class contract layer for frame/dependency facts and
  proof classes.
- Phase 13: runtime-safety obligations discharged by loop/frame facts.
- Phase 17: certification-style assurance bundle and release claim discipline.
- Phase 18: package/import evidence summaries and authority/evidence budgets.
- Phase 19: editor/LSP diagnostics for failed invariants, frame facts,
  dependency facts, and proof obligations.
