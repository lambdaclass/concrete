# Contracts, Verification Conditions, And SMT

Status: design proposal

This document defines the intended architecture for source-level contracts,
verification-condition generation, and SMT assistance in Concrete.

The design is not "add contracts" or "add SMT" as isolated features. The
architecture is:

```text
claim -> obligation -> evidence -> audit
```

Contracts are one source of claims. Capabilities, policies, runtime-safety
rules, proof registries, assumptions, and trusted boundaries are also claim
sources. The compiler lowers all of them into obligations and evidence facts
that `concrete audit`, CI, and release bundles can inspect.

## Design Principles

1. Contracts live in source.
2. Contracts create obligations; they are not guarantees by themselves.
3. Obligations are first-class compiler artifacts with stable ids and source
   spans.
4. Evidence discharges obligations.
5. Lean, SMT, tests, assumptions, enforcement, and trust are different evidence
   classes and must not collapse into one green badge.
6. SMT assists proof work but does not define Concrete semantics.
7. Audit is the user-facing truth surface.
8. The contract syntax must remain LL(1)-friendly.

## Claim Sources

A claim is something the program, compiler, project policy, registry, or user
says about code.

Examples:

- `ct_compare` returns `1` exactly when its two tags are equal.
- `a[i]` is in bounds.
- `x + y` does not overflow under the selected arithmetic profile.
- A loop terminates because its variant decreases.
- A function allocates nothing.
- A function has no `File` capability.
- Machine-level constant-time behavior is assumed for a target/toolchain.
- An extern function implements a named contract.

Contracts are only one way to introduce these claims.

## Obligation Model

An obligation is the compiler's representation of what must be shown for a
claim to hold.

Obligations should have stable machine-readable shape:

```text
Obligation:
  id: stable string
  kind: contract_postcondition | call_precondition | bounds | div_nonzero
        | overflow | cast | loop_invariant_init | loop_invariant_preserve
        | loop_variant_decrease | assumption | trust_boundary | ...
  function: qualified function name
  source_span: source range
  hypotheses: normalized facts
  conclusion: normalized fact
  dependencies: functions/contracts/obligations used by this obligation
  evidence: current evidence status
  replay: command or theorem/solver handle where available
```

The obligation layer is the center of the design. Source contracts, runtime
safety checks, policy claims, and proof registry entries all feed this layer.

## Evidence Classes

Evidence discharges or classifies obligations.

| Evidence class | Meaning |
|---|---|
| `proved_by_lean` | A Lean theorem checks against the current source/spec/fingerprint. |
| `proved_by_smt_replayed_in_lean` | SMT found a proof and the result was replayed or reconstructed in Lean. |
| `proved_by_trusted_solver` | A configured solver reported valid; the solver is part of the trusted base for this claim. |
| `enforced_by_checker` | The compiler rejects violations. |
| `tested_by_oracle` | Differential/reference/property tests exercise the claim, but do not prove it. |
| `reported` | The compiler reports the fact without enforcing or proving it. |
| `assumed` | The claim depends on an explicit assumption artifact. |
| `trusted` | The claim depends on compiler/backend/runtime/target/foreign code trust. |
| `missing` | No evidence currently discharges the obligation. |
| `blocked` | The obligation cannot yet be generated or checked because required compiler support is missing. |
| `counterexample` | A solver or checker produced a concrete violation. |
| `stale` | Evidence exists, but no longer matches the current source/spec/fingerprint. |

Do not display all successful evidence as plain "proved." The distinction is
load-bearing.

## Source Syntax

Contracts should use attribute syntax because it is parser-friendly and matches
existing Concrete style.

Function contracts:

```concrete
#[requires(i >= 0)]
#[requires(i < 16)]
#[ensures(result == a[i])]
fn get_byte(a: [u8; 16], i: Int) -> u8 {
    return a[i];
}
```

Loop contracts:

```concrete
#[invariant(i >= 0)]
#[invariant(i <= 16)]
#[invariant(diff == prefix_diff(a, b, i))]
#[variant(16 - i)]
while i < 16 {
    set diff = diff | (a[i] ^ b[i]);
    set i = i + 1;
}
```

Trusted and assumed boundaries:

```concrete
#[trusted(reason = "audited C implementation")]
#[assumes("implements RFC 6234 SHA-256 compression")]
extern fn sha256_compress(state: &mut [u32; 8], block: &[u8; 64]) with(Unsafe);
```

Contracts do not insert runtime checks by default. A `requires` clause creates
a caller obligation. If Concrete later supports runtime-enforced contracts,
they should use distinct syntax such as `#[requires_checked(...)]`.

## Contract Expression V1

The first contract language should be deliberately small.

Allowed in v1:

- parameters
- `result`
- literals
- `==`, `!=`, `<`, `<=`, `>`, `>=`
- `&&`, `||`, `!`
- simple `+`, `-`, `*`
- fixed array lengths and `len(x)` where meaningful
- array indexing only when a bounds obligation can be generated
- named pure predicates that are explicitly admitted

Deferred:

- `old(x)`
- quantifiers
- implication / iff as dedicated syntax
- allocation
- mutation inside contract expressions
- loops in contract expressions
- capability calls
- FFI calls
- arbitrary function calls
- unbounded recursion

Dedicated `iff` syntax is not required for v1. The parser can use equality over
booleans first:

```concrete
#[ensures((result == 1) == bytes_eq(a, b))]
```

Named predicates should be admitted only when they are pure and have their own
proof/evidence story.

## Contract IR

After parsing and checking, contracts lower into stable artifacts.

Example:

```text
Contract:
  id: hmac.ct_compare.ensures.same_tag
  function: hmac.ct_compare
  kind: ensures
  expr: (result == 1) == bytes_eq(a, b)
  source_span: examples/hmac_sha256/src/main.con:42
  dependencies: [hmac.bytes_eq]
```

Contract ids must appear in:

- `--report contracts`
- `--report obligations`
- `--report audit`
- proof registries
- release bundles
- CI drift gates

## Verification Conditions

Contracts and runtime-safety rules generate VCs.

Function contracts generate:

- callee precondition obligations at call sites
- postcondition obligations at returns
- dependency obligations for named predicates

Runtime safety generates:

- array index bounds
- division/modulo nonzero
- overflow under checked/proved arithmetic profiles
- invalid casts
- loop bound obligations
- stack/recursion obligations where a profile claims boundedness

Loop contracts generate:

- invariant initialization
- invariant preservation
- variant nonnegative
- variant decreases
- invariant plus exit condition implies postcondition

Example VC:

```text
VC:
  id: hmac.ct_compare.loop.i_le_16.preserve
  kind: loop_invariant_preservation
  hypotheses:
    i >= 0
    i <= 16
    i < 16
  conclusion:
    i + 1 <= 16
  evidence: proved_by_trusted_solver
```

## SMT Assistance

SMT is an assistant for boring obligations:

- linear arithmetic
- boolean facts
- bounds checks
- simple no-overflow side conditions
- loop-index invariants
- counterexample generation

The first SMT backend should be explicit and replayable:

1. emit SMT-LIB for each supported VC,
2. run one configured solver behind a policy/flag,
3. classify the result,
4. include the solver command/configuration in audit artifacts,
5. treat timeouts and unknowns as non-proofs.

Solver result classes:

- `proved_by_trusted_solver`
- `proved_by_smt_replayed_in_lean`
- `smt_unknown`
- `smt_counterexample`
- `smt_timeout`
- `unsupported_theory`
- `solver_error`

SMT must not silently mark an obligation as `proved_by_lean`. If replay into
Lean is not available, the solver is part of the trusted base for that claim.

## Lean Integration

Lean remains the highest-confidence evidence path.

The proof registry should evolve from function-level attachment:

```json
{
  "function": "hmac.ct_compare",
  "proof": "Concrete.Proof.ct_compare_same_tag_correct",
  "coverage": "one_direction"
}
```

to obligation-level attachment:

```json
{
  "obligation": "hmac.ct_compare.ensures.same_tag.forward",
  "proof": "Concrete.Proof.ct_compare_same_tag_correct",
  "coverage": "universal_one_direction"
}
```

A Lean theorem may discharge:

- a function postcondition,
- one direction of a postcondition,
- a loop invariant,
- a runtime-safety VC,
- a semantic equivalence claim,
- or a helper predicate used by other obligations.

## Audit Output

`concrete audit` should be the single user-facing truth surface.

Example shape:

```text
Function: ct_compare

Contracts:
  ensures (result == 1) == bytes_eq(a, b)
    status: partial
    positive direction: proved_by_lean
    negative direction: missing

Runtime obligations:
  a[i] bounds: proved_by_trusted_solver
  b[i] bounds: proved_by_trusted_solver
  loop invariant i >= 0: proved_by_trusted_solver
  loop invariant i <= 16: proved_by_trusted_solver
  loop variant 16 - i decreases: proved_by_trusted_solver

Capabilities:
  none

Allocation:
  none

Assumptions:
  machine-level constant-time: assumed
  LLVM timing preservation: trusted

Overall:
  evidence: mixed
  provable_v1: full
```

The audit should make mixed evidence normal. The goal is not a universal green
badge; the goal is no hidden claim weakening.

## Policy Controls

Projects should be able to choose how much automation/trust they accept:

```toml
[verification]
allow_trusted_solver = true
require_lean_for_public_contracts = true
solver_timeout_ms = 500
```

Possible policies:

- require Lean for public contracts,
- allow trusted-solver evidence for runtime safety only,
- reject solver evidence unless replayed in Lean,
- require all generated VCs to be discharged before graduation,
- allow oracle evidence only for explicitly empirical claims.

## Threat Model

The SMT/contract path adds new trust surfaces.

Trusted or assumed components may include:

- Lean kernel,
- Concrete compiler pieces not yet proved,
- SMT solver binary when trusted-solver mode is enabled,
- SMT encoding correctness,
- LLVM/backend/toolchain,
- runtime/OS/hardware behavior,
- extern/FFI bodies,
- trusted wrappers.

The audit and release bundle must name which of these affect a given claim.

## Implementation Order

1. Write this design doc and keep it linked from the roadmap.
2. Parse and store `#[requires]` / `#[ensures]` on functions.
3. Add the v1 contract expression parser/checker.
4. Lower contracts into stable Contract IR with ids, spans, and dependencies.
5. Add `--report contracts`.
6. Generate tiny VCs for pure no-loop contracts: call-site preconditions and
   return-site postconditions.
7. Let proof-registry entries discharge contract/VC ids.
8. Generate runtime-safety VCs: bounds, div/mod nonzero, overflow, casts.
9. Emit SMT-LIB for the first supported VC fragment.
10. Add one solver backend with explicit trust classification.
11. Map counterexamples back to source names where possible.
12. Add loop `#[invariant]` / `#[variant]` and loop VCs.
13. Integrate contracts/VCs/solver results into `concrete audit`.
14. Add policy controls for Lean/SMT/trusted solver requirements.
15. Retrofit one flagship with source contracts.
16. Add Lean replay for simple arithmetic/bounds fragments.

## Relationship To Existing Docs

- `docs/PROVABLE_V1.md` defines the current subset that can carry
  Lean-backed value-semantics evidence.
- `docs/PROOF_STORY_MATRIX.md` is the language-level no-dark-constructs
  inventory.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` tracks ProofCore extraction and
  soundness obligations.
- This document defines the future source-contract, VC, and SMT architecture
  that will feed the same evidence/audit system.

## Non-Goals For V1

- A full dependent type system.
- Arbitrary executable contracts.
- Hidden runtime checks inserted by contract syntax.
- Treating SMT as equivalent to Lean.
- Proving backend binary behavior.
- Full side-channel security.
- Whole-language verification.

Concrete should grow toward SPARK-like automation while preserving its own
identity: a no-GC systems language with linear/capability-aware code,
Lean-backed evidence, explicit assumptions, and audit output that refuses to
hide what is trusted.
