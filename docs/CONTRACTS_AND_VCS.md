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
9. Discharge is tiered by trust cost: kernel-checked first (Lean and
   `bv_decide`), external SMT only when needed and always as a louder,
   distinct class. See [PROOF_LADDER.md](PROOF_LADDER.md) for the tiers and
   evidence classes.
10. `assume` is a controlled trapdoor: it taints its obligation to `assumed`
    (never `proved`), flows into the same audit ledger as `assumptions.toml`,
    and is gate-forbiddable in release.

**Build order — the proof teaches the syntax.** This document is a *design
proposal*; the contract syntax and VC shapes are not frozen. They must be
designed against an obligation we have actually discharged, not an imagined
one. The spec layer, the `bv_decide` tier, the refinement pattern, and a first
real proof (`block_to_words_refines_spec`) come **before** contract syntax. The
full sequence is in [PROOF_LADDER.md](PROOF_LADDER.md#build-order--the-proof-teaches-the-syntax).

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

## Proposed User-Facing Shape

This section is illustrative. The syntax is not implemented or frozen. Its
purpose is to keep the design honest about what users would write, what
obligations the compiler would generate, and what `concrete audit` would show.

No Lean tactics or theorem terms appear in `.con` files. Source code states
claims; the compiler generates obligations; Lean, kernel-checked automation,
SMT, runtime checks, or assumptions discharge or classify those obligations.

### Lean-checked proof

```concrete
spec fn ch_spec(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ ((~x) & z)
}

#[ensures(result == ch_spec(x, y, z))]
#[prove_by(lean)]
fn ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ ((~x) & z);
}
```

The compiler would generate an obligation such as:

```text
O1: extracted ch PExpr refines ch_spec
```

Audit shape:

```text
ch
  O1 ensures result == ch_spec(x,y,z)
     status: proved_by_lean
     theorem: Concrete.Proof.ch_correct
```

### Kernel-checked BitVec automation

`bv_decide` is the preferred first automation tier for bitvector leaves. It is
SMT-like automation, but the result is checked by the Lean kernel, so it does
not add an external solver to the trusted base.

```concrete
spec fn rotr_spec(x: u32, n: i32) -> u32 {
    (x >> n) | (x << (32 - n))
}

#[requires(0 <= n && n < 32)]
#[ensures(result == rotr_spec(x, n))]
#[prove_by(bv_decide)]
fn rotr(x: u32, n: i32) -> u32 {
    return (x >> n) | (x << (32 - n));
}
```

Audit shape:

```text
rotr
  O1 requires 0 <= n && n < 32
     status: proved_by_kernel_decision
     engine: bv_decide
  O2 ensures result == rotr_spec(x,n)
     status: proved_by_kernel_decision
     engine: bv_decide
```

### SMT-assisted arithmetic glue

External SMT is useful for arithmetic glue that is awkward to prove by hand and
outside the cheap kernel-decision path. It is not the same evidence class as
Lean, `omega`, or `bv_decide`, and it should not be used for facts Concrete
already enforces directly, such as ordinary fixed-array bounds.

The HMAC proof exposed the right kind of example: symbolic padding arithmetic.
The source is simple, but proving the integer division bound manually requires
bridge lemmas about `Int`, `Nat`, `BitVec`, and division.

```concrete
#[requires(0 <= len && len <= 375)]
#[ensures(1 <= result && result <= 6)]
#[prove_by(smt)]
fn sha256_block_count(len: i32) -> i32 {
    return (len + 9 + 63) / 64;
}
```

Audit shape without certificate replay:

```text
sha256_block_count
  O1 requires 0 <= len && len <= 375
     status: assumed_at_entry
  O2 ensures 1 <= result && result <= 6
     status: proved_by_smt
     solver: z3 4.13.0
     replay: none
     trust: solver_trusted
```

The counterexample path is just as important. If the postcondition is too
strong, the solver should report a source-level witness instead of a solver
term:

```concrete
#[requires(0 <= len && len <= 375)]
#[ensures(result <= 5)]
#[prove_by(smt)]
fn sha256_block_count_bad(len: i32) -> i32 {
    return (len + 9 + 63) / 64;
}
```

Audit shape:

```text
sha256_block_count_bad
  O1 ensures result <= 5
     status: counterexample
     solver: z3 4.13.0
     model:
       len: 312
       result: 6
```

If a future SMT certificate or replay path is checked by Lean, the audit must
say so explicitly:

```text
status: proved_by_smt
certificate: checked_by_lean
trust: kernel_checked
```

### Loop invariant with Lean structure and automated leaves

```concrete
spec fn diff_prefix(a: [u8; 16], b: [u8; 16], n: i32) -> u8;

contract SameTag(a: [u8; 16], b: [u8; 16], r: Int) {
    (a == b && r == 1) || (a != b && r == 0)
}

#[ensures(SameTag(a, b, result))]
#[prove_by(lean)]
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> Int {
    let mut diff: u8 = 0;
    let mut i: i32 = 0;

    #[invariant(0 <= i && i <= 16)]
    #[invariant(diff == diff_prefix(a, b, i))]
    #[decreases(16 - i)]
    while i < 16 {
        set diff = diff | (a[i] ^ b[i]);
        set i = i + 1;
    }

    if diff == 0 {
        return 1;
    }
    return 0;
}
```

Audit shape:

```text
ct_compare
  O1 invariant init: 0 <= i && i <= 16
     status: proved_by_smt
  O2 invariant init: diff == diff_prefix(a,b,i)
     status: proved_by_lean
  O3 invariant preservation
     status: proved_by_lean
     theorem: Concrete.Proof.ct_compare_loop_preserves
     leaves:
       - bounds i < 16: proved_by_smt
       - u8 xor/or fact: proved_by_kernel_decision
  O4 decreases 16 - i
     status: proved_by_smt
  O5 ensures SameTag(a,b,result)
     status: proved_by_lean
```

The intended split is that Lean proves the loop structure/refinement, while
`bv_decide` and SMT discharge the arithmetic leaves.

### Runtime-checked gradual mode

Runtime checking is a bridge for unproved claims, not a proof.

```concrete
#[ensures(result == sha256_spec(msg))]
#[check_runtime]
fn sha256_hash(msg: [u8; 64]) -> [u8; 32] {
    ...
}
```

In gradual mode:

```text
sha256_hash
  O1 ensures result == sha256_spec(msg)
     status: runtime_checked
     inserted_check: yes
```

In a stricter release mode, policy may reject it:

```text
error: release policy requires proved_by_lean for sha256_hash.O1
```

### Assumptions are loud

```concrete
#[assume(machine_level_constant_time)]
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> Int {
    ...
}
```

Audit shape:

```text
ct_compare
  A1 machine_level_constant_time
     status: assumed
     scope: function
     release_allowed: no unless policy permits
```

`assume` is intentionally not a proof mechanism. It routes into the same
assumption lifecycle and release-policy machinery as `assumptions.toml`.

### HMAC shape

```concrete
spec fn hmac_sha256_spec(
    key: [u8; 64],
    msg: [u8; 64],
    msg_len: i32
) -> [u8; 32];

#[requires(0 <= msg_len && msg_len <= 64)]
#[ensures(result == hmac_sha256_spec(key, msg, msg_len))]
#[prove_by(lean)]
fn hmac_sha256(
    key: [u8; 64],
    msg: [u8; 64],
    msg_len: i32
) -> [u8; 32] {
    ghost let expected = hmac_sha256_spec(key, msg, msg_len);

    ...
}
```

The `.con` file states the claim. The compiler generates obligations. The
audit records whether each obligation is proved by Lean, proved by
kernel-checked automation, discharged by an external solver, runtime-checked,
assumed, open, stale, or blocked.

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
- overflow under `#[overflow_checked]` and future checked/proved arithmetic
  profiles
- invalid casts
- loop bound obligations
- stack/recursion obligations where a profile claims boundedness

Loop contracts generate:

- invariant initialization
- invariant preservation
- variant nonnegative
- variant decreases
- invariant plus exit condition implies postcondition

### Schema v1 (shipped)

`Report.collectVCs` projects every obligation above into a stable, machine-readable
`VC` record (schema v1). Surface it with `concrete <file> --report vcs` (human) or
`--report vcs --json` (versioned envelope, `vc_schema_version: 1`; documented under
`envelopes.vcs` in `--report schema`). Each VC carries:

- `id` — stable key (e.g. `hmac.f#pre3`, `f@21#O2`), shared with the other reports;
- `loc` — `{file, line}` source span; `kind`; `function`;
- `hypotheses` (list) and `conclusion` — the implication the backend receives,
  split apart, not pre-joined;
- `origin` — the contract/obligation it came from;
- `dependencies` — proof links it leans on (e.g. an `ensures_proof` for a
  postcondition, a `coverage: invariant` proof for O2);
- `arith_profile` — `constant | linear | bitvector | nonlinear | refinement |
  operational | unsupported`;
- `expected_discharge` — which backend *should* own it: `constant_fold | omega |
  bv_decide | lean | smt | none`;
- `status` — the actual discharge OUTCOME, folded in after the kernel-checked
  backends run: `planned | proved_by_kernel_decision | proved_by_lean |
  arithmetic_proved | counterexample | unproven | missing`;
- `engine` — which engine produced a proof: `constant_fold | omega | bv_decide |
  lean | ""`.

The kernel-checked decision path (omega + Lean's `bv_decide`, both in-toolchain
with no TCB growth) is the only route by which a VC reaches `status:
proved_by_kernel_decision` — a class kept deliberately distinct from any future
`proved_by_smt` / `solver_trusted`. Loop-invariant *preservation* is special:
omega closes only its arithmetic half, so it can reach `arithmetic_proved` but
never a full `proved_by_kernel_decision` — the operational realization still
needs Lean. The schema is separate from discharge so a solver can never silently
turn a VC into "proved": `smt` is not a valid `engine` or `expected_discharge`
yet, and `check_vc_schema.sh` asserts it stays that way until the external-SMT
trust model (items 8–9) lands.

Example VC (JSON, schema v1) — a symbolic precondition discharged by omega from
the caller's `#[requires]`:

```json
{
  "id": "hmac_sha256.sha256_compress_at#pre20",
  "kind": "precondition",
  "function": "hmac_sha256.sha256_compress_at",
  "loc": { "file": "examples/hmac_sha256/src/main.con", "line": 284 },
  "hypotheses": ["(0 ≤ off ∧ (off + 64) ≤ 384)"],
  "conclusion": "(0 ≤ off ∧ (off + 64) ≤ 384)",
  "origin": "precondition of block_to_words_at(buf, off) in hmac_sha256.sha256_compress_at",
  "dependencies": [],
  "arith_profile": "linear",
  "expected_discharge": "omega",
  "status": "proved_by_kernel_decision",
  "engine": "omega"
}
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

### Shipped (v1 narrow slice)

The first external-SMT slice is live, deliberately narrow, and opt-in:

- **One VC class.** Only `#[overflow_checked]` no-overflow obligations whose
  operand is genuinely nonlinear (a product of two variables), not constant, and
  not closed by the interval `bv_decide` path — i.e. exactly what the
  kernel-checked tiers cannot own. `Report.overflowSmtGoals` selects them.
- **Stable SMT-LIB output.** `--report vcs --emit-smt` emits a QF_NIA query per
  eligible VC, translated from structured `Expr`s (so it is well-formed by
  construction): declare the integer vars, assert every in-scope `#[requires]`,
  assert the *negation* of the range goal, `(check-sat)`. If any hypothesis falls
  outside the SMT fragment the whole query is dropped — never emitted missing a
  constraint.
- **One solver, pinned.** `--report vcs --smt` runs Z3 (`-T:5`), reads the first
  line, and classifies: `unsat → solver_trusted`, `sat → counterexample`,
  `unknown → unknown`, timeout → `timeout`. If Z3 is absent the verdict is
  `solver_error` — an absent solver never yields a proof.
- **The boundary holds.** SMT results are folded in by `Report.foldSmtResults`
  ONLY onto a VC the kernel tiers left `unproven`, so a solver can never override
  or be confused with `proved_by_kernel_decision`/`proved_by_lean`. Without the
  flag, no VC advertises `smt` (`expected_discharge`, `engine`) or any solver
  status. `solver_trusted` means the solver is in the trusted base for that claim
  (no Lean replay yet — that is item #12). Pinned by `check_smt_path.sh` and
  `check_vc_schema.sh`; example in `examples/smt/nonlinear_overflow/`.

## Lean Integration

Lean remains the highest-confidence evidence path.

The proof registry should evolve from function-level attachment:

```json
{
  "function": "hmac.ct_compare",
  "proof": "Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct",
  "coverage": "one_direction"
}
```

to obligation-level attachment:

```json
{
  "obligation": "hmac.ct_compare.ensures.same_tag.forward",
  "proof": "Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct",
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

### Shipped: the SMT release-policy gate

The external-solver stance is enforced at build time. **SMT is useful, but
`solver_trusted` is not Lean/kernel evidence unless replayed** — so a release must
opt into trusting it:

```toml
[policy]
solver-evidence  = "forbid"        # solver_trusted is a release blocker (E0615)
# solver-evidence = "allow"        # accepted
# solver-evidence = "assumptions"  # accepted only with a named justification:
# solver-assumption = "z3-4.16-QF_NIA-trusted"
```

`Policy.enforceSolverEvidence` inspects the VC ids an external solver discharged
as `solver_trusted` during the build (`Main.computeSolverTrustedQuals`, run only
when the policy takes a stance) and rejects the build (E0615) when they are not
permitted. The stance is opt-in: with no `solver-evidence` key the build never
invokes a solver. `counterexample` / `unknown` / `timeout` / `solver_error` are
non-proofs regardless of policy and are never counted as evidence. When Lean
replay (item 12) lands, a replayed fragment graduates from `solver_trusted` to a
kernel-checked class and is no longer subject to this gate.

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
7. Let in-source proof links discharge contract/VC ids.
8. Generate runtime-safety VCs: bounds, div/mod nonzero, overflow, casts.
9. Emit SMT-LIB for the first supported VC fragment.
10. Add one solver backend with explicit trust classification.
11. Map counterexamples back to source names where possible.
12. Add loop `#[invariant]` / `#[variant]` and loop VCs.
13. Integrate contracts/VCs/solver results into `concrete audit`.
14. Add policy controls for Lean/SMT/trusted solver requirements.
15. Retrofit one flagship with source contracts.
16. Add Lean replay for simple arithmetic/bounds fragments.

## Soundness Bridge

The source-contract pipeline is justified at the kernel level by the
`Source-contract soundness (R-22..R-28)` section of `Concrete/ProofSoundness.lean`.
A small `Clause` inductive models the decidable contract fragment the VC
generators actually lower; its `eval` is the contract's intended semantics, and
the theorems prove the relationships the pipeline relies on:

- **R-22 `discharged_implies_claim` / `callsite_sound`** — the omega goal
  `∀ env, pre → post` *is* the contract claim; closing it leaves no gap.
- **R-23 `vacuous_trivializes` / `vacuous_even_proves_false`** — an unsatisfiable
  precondition makes the postcondition VC hold for *any* `post` (even `False`),
  so a vacuous contract is reported `vacuous`, never `proved` (E0613).
- **R-24 `add_requires_strengthens` / `add_requires_can_break`** and
  **R-25 `drop_ensures_compatible` / `drop_ensures_loses_guarantee`** — justify
  the `concrete diff` contract-drift classification: a `requires` conjunct added,
  or an `ensures` conjunct dropped, is a breaking change.
- **R-26 `spec_total`** — a `spec fn` is modeled as a total Lean function.
- **R-27 `Clause.evalConst_sound` / `const_false_unsat`** — the constant folder
  (vacuity constant tier, `assert(...)` VIOLATION detector) agrees with the real
  semantics in every environment, so a `some false` verdict is genuinely
  unsatisfiable, never a false alarm.
- **R-28** — source proof links carrying the same claim class as their registry
  entry is enforced operationally by the spec-drift gate (no kernel over-claim).

Anything outside the modeled fragment the real lowerer maps to `none` and reports
`unproven` — never `proved` — so the model's coverage is a lower bound on
honesty, not an over-claim.

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
