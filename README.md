<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

**Concrete is an LL(1), linear, capability-oriented systems programming language
designed for auditable and formally verifiable code. Non-`Copy` values are used
exactly once, function headers expose required capabilities, and the
compiler/tooling preserve a clear path from source code to evidence.**

Concrete combines a narrow no-GC systems-language core with an evidence ledger a
reviewer can actually inspect. It puts systems control, explicit authority,
runtime-safety obligations, tests, solver results, assumptions, and Lean-checked
proofs in one toolchain without pretending they are all the same kind of
evidence.

That is Concrete's thesis: **systems control plus evidence accounting**.

## At a Glance

- **Simple syntax:** the grammar is LL(1) and checked as part of the project.
- **Linear ownership:** non-`Copy` values are used exactly once; `_` may ignore
  only `Copy` data.
- **No garbage collector:** resource lifetimes are explicit and checked.
- **Capability headers:** side effects and authority appear in function
  signatures, such as `with(Console)`, `with(File)`, and `with(Alloc)`.
- **Runtime safety:** array bounds, arithmetic traps, assertions, and
  preconditions become visible obligations or checks.
- **Formal verification path:** contracts can be connected to Lean proofs,
  kernel decision procedures, SMT runs, oracle tests, or named assumptions.
- **No single green badge:** reports keep proof, testing, runtime checking, and
  trust boundaries separate.

## A Small Example

Concrete code looks like systems code, but the authority and obligations are
part of the language surface:

```con
#[requires(0 <= off && off + 1 < len && len <= 512)]
fn read_u16_be(packet: [u8; 512], off: i32, len: i32) -> i32 {
    let hi: i32 = packet[off] as i32;
    let lo: i32 = packet[off + 1] as i32;
    return hi * 256 + lo;
}

fn report(result: i32) with(Console) {
    if result == 0 { println("ok"); } else { println("fail"); }
}
```

The first function is pure and creates bounds/arithmetic obligations. The
second function can print because it declares `with(Console)`.

In Concrete, a function can say, and the tools can report:

- what authority it needs, such as `with(Console)`, `with(File)`, `with(Alloc)`;
- what runtime risks it creates, such as array bounds, division, overflow, or
  assertion obligations;
- what was proved by Lean's kernel;
- what was discharged by kernel decision procedures such as `omega` or
  `bv_decide`;
- what was trusted to an external solver such as Z3;
- what was tested against an independent oracle;
- what remains assumed, trusted, partial, stale, vacuous, or unproven.

## Try It

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html)
(v4.28.0+) and clang.

```bash
make build
make test

.lake/build/bin/concrete examples/parse_validate/src/main.con --report effects
.lake/build/bin/concrete examples/vc_suite/fixed_point_filter.con --report vcs
.lake/build/bin/concrete examples/constant_time_tag/src/main.con --report audit
```

## Why Concrete Exists

Systems code often asks reviewers to infer too much from convention.

Concrete tries to make the important facts queryable:

```text
What can this function touch?
What can fail at runtime?
Which postconditions are actually proved?
Which facts came from Lean?
Which facts came from SMT?
Which claims are only tests?
Which assumptions are trusted?
Did a proof go stale after the source changed?
```

The answer should come from the compiler, not from a comment, a wiki page, or a
human memory of how the code was meant to be reviewed.

Concrete's design bias is deliberately conservative:

- no GC as the default runtime story;
- linear ownership: non-`Copy` values are used exactly once;
- compile-time borrowing for memory discipline;
- explicit capabilities in function headers for side effects;
- fixed arrays and explicit control flow as the easy path;
- source contracts for important claims;
- source-linked proof evidence with body fingerprints;
- audit reports that refuse to collapse different evidence classes into one
  green badge.

## Evidence, Not One Badge

Concrete reports evidence classes separately. That distinction is the product.

```text
proved_by_lean              Lean kernel checked a linked theorem
proved_by_kernel_decision   Lean-owned decision procedure closed the obligation
solver_trusted              external SMT solved it; the solver is trusted
tested_by_oracle            compiled code matched an independent reference
runtime_checked             checked dynamically or instrumented at runtime
enforced                    compiler enforced a structural property
assumed                     accepted assumption, visible in audit
trusted                     outside the proof model, named explicitly
partial                     narrower proof than the full claim
stale                       source changed after proof attachment
vacuous                     claim follows only because premise is impossible
counterexample              source-level witness refutes the claim
unproven                    obligation exists but was not discharged
```

This is why Concrete does not use "formally verified" as a single undifferentiated
badge. A Concrete report should say what was verified, which theorem, decision
procedure, test oracle, runtime check, or trusted boundary supports the claim,
and what remains outside it.

## Four Claim Shapes

The examples below are intentionally different. Concrete keeps them different.

### Lean Proof Attached To Source

`ct_compare` has a value-correctness contract: equal tags return `1`, different
tags return `0`. The proof is linked from the source.

```con
#[proof_by(Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct)]
#[ensures_proof(Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct)]
#[proof_coverage(iff)]
#[ensures((a == b && result == 1) || (a != b && result == 0))]
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32 {
    let mut diff: u8 = 0;
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        diff = diff | (a[i] ^ b[i]);
    }
    if diff == 0 { return 1; }
    return 0;
}
```

Report shape:

```text
ct_compare
  ensures equal tags return 1 and different tags return 0
    status: proved_by_lean
    coverage: iff
    theorems:
      Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct
      Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct
```

This proves value correctness in the proof model. It does not pretend to prove
machine-level timing. The constant-time source shape and CPU/backend timing
assumptions are reported separately.

### Runtime Safety Discharged In The Kernel

Array bounds and overflow obligations can be generated from ordinary systems
code and discharged without an external solver.

```con
#[overflow_checked]
#[requires(0 <= off && off + 1 < len && len <= 512)]
fn read_u16_be(packet: [u8; 512], off: i32, len: i32) -> i32 {
    let hi: i32 = packet[off] as i32;
    let lo: i32 = packet[off + 1] as i32;
    return hi * 256 + lo;
}
```

Report shape:

```text
read_u16_be
  runtime array_bounds packet[off]
    status: proved_by_kernel_decision
    engine: omega
  runtime array_bounds packet[off + 1]
    status: proved_by_kernel_decision
    engine: omega
  runtime overflow hi * 256 + lo
    status: proved_by_kernel_decision
    engine: bv_decide
```

The same VC machinery handles contracts, call-site preconditions, loop
invariants, `assert`, overflow, division, modulo, and array-bounds obligations.
Ordinary linear arithmetic and fixed-width bitvector facts stay in Lean-owned
decision procedures.

### Oracle Evidence, Not Proof

HMAC/SHA-256 is checked against independent references and vectors.

```sh
make test-hmac-oracle
```

Report shape:

```text
hmac_sha256
  reference: Python hmac/hashlib
  vectors:
    RFC 4231 TC1, RFC 4231 TC2, FIPS 180-4 SHA-256("abc")
    generated cases across key/message length regimes
  status: tested_by_oracle
  proof level: regression evidence, not proof
```

Oracle tests are useful because they compare the compiled program against an
independent implementation. They are still tests. Concrete labels them as
tests.

### External SMT With Named Trust

External SMT is useful for some nonlinear arithmetic. Concrete treats it as a
separate evidence class.

```con
#[overflow_checked]
#[requires(-30000 <= sample && sample <= 30000)]
#[requires(-30000 <= gain && gain <= 30000)]
fn scale(sample: i32, gain: i32) -> i32 {
    return sample * gain;
}
```

Report shape:

```text
scale
  runtime overflow sample * gain
    status: solver_trusted
    solver: z3
    logic: QF_NIA
    smtlib_sha: cc35e339...
    replay: z3 -T:5 vc.smt2
```

If the claim is false, the solver path reports a source-level counterexample,
not a proof:

```text
scale_unbounded
  runtime overflow sample * gain
    status: counterexample
    counterexample: sample = 99161, gain = 98166
```

External SMT is opt-in, reproducible, policy-gated, and never reported as Lean
kernel evidence unless a separate Lean replay actually checks it.

## Authority Is Visible

Concrete uses explicit capabilities for side effects.

```con
fn validate(data: Int, len: Int) -> Int {
    if len < 10 { return 1; }
    return data + len;
}

fn report(result: Int) with(Console) {
    if result == 0 { println("ok"); } else { println("fail"); }
}
```

The pure core and effectful shell are not a style convention. They are visible
in reports:

```text
validate       caps: (pure)     loops: no     evidence: enforced
report         caps: Console    loops: no     evidence: enforced
```

Concrete should let a reviewer ask "why does this need `File`?" or "which
callee introduced `Network`?" and get a compiler answer.

## Contracts, Assert, And Assume

Concrete distinguishes three common moves:

```con
#[requires(0 <= i && i < 16)]
#[ensures(result == a[i])]
fn get16(a: [u8; 16], i: i32) -> u8 {
    assert(i < 16);
    return a[i];
}
```

- `#[requires]` is a caller obligation or an entry assumption.
- `#[ensures]` is a postcondition that needs evidence.
- `assert(e);` creates an obligation.
- `assume(e);` is a trapdoor: it taints the function as `assumed`, appears in
  audit output, and can be rejected by policy.

An `assume` never manufactures proof evidence.

## Proof Authoring

Concrete proofs are source-linked. The old JSON proof registry has been
removed from examples and user-facing proof flow.

The proof workflow is binary-first:

```bash
.lake/build/bin/concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare --json
.lake/build/bin/concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare --show-obligation O2 --json
.lake/build/bin/concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare --emit-lean
.lake/build/bin/concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare --workspace
.lake/build/bin/concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare --check --json
```

The workspace is disposable build output. It contains proof context, per
obligation JSON, a Lean stub, replay commands, and the source-link attributes
that attach the finished theorem back to the Concrete function.

## Language Shape

- **LL(1) surface.** The grammar is intentionally simple and mechanically
  checked for predictable parsing.
- **No garbage collector.** Ownership and borrowing are checked at compile time.
- **Linear values.** Non-`Copy` values must be consumed exactly once; `Copy`
  is the explicit escape for ordinary duplicable data.
- **Explicit capabilities.** Side effects and authority appear in function
  signatures.
- **Fixed arrays and predictable loops.** The easy path is analyzable systems
  code.
- **Source contracts.** Important claims can live next to the function and be
  turned into proof obligations.
- **Trusted boundaries are named.** FFI, unsafe code, backend assumptions, and
  machine timing do not disappear into a green check.
- **Lean 4 substrate.** The compiler is written in Lean, and selected user
  proofs are checked by Lean's kernel.

## Examples To Read

- [examples/constant_time_tag](examples/constant_time_tag/) shows layered
  evidence: Lean proves value correctness, source shape reports constant-time
  discipline, and machine timing remains an assumption.
- [examples/hmac_sha256](examples/hmac_sha256/) is the deepest proof artifact:
  SHA-256/HMAC refinement against an independent spec, plus oracle tests against
  RFC/FIPS/Python references.
- [examples/evidence_classes](examples/evidence_classes/) is the compact
  catalog of evidence classes.
- [examples/contract_negatives](examples/contract_negatives/) shows cases that
  must not turn green: invalid contracts, unmet preconditions, vacuous claims,
  fabricated theorem names, duplicate proof links, `assert`, and `assume`.
- [examples/proof_patterns](examples/proof_patterns/) is the proof-authoring
  pattern corpus: straight-line refinement, array update, loop copy, fold,
  composition, ghost state, workspace, and repair-loop examples.
- [examples/vc_discharge](examples/vc_discharge/) and
  [examples/vc_suite](examples/vc_suite/) show the VC discharge matrix and
  end-to-end VC examples: packet windows, fixed-point filters, chunked hash
  padding, rate limits, and ring-buffer indices.
- [examples/smt](examples/smt/) shows where external SMT is useful and where
  Concrete refuses it: kernel-preferred facts, nonlinear overflow, source-level
  counterexamples, solver provenance, policy gates, replay artifacts, and
  red-team negatives.

## Reports To Try

```bash
make build

.lake/build/bin/concrete examples/parse_validate/src/main.con --report effects
.lake/build/bin/concrete examples/parse_validate/src/main.con --report proof-status
.lake/build/bin/concrete examples/parse_validate/src/main.con --report audit
.lake/build/bin/concrete examples/vc_suite/fixed_point_filter.con --report vcs
.lake/build/bin/concrete examples/smt/nonlinear_overflow/src/main.con --report vcs --smt
```

Useful report surfaces include `effects`, `contracts`, `vcs`, `audit`,
`proof-status`, `check-proofs`, `obligation-ledger`, `caps`, `authority`,
`unsafe`, `layout`, `interface`, `alloc`, `mono`, `eligibility`,
`stack-depth`, `fingerprints`, `recursion`, `consistency`, and `verify`.

## Gates To Run

```bash
make test
make test-phase1-contracts
make test-phase2-vc
make test-prove-cli
make test-proof-patterns
make test-evidence-corpus
make test-release-bundle
```

These gates are part of the point. Concrete's claims are meant to be replayed,
not trusted from prose.

## Nearby Systems

Concrete is not trying to replace Rust, Zig, Odin, SPARK, Dafny, Austral, Lean,
or C. Its claim is the composition: systems control, linear ownership, explicit
authority, source contracts, Lean-checked proof links, drift detection,
external-solver accounting, oracle evidence, and audit reports that refuse to
hide trust.

| System | What Concrete learns from it | Where Concrete differs |
| --- | --- | --- |
| Rust | Ownership, memory safety, strong tooling | Concrete is linear rather than affine-by-default, has explicit capability headers, and treats proof/evidence as part of the toolchain. |
| Zig | No hidden control flow, no hidden allocation, explicit systems control | Concrete adds linear ownership, capability signatures, and formal evidence accounting. |
| Odin | Direct systems programming and data-oriented ergonomics | Concrete rejects ambient context authority and makes effects visible in function headers. |
| C/C++ | Low-level control and ABI reality | Concrete removes undefined-behavior-shaped language holes from the safe core and reports trusted boundaries explicitly. |
| Austral | Linear types, capabilities, no GC, no implicit cleanup | Concrete adds Lean-backed proof attachment, VC generation, oracle evidence, and audit reports. |
| SPARK/Ada | Contracts, absence-of-runtime-error goals, high-assurance culture | Concrete aims at a smaller C/Rust-shaped systems core with explicit ownership and capability flow. |
| Dafny/F*/Why3 | Verification-aware programming and proof obligations | Concrete keeps the systems-language surface and separates Lean proof, SMT trust, tests, runtime checks, and assumptions. |
| Lean/Coq/Isabelle | Small trusted kernels and machine-checked proofs | Concrete uses theorem proving as evidence for systems code rather than making proof authoring the whole programming experience. |

For the longer C/Rust-oriented argument, read
[docs/WHY_CONCRETE.md](docs/WHY_CONCRETE.md).

## Building

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html)
(v4.28.0+) and clang.

```bash
make build
make test
make clean
```

## License

Concrete was originally specified and created by Federico Carrone at
LambdaClass.

[Apache 2.0](/LICENSE)
