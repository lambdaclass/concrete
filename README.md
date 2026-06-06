<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

> Most ideas come from previous ideas. Alan C. Kay, *The Early History Of Smalltalk*

**A small systems language with linear types, no GC, and a compiler written in Lean. The same kernel that builds the toolchain can check selected theorems about your code.**

Concrete's claim is the composition: systems control and evidence accounting in
the same toolchain.

> Concrete is a small systems language for C and Rust programmers where
> authority, runtime risk, assumptions, and proof evidence are visible in the
> toolchain.

Concrete combines Rust style ownership, Zig style explicit control, Austral
style capability discipline, and Lean 4 kernel checked proofs. It is not a
proof assistant. It is a no GC systems language that Lean can reason about.
The compiler is written in Lean 4. The long term aim is to prove selected
compiler properties in the same kernel, but that work is gated. See "Where
Concrete is honest" below.

Concretely, that means:

1. **No GC systems code.** Fixed arrays, explicit control, and no hidden runtime
   story as the default.
2. **Linear ownership plus explicit capabilities.** Functions show what they
   can touch: `with(Console)`, `with(File)`, `with(Alloc)`, etc.
3. **Proofs attached to source.** A function can carry contracts and
   source linked Lean evidence. If the body changes, the proof can go stale.
4. **Multiple evidence classes, not one green badge.** Concrete distinguishes
   `proved_by_lean`, `proved_by_kernel_decision`, `proved_by_smt` with named
   solver trust, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
   `partial`, `stale`, and `vacuous`.
5. **Audit first philosophy.** The compiler should tell the reviewer what is
   proved, what is enforced, what is tested, what is assumed, and what is
   trusted.
6. **Lean as compiler and proof substrate.** The compiler is written in Lean, and
   selected user proofs are checked by Lean's kernel. Long term, compiler
   soundness work can live in the same ecosystem.

Concrete does not claim to have invented verification or ownership. Its claim
is the composition: systems control, explicit authority, contracts in source,
Lean checked proof links, drift detection, and audit reports that refuse to
hide trust.

## Four Claim Shapes

Functional correctness, proved in Lean:

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

Audit:

```text
ct_compare
  ensures equal tags return 1 and different tags return 0
    status: proved_by_lean
    coverage: iff
    theorems:
      Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct
      Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct
```

This is value correctness. The constant-time source shape and machine timing
assumptions are separate evidence, not hidden inside the postcondition.

Runtime safety, discharged by a kernel decision procedure:

```con
#[overflow_checked]
#[requires(0 <= off && off + 1 < len && len <= 512)]
fn read_u16_be(packet: [u8; 512], off: i32, len: i32) -> i32 {
    let hi: i32 = packet[off] as i32;
    let lo: i32 = packet[off + 1] as i32;
    return hi * 256 + lo;
}
```

Audit:

```text
read_u16_be
  requires 0 <= off && off + 1 < len && len <= 512
    status: assumed_at_entry
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

Regression evidence, tested against an independent oracle:

```sh
make test-hmac-oracle
```

Audit:

```text
hmac_sha256
  reference: Python hmac/hashlib
  vectors:
    RFC 4231 TC1, RFC 4231 TC2, FIPS 180-4 SHA-256("abc")
    600 generated cases across key/message length regimes
  status: tested_by_oracle
  proof level: regression evidence, not proof
```

The third class is deliberately not the same as a Lean proof or a kernel
decision. Oracle tests are valuable because they compare the compiled program
against an independent implementation, but Concrete must still label them as
tests, not proof.

External SMT, when policy allows it:

```con
#[requires(0 <= msg_len && msg_len <= 256)]
#[ensures(result <= 5)]
fn sha256_padded_blocks(msg_len: i32) -> i32 {
    return (msg_len + 9 + 63) / 64;
}
```

Audit:

```text
sha256_padded_blocks
  postcondition result <= 5
    status: proved_by_smt
    solver: z3
    trust: solver_trusted
    replay: none
```

SMT belongs to the same accounting discipline: useful, named, policy gated, and
never confused with a Lean theorem or kernel checked decision procedure unless
the result is replayed.

Other honest statuses appear in reports too:

```text
assumed       explicit assumption, not proof
trusted       outside the proof model, named in audit
partial       one direction or point proved, not the full claim
stale         source changed after the proof was linked
vacuous       contract holds only because the premise is impossible
missing       proof eligible, but no proof is linked yet
unproven      obligation generated, not discharged
violation     runtime safety failure detected
invalid       malformed or ill scoped contract expression
```

If you are coming from C or Rust and want the short "why this exists" version,
read [docs/WHY_CONCRETE.md](docs/WHY_CONCRETE.md).

## Where Concrete sits

| | Rust | Zig | SPARK / Ada | Lean 4 | Austral | **Concrete** |
|---|:-:|:-:|:-:|:-:|:-:|:-:|
| No GC | ✓ | ✓ | ✓ | ✗ | ✓ | **✓** |
| Linear types | ⚠ borrow checker | ✗ | ✗ | ✗ | ✓ | **✓** |
| Capability visible effects | ✗ | ✗ | ⚠ contracts | ✗ | ✓ | **✓** |
| Kernel checked user proofs | ✗ | ✗ | ✓ (SMT) | ✓ | ✗ | **✓ (Lean)** |
| Compiler written in same kernel | ✗ | ✗ | ✗ | n/a | ✗ | **✓** |
| Systems target | ✓ | ✓ | ✓ | ✗ | ✓ | **✓** |

The bet is the combination: no GC + linear + capability visible effects + kernel
checked user proofs + compiler written in the same kernel + systems target.

## Nearby Systems

Concrete is adjacent to several mature systems, but none has exactly this
ledger.

- **Rust** has great ownership and systems ergonomics, but proofs are mostly
  external.
- **Zig / C / C++** give excellent low level control, but little built in
  evidence tracking.
- **SPARK / Ada, Dafny, F*, Why3** have strong verification stories, usually
  SMT heavy, but they are not trying to feel like a small no GC C/Rust style
  systems language with Lean as the compiler/proof substrate.
- **Lean / Coq / Isabelle** are excellent proof systems, but writing normal
  low level systems code there is not the primary path.
- **Austral** is closer on linear types and safety, but it does not have the
  same Lean backed proof and evidence pipeline.

That is the gap Concrete is trying to occupy: systems language shape,
compiler enforced discipline, Lean backed theorem attachment, drift gates, and
evidence bundles in one toolchain.

## The Thesis

Most systems languages give you safety **or** control. Concrete is trying to make four things visible at the function boundary:

1. **what authority** a function has (capabilities)
2. **whether it allocates, blocks, recurses, or runs unboundedly** (predictable execution profile)
3. **where it crosses trust boundaries** (`trusted`, `unsafe`)
4. **whether those claims are reported, enforced, or proved**

A reviewer should be able to audit a function from its signature plus the compiler's reports, without reading the implementation and without trusting convention. That same discipline makes the code unusually legible to LLM tools. The model can ask the compiler what code is allowed to do instead of guessing from source.

## The Language

- **No garbage collector.** Memory is managed through ownership and borrowing, checked at compile time. No runtime GC, no hidden reference counting.
- **Linear type system.** Every non-`Copy` value must be consumed exactly once. Programs that leak, double-free, or use-after-move are rejected.
- **Copy vs linear.** Types are `Copy` (integers, small structs that opt in) or linear (heap-owning by default).
- **Capability based effects.** Side effects are declared in signatures: `with(File)`, `with(Console)`, `with(Alloc)`. A function with no capabilities is pure: no I/O, no allocation, no FFI.
- **Predictable execution profile.** The compiler can reject functions that recurse, allocate, block, cross FFI, or run unbounded loops. Per function, not whole program.
- **Explicit trust boundaries.** `trusted` marks code the compiler cannot fully verify (pointer arithmetic, FFI). Everything else is checked. The boundary is visible.
- **Lean backed proofs.** Selected pure functions can carry Lean 4 theorems. `make build` runs the kernel; drift in source revokes the `proved` evidence automatically.

## What This Looks Like

The first graduated flagship, [examples/parse_validate](examples/parse_validate/), is the example to read first.  Its [`README.md`](examples/parse_validate/README.md) follows the honest framing the rest of the project tries to live up to: what is proved, what is enforced (statically and by CI), what is reported, what is assumed, and **what is not yet done**.  Four later flagships ([crypto_verify](examples/crypto_verify/), [fixed_capacity](examples/fixed_capacity/), [constant_time_tag](examples/constant_time_tag/), [hmac_sha256](examples/hmac_sha256/)) reuse the same template across different systems domains.

For the minimal effects-report shape, here is [examples/thesis_demo](examples/thesis_demo/src/main.con):

```con
fn parse_byte(data: Int, offset: Int) -> Int { return data + offset; }
fn check_length(len: Int) -> Int { if len < 10 { return 1; } return 0; }

fn validate(data: Int, len: Int) -> Int {
    if check_length(len) != 0 { return 1; }
    let mut checksum: Int = 0;
    for (let mut i: Int = 0; i < len; i = i + 1) {
        checksum = checksum + parse_byte(data, i);
    }
    if checksum == 0 { return 2; }
    return 0;
}

fn report(result: Int) with(Console) {
    if result == 0 { println("ok"); } else { println("fail"); }
}

pub fn main() with(Std) -> Int {
    let result: Int = validate(42, 10);
    report(result);
    return result;
}
```

`concrete --report effects` on this file produces, today:

```
parse_byte     caps: (pure)     loops: no     evidence: proved
check_length   caps: (pure)     loops: no     evidence: proved
validate       caps: (pure)     loops: bounded evidence: enforced
report         caps: Console    loops: no     evidence: enforced
pub main       caps: File,Network,Clock,Env,Random,Process,Console,Alloc
                                              evidence: reported
```

Read the signatures. `parse_byte`, `check_length`, `validate` are pure. `report` can write to the console and nothing else. `main` has `Std` because it is the entry point. The split between bounded core and effectful shell is the point. Concrete does not pretend the whole program is predictable; it makes the boundary explicit.

## What the compiler reports

Evidence levels:

- **proved**: a linked Lean 4 theorem backs the claim.
- **enforced**: the compiler can reject violations (passes all 5 predictable gates).
- **reported**: the compiler classifies it but cannot enforce.
- **trusted assumption**: claim depends on an explicit trust boundary.

The predictable profile (`--check predictable`) rejects functions that recurse, contain unbounded loops, allocate (or declare `Alloc`), cross FFI, or block through file/network/process capabilities. Per function.

Reports that run cleanly today: `caps`, `unsafe`, `layout`, `interface`, `alloc`, `mono`, `authority`, `proof`, `eligibility`, `proof-status`, `obligations`, `stack-depth`, `fingerprints`, `effects`, `recursion`, `consistency`, `verify`.

## Research Direction: Evidence-Bearing Concurrency

Sketched as future work; not in scope for the first release. The interesting target is not Rust-style async/await but **evidence-bearing structured concurrency**:

- `with(Async)` for order-independent work that may safely run sequentially
- `with(Concurrent)` for work requiring real concurrent progress for correctness
- structured scopes; child tasks cannot outlive their parent
- linear task handles; tasks cannot leak
- owned-value transfer instead of `Send` contagion
- bounded channels, race/select, cooperative cancellation only if they fit the linear/evidence model
- deterministic simulation as a future backend, with scheduler bugs reproducible from a seed

`Async` vs `Concurrent` is the key distinction. If two operations are merely independent, sequential fallback is correct. If both must make progress or the program deadlocks, the type system should say so. "Missing concurrency" becomes a type-system issue, not a scheduler accident.

See [research/stdlib-runtime/async-concurrency-evidence.md](research/stdlib-runtime/async-concurrency-evidence.md).

## Try it

```bash
make build

# The pilot example. Read its README first.
.lake/build/bin/concrete examples/parse_validate/src/main.con --report effects
.lake/build/bin/concrete examples/parse_validate/src/main.con --report proof-status
.lake/build/bin/concrete examples/parse_validate/src/main.con --check predictable

# CI gates that enforce the showcase contract
make test-policy          # enforced budgets in Concrete.toml [policy]
make test-assumptions     # declared trust surface in assumptions.toml
make test-catches         # "Concrete catches this" negative cases
make test-verify-gates    # pass-by-pass compiler self-checks

# A simpler example
.lake/build/bin/concrete examples/thesis_demo/src/main.con --report effects
```

## Building

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.28.0+) and clang.

```bash
make build
make test
make clean
```

## License

Concrete was originally specified and created by Federico Carrone at LambdaClass.

[Apache 2.0](/LICENSE)
