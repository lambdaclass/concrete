<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

**Concrete is a Lean-hosted, verification-oriented systems language whose main
goal is that you can see and trust exactly what code does: its authority, its
allocation, its failure modes, its ownership, and its evidence.**

It is not "Rust but smaller" or "Go but safer." Concrete optimizes for honesty
and provability over convenience, and accepts real ergonomic costs to get them:
non-`Copy` values are used exactly once, function headers expose required
capabilities, cleanup is explicit, and the tooling keeps a clear path from source
to evidence. The thesis in one line — **systems control plus evidence
accounting**: a no-GC systems core plus an evidence ledger a reviewer can
actually inspect, one that never collapses proofs, tests, solver results, runtime
checks, and assumptions into a single green badge.

## At a Glance

- **Simple syntax:** the grammar is LL(1) and checked as part of the project.
- **Linear ownership:** non-`Copy` values are used exactly once; `_` may ignore
  only `Copy` data; cleanup is explicit (`defer x.drop()`), never an implicit
  scope-exit drop.
- **Scoped references:** safe references are second-class — they flow *down* into
  calls, callbacks, and borrow blocks, but safe APIs never return `&T` / `&mut T`.
  Accessors use scoped callbacks, owned views, or value returns. No lifetime
  parameters.
- **No garbage collector:** resource lifetimes are explicit and checked.
- **Capability headers:** side effects and authority appear in the signature —
  `with(Console)`, `with(File)`, `with(Alloc)`.
- **Runtime safety:** array bounds, arithmetic traps, assertions, and
  preconditions become visible obligations or checks.
- **Evidence, not one badge:** proofs, tests, solver results, runtime checks, and
  trusted boundaries are reported as *distinct* classes.

## A Small Example

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

The first function is pure and creates bounds/arithmetic obligations; the second
can print only because it declares `with(Console)`. From that surface the tools
answer, per function: what authority it needs, what can fail at runtime, what
Lean's kernel proved, what a decision procedure discharged, what an external
solver was trusted for, what an oracle tested, and what remains assumed, trusted,
stale, or unproven.

## A Complete Program

A whole program — the base64 CLI, Concrete's first real stdlib workload (trimmed;
full source in [examples/base64_cli/src/main.con](examples/base64_cli/src/main.con)).
Notice that every owned value is *explicitly* disposed and the `Writer` is closed
by hand: that visible cleanup is the linear-ownership pillar, not boilerplate.

```con pseudocode
mod base64_cli {
    import std.args.{count, get};
    import std.bytes.{Bytes};
    import std.io.{Writer, console_writer, eprintln, IoError};
    import std.base64.{encode, decode};

    fn main() with(Std) -> Int {
        if count() < 3 {
            let u: String = "usage: base64_cli encode|decode <text>";
            eprintln(&u); u.drop();                 // owned String, disposed
            return 1;
        }
        let cmd: String = get(1);
        let arg: String = get(2);
        let enc: String = "encode";
        let is_enc: bool = cmd.eq(&enc);
        enc.drop(); cmd.drop();

        let input: Bytes = Bytes::from_string(&arg);
        arg.drop();
        let mut w: Writer = console_writer();       // a linear Writer, must be closed
        let mut rc: Int = 0;

        if is_enc {
            let out: Bytes = encode(&input);        // from std.base64
            let r: Result<u64, IoError> = w.write(&out);
            let ignored: u64 = r.unwrap_or(0);
            out.drop();
        } else {
            // decode is symmetric: decode(&input) -> Option<Bytes>; None sets rc = 1
            rc = 1;
        }

        let closed: Result<u64, IoError> = w.close();
        let ignored2: u64 = closed.unwrap_or(0);
        input.drop();
        return rc;
    }
}
```

`with(Std)` is the entrypoint's bundled authority; `encode`/`decode` come from
`std.base64`; a bad argument is a *recoverable* failure (message + exit 1), never
a trap. The two small snippets above show the obligation and capability surfaces;
this shows how they read in a real program.

## Why It Coheres

Systems code usually asks reviewers to infer authority, failure, and ownership
from convention. Concrete makes those facts come from the compiler instead of a
comment — and the pillars are not independent features bolted together. They lock
into each other, and each is cheaper *because* of the others:

- **Second-class references make ownership provable.** No returned references
  means no aliasing to track, which means no lifetime algebra — a small,
  checkable ownership fragment. The ergonomic cost buys the proof simplicity.
- **Linear ownership plus abort-not-unwind makes cleanup simple.** Because
  Concrete aborts rather than unwinding, destruction runs only on normal control
  flow — no drop flags, no partial-initialization tracking, none of the machinery
  an unwinding language needs to keep destructors panic-safe.
- **Capabilities and linearity compose.** When a collection is explicitly
  disposed, its compiler-generated drop glue **inherits its elements' destructor
  capabilities**, derived at monomorphization — otherwise automatic destruction
  would become an invisible authority path. Two guarantees made to hold at once.
- **The interpreter and the judgment modules make the compiler verifiable.** Each
  semantic decision lives in one pure module (arithmetic, types, capabilities,
  ownership); an interpreter runs the reference semantics and is differentially
  tested against compiled output; and stage contracts catch a violation at the
  first boundary it crosses — so the pipeline stays honest enough to prove
  against.

The unifying pattern: **every design choice trades convenience for a property you
can see and check.** That is the language.

## What Concrete Deliberately Rejects

Concrete's shape is defined as much by what it refuses as by what it adds:

- **No affine implicit drop.** Non-`Copy` values do not disappear at scope exit;
  cleanup is a visible consuming action.
- **No returned safe references.** Safe references are scoped access paths, not
  lifetime-bearing values that escape through APIs.
- **No ambient authority.** Files, console, network, time, allocation, and unsafe
  operations appear in capability headers.
- **No trait objects or iterator tower.** Generic behavior is monomorphized;
  traversal is internal (`for_each`/`fold`/callbacks), not a lazy adapter stack.
- **No one-word "verified."** Proofs, tests, runtime checks, solver trust,
  assumptions, and unsafe boundaries remain separate evidence classes.
- **No hidden runtime convenience as the default story.** No GC, no unwinding
  destructors, no implicit allocation, no implicit conversions.

## Evidence, Not One Badge

Concrete reports evidence classes separately — that distinction is the product:

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

A report says *what* was verified, *which* theorem / decision procedure / oracle
/ runtime check / trusted boundary supports it, and what remains outside — never
one undifferentiated "formally verified."

## Four Claim Shapes

These are intentionally different, and Concrete keeps them different — each is
labeled with its own evidence class, and none is allowed to borrow another's
strength.

| Claim shape | Example | Discharged by | Class |
| --- | --- | --- | --- |
| Value correctness, proved | `ct_compare` (equal tags → 1, else 0) | Lean kernel checks a linked theorem | `proved_by_lean` |
| Runtime safety from ordinary code | `read_u16_be` bounds + overflow | Lean-owned decision procedures (`omega`, `bv_decide`) | `proved_by_kernel_decision` |
| Reference agreement | HMAC/SHA-256 vs RFC/FIPS/Python | an independent oracle — a test, not a proof | `tested_by_oracle` |
| Nonlinear arithmetic | `scale` overflow | external SMT (Z3): trust named, replayable, counterexample if false | `solver_trusted` |

The two proof classes read like this in a report — value correctness attached to
source, and runtime safety discharged in-kernel with no external solver:

```text
ct_compare
  ensures equal tags return 1 and different tags return 0
    status: proved_by_lean   coverage: iff
    theorems: Examples.ConstantTimeTag.Proofs.ct_compare_{same,different}_tag_correct

read_u16_be
  runtime array_bounds packet[off]        status: proved_by_kernel_decision  engine: omega
  runtime array_bounds packet[off + 1]    status: proved_by_kernel_decision  engine: omega
  runtime overflow    hi * 256 + lo       status: proved_by_kernel_decision  engine: bv_decide
```

The proved-value contract does **not** claim machine-level timing; the
constant-time source shape and CPU/backend assumptions are reported separately.
External SMT is opt-in, reproducible, policy-gated, reports a source-level
counterexample when a claim is false, and is never counted as Lean evidence
unless a separate Lean replay checks it. Worked source, report output, and replay
commands live in the examples below.

## Contracts, Assert, And Assume

```con
#[requires(0 <= i && i < 16)]
#[ensures(result == a[i])]
fn get16(a: [u8; 16], i: i32) -> u8 {
    assert(i < 16);
    return a[i];
}
```

- `#[requires]` — a caller obligation / entry assumption.
- `#[ensures]` — a postcondition that needs evidence.
- `assert(e);` — creates an obligation.
- `assume(e);` — a trapdoor: it taints the function as `assumed`, shows up in
  audit, and can be rejected by policy. An `assume` never manufactures proof
  evidence.

Authority is visible the same way: a reviewer can ask "why does this need
`File`?" or "which callee introduced `Network`?" and get a compiler answer rather
than a convention — capabilities and their sources show up in `--report caps` /
`--report authority`.

## Try It

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.28.0+)
and clang.

```bash
make build
make test
make clean
```

The daily workflow (run `concrete --help` from anywhere for the full map):

```bash
concrete file.con                 # compile
concrete run file.con             # compile and run
concrete test                     # run #[test] functions (in a project)
concrete fmt file.con --check     # format

concrete report caps file.con     # what authority does this code have?
concrete trace file.con --json    # per-stage pipeline trace (first failing phase)
concrete reduce file.con --predicate check-error   # minimize a failing program
```

Explore the evidence surfaces on the shipped examples, and run the replay gates:

```bash
.lake/build/bin/concrete examples/parse_validate/src/main.con --report audit
.lake/build/bin/concrete examples/vc_suite/fixed_point_filter.con --report vcs
.lake/build/bin/concrete examples/smt/nonlinear_overflow/src/main.con --report vcs --smt

make test-phase1-contracts   # contract negatives + snapshots
make test-phase2-vc          # VC discharge matrix
make test-prove-cli          # the prove workflow
make test-evidence-corpus    # every evidence class reports correctly
```

Report surfaces include `effects`, `contracts`, `vcs`, `audit`, `proof-status`,
`caps`, `authority`, `unsafe`, `layout`, `alloc`, `eligibility`, `fingerprints`,
`consistency`, and `verify`. Proofs are source-linked and binary-first
(`concrete prove <file> <fn> --emit-lean --workspace`); the workspace is
disposable build output holding proof context, per-obligation JSON, a Lean stub,
and replay commands. The gates are the point: Concrete's claims are meant to be
replayed, not trusted from prose.

## Examples To Read

- [examples/constant_time_tag](examples/constant_time_tag/) — layered evidence:
  Lean proves value correctness, the source shape reports constant-time
  discipline, and machine timing stays an assumption.
- [examples/hmac_sha256](examples/hmac_sha256/) — the deepest proof artifact:
  SHA-256/HMAC refinement against an independent spec, plus oracle tests against
  RFC/FIPS/Python references.
- [examples/evidence_classes](examples/evidence_classes/) — the compact catalog
  of evidence classes.
- [examples/contract_negatives](examples/contract_negatives/) — cases that must
  *not* turn green: invalid contracts, unmet preconditions, vacuous claims,
  fabricated theorem names, duplicate proof links, `assert`, and `assume`.
- [examples/proof_patterns](examples/proof_patterns/) — the proof-authoring
  corpus: refinement, array update, loop copy, fold, composition, ghost state,
  workspace, and repair-loop patterns.
- [examples/vc_suite](examples/vc_suite/) and
  [examples/vc_discharge](examples/vc_discharge/) — the VC discharge matrix and
  end-to-end VCs: packet windows, fixed-point filters, hash padding, rate limits,
  ring-buffer indices.
- [examples/smt](examples/smt/) — where external SMT helps and where Concrete
  refuses it: kernel-preferred facts, nonlinear overflow, counterexamples, solver
  provenance, policy gates, replay artifacts, red-team negatives.

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

## License

Concrete was originally specified and created by Federico Carrone at LambdaClass.

[Apache 2.0](/LICENSE)
