# Why3: architecture to copy, and why Concrete still exists

**Status:** design note (2026-07-29). Companion to
[`lean-vs-rocq-tradeoffs.md`](lean-vs-rocq-tradeoffs.md) and the influence entry in
[`../INFLUENCES.md`](../INFLUENCES.md).

Why3 is the closest existing system to Concrete's proof-backend ambition, so it forces
the sharpest version of the question: **why build Concrete if Why3/WhyML already exists?**
This note answers that honestly, and records what to copy from Why3's architecture.

---

## What Why3 is

A production platform for deductive verification — the engine under **Frama-C** (C) and
**SPARK** (Ada, shipped in avionics/rail). You write programs + specs (WhyML), it computes
weakest-precondition VCs, and dispatches each obligation to *many* provers (Z3, cvc5,
Alt-Ergo, and ITPs: Coq, Isabelle, PVS) through a shared neutral layer. WhyML extracts to
OCaml so verified programs can run.

So on the surface Why3 = "verified programs, multi-prover, extractable." That overlaps a
lot with Concrete, which is exactly why the positioning has to be precise.

## Concede first: what Why3 already wins

- **Multi-prover dispatch is NOT a justification for Concrete.** The prover-neutral
  obligation IR, drivers, and sessions are Concrete *catching up to* Why3, not surpassing
  it. If that were the whole pitch, we should build on Why3, not rebuild it.
- For **functional/algorithmic code extracted to OCaml**, Why3 is strictly better.
- Why3 is mature and battle-tested; Concrete is Phase 7 research.

The bar to justify a new language + compiler + proof system over adopting Why3 is high,
and "it also does multi-prover" does not clear it.

## Why Concrete exists anyway: two axes Why3 cannot occupy

### 1. Substrate — systems code, not ML
WhyML is a GC'd, higher-order ML that extracts to OCaml-with-a-runtime; its memory story
is regions/aliasing *for the prover*, not a systems operational model you ship. Concrete
is a no-GC systems language: linear ownership, capabilities-as-effects, explicit
`defer drop`, whole-program monomorphization, predictable LLVM/QBE lowering, visible
byte/text boundaries. For *"verified low-level code I ship as a lean binary with C-like
control,"* WhyML's OCaml/GC extraction is the wrong target. On this axis Concrete's real
competitor is Rust/Zig + verification, not Why3.

### 2. The evidence ledger — graded, first-class, composing
Why3's outcome is essentially binary: a VC is discharged (by a prover you trust, through
drivers you trust) or not. Concrete's thesis is **evidence accounting**: every construct
is `proved` / `enforced` / `runtime_checked` / `tested_by_oracle` / `solver_trusted` /
`assumed` / `trusted` — distinct classes, never one "verified" badge — that travel with
the code, track staleness (`stale`/`unbound`/`depsNotCurrent`), and propagate (a claim
reaching a `trusted` boundary cannot launder into `proved_by_lean`). Why3 has no such
notion. It answers "is it proved?"; Concrete answers "exactly what do we know, by what
means, what is still assumed/trusted, and is it still current?"

### Supporting deltas
- **Enforced-by-construction, not just proved.** Ownership/effect guarantees are enforced
  by Concrete's type system (`enforced`), so they never become solver obligations. Why3
  proves; Concrete moves work to "can't express the violation."
- **Smaller, more honest TCB ambition.** Why3 trusts its VC generator, transformations,
  drivers/printers, and OCaml extraction — a large, largely-unverified TCB. Concrete aims
  to prove its own lowering sound and certificate-check solvers. (Honest caveat: the
  Concrete compiler binary is itself trusted + differentially tested — not zero — but the
  *direction* is a shrinking TCB, opposite Why3's structural trust.)

## Copy the ideas, don't build on Why3 — and the GC is not the main reason

"Use Why3" can mean two different things, and only one is blocked by the GC:

1. **Why3 as execution substrate** (WhyML → extract to OCaml). Ruled out by GC — but
   Concrete already owns its LLVM/QBE codegen, so it was never going to use this.
2. **Why3 as a proof-dispatch backend** (feed it obligations, it fans out to provers).
   **The GC does *not* rule this out** — proving a VC never runs the program, so there is
   no GC anywhere in the prove path.

So the real reason to reimplement Why3's ideas rather than depend on Why3-the-tool is the
**TCB thesis**, not the GC: depending on Why3 imports its trusted, unverified
drivers/transformations — exactly the trusted surface Concrete is trying not to have — and
gives no place for the evidence ledger. Concrete copies the *architecture* but owns the
*implementation* so the trust story and the evidence model are its own.

**Pragmatic nuance:** Why3 can still serve as a *temporary scaffold* — point obligations at
it early for fast multi-prover coverage, and use it as an **oracle to differential-test**
Concrete's own dispatch — then drop the dependency. Maturity benefit during bootstrap, no
permanent trusted dependency.

## The reframe

It is not *Concrete vs Why3*. The right architecture is **Concrete *above* a Why3-style
layer**: Concrete's value is the frontend (systems language + evidence ledger); the
backend (obligation IR, transformations, driver-per-prover dispatch) is what Why3 solved
and what Concrete should steal.

## Architecture to copy (concrete list)

| Why3 concept | Copy as | Concrete anchor |
|---|---|---|
| **Task** (context + one goal) | prover-neutral obligation unit | `NeutralObligation` (see the design in `concrete-design.md`) |
| **Transformations** (composable, per-backend) | named lowering passes, driver-selected | generalize `toLeanProp`/`exprToSmt` |
| **Driver** (declarative per-prover data) | printing + transforms + built-ins + command + result regexp | `obBinOpLean`/`obBinOpSmt` table → full driver files |
| **Session** (shape + checksum) | staleness re-association across edits | `ProofSubjectDigest`, `stale`/`unbound`/`depsNotCurrent` |
| **Realization** (axioms sound in each ITP) | semantics bridge for ITP backends | the `eval`-agreement / conformance-vector problem |

## Where Concrete must diverge (the differentiator)

- **Certificate-check, don't trust drivers.** Prefer LRAT/Alethe replayed in a kernel
  (`solver_trusted` → `solver_checked`) over trusting a printer/driver.
- **Keep the transform set small and, where feasible, proven sound.**
- **Weave the evidence ledger through dispatch** — every attestation records host, method,
  digest checked against, and trust dependencies; classes stay distinct.

## Bottom line

Concrete makes sense **iff** the use case needs *both* (1) systems-level control with
predictable, GC-free, own-verified codegen and (2) a graded, audit-visible evidence model.
Need only verified functional code + provers + extraction → use Why3. Need systems control
without proofs → use Rust/Zig. Concrete's bet is that the intersection — auditable
low-level code with an honest ledger of proved-vs-enforced-vs-assumed — is unoccupied, and
that Why3 cannot reach it because it is an ML with a trust-the-drivers backend.
