+++
title = "Why Concrete"
+++

Concrete is not trying to win by having the most features. Its intended strength is that important low-level properties stay explicit enough to inspect, report, audit, and eventually prove.

> Concrete should be strongest where many systems languages are not explicitly centered: auditability, explicit authority/trust boundaries, and proof-friendly compiler structure.

Concrete is also aiming at something broader than "a working compiler": a compiler that can explain itself, surface audit-relevant facts directly, and eventually produce inspectable and reproducible outputs that users can trust.

One important long-term direction is an explicit high-integrity profile: stricter execution and safety modes for critical code, built around bounded behavior, analyzability, and better evidence rather than feature sprawl.

## One Concrete Shape Of The Vision

Concrete is trying to make three things line up:

1. low-level implementation code
2. audit/report outputs
3. Lean 4 proof direction

Example implementation:

```con
fn unwrap_or_zero(x: Option<Int>) -> Int {
    match x {
        Option::Some { value } => return value,
        Option::None => return 0,
    }
}
```

Compiling that file and running `concrete main.con --report caps` gives:

```text
=== Capability Summary ===

module main:
      unwrap_or_zero : (pure)

Totals: 1 functions (1 pure), 0 externs
```

and `--report unsafe` answers `No unsafe signatures found.`

The proof direction, in sketch form, is a Lean 4 meaning for the same function:

```lean
def unwrap_or_zero_core : ProofCoreFn := ...

theorem unwrap_or_zero_correct (x : Option Int) :
  unwrap_or_zero_sem x =
    match x with
    | some v => v
    | none => 0 := by
  cases x <;> rfl
```

That is the point of the project identity:

- Concrete stays the executable low-level language
- the compiler explains authority, trust, and allocation facts directly
- Lean 4 can eventually be used to prove selected Concrete code

## Why Concrete Exists

Concrete was created to close a gap between low-level programming and mechanized reasoning.

It is trying to make systems code explicit enough that you can answer concrete questions about it: what authority it uses, where it allocates, where it cleans up, where trust boundaries are crossed, and what the compiler actually means by the program.

Lean 4 is part of that story, but Lean is not itself a low-level systems language. It is a theorem prover with a runtime and garbage collector. Concrete exists so low-level code can stay explicit and non-GC-oriented while Lean 4 is leveraged to prove properties about that code.

The point is not only speed or control. The point is to keep low-level power while making authority, resources, `Unsafe`, `trusted`, and compiler meaning visible enough to inspect and eventually prove.

## What Makes It Different

### Auditability

Concrete is trying to show where authority enters, where allocation and cleanup happen, what layout/ABI a type really has, and what monomorphized code actually exists.

### Explicit Trust

Capabilities, `Unsafe`, `trusted fn`, `trusted impl`, and `trusted extern fn` are explicit surfaces, not hidden implementation accidents.

### Small Semantic Surface

Ordinary names should stay ordinary, compiler magic should stay narrow, and the trusted computing base should remain easier to reason about.

### Proof-Friendly Structure

The compiler is being shaped around clear Core semantics, SSA as a real backend boundary, explicit pass structure, and formalization targets that match the architecture.

## Two Lean Goals

Concrete's proof direction has two layers:

- prove properties of the language/compiler in Lean
- eventually prove properties of selected Concrete programs in Lean through formalized Core semantics

Those are different goals. Compiler proofs give trust in the language rules and pipeline. Program proofs give trust in specific user code.

That second goal matters because it is a much stronger claim than "the compiler seems well-designed": it points toward real user functions whose formal Core meaning can be proved against a specification in Lean.

## What Concrete Is Not Trying To Be

Concrete is not primarily trying to out-compete:

- Rust on macro power or ecosystem scale
- Zig on comptime or cross-compilation ergonomics
- Odin on minimal syntax alone
- other systems languages on feature count for its own sake

The goal is a language that is unusually explicit, inspectable, and honest.

Compared to Lean, Concrete is the low-level language and Lean 4 is the proof environment. Compared to mainstream systems languages, it is more explicitly centered on auditability and trust boundaries, with a style that should stay closer to Austral's clarity than to feature sprawl. Compared to verification-first languages, it is trying to keep FFI, layout, ownership, and low-level runtime concerns first-class.

## Start Here

- [Getting started](@/guide/getting_started.md)
- [Project identity](@/reference/IDENTITY.md)
- [High-integrity Concrete](@/guide/high_integrity.md)
- [Use cases](@/guide/use_cases.md)
- [Status](@/guide/status.md)
- [Standard library](@/guide/stdlib.md)
- [Testing](@/guide/testing.md)
- [Architecture](@/guide/architecture.md)
- [Reference](@/reference/_index.md)
- [Internal details](@/guide/internal/_index.md)
- [Roadmap](https://github.com/unbalancedparentheses/concrete2/blob/main/ROADMAP.md)
- [Changelog](https://github.com/unbalancedparentheses/concrete2/blob/main/CHANGELOG.md)
