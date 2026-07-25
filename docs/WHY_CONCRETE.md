# Why Concrete?

Status: explanatory overview for C and Rust developers

Concrete is not trying to make formal verification feel magical.

It is trying to make evidence hard to lose.

If you come from C, Rust, or Zig, the code shape should look familiar:
fixed-size values, explicit control flow, integer operations, arrays, no garbage
collector, and no runtime trying to hide what the program is doing. The unusual
part is not the surface syntax. The unusual part is that the toolchain keeps a
ledger of what a function is allowed to do, what has been proved, what was only
tested, and what is still trusted.

That ledger is the point of the language.

## Systems Code First

Concrete is a small systems language. It is not Lean with nicer syntax, and it
is not a proof assistant that happens to emit native code.

The baseline assumptions are deliberately ordinary:

- no garbage collector;
- fixed arrays and bounded loops where possible;
- wrapping integer operations when requested;
- explicit heap allocation;
- scoped, second-class safe references instead of returned/stored
  lifetime-bearing reference APIs;
- explicit FFI and unsafe boundaries;
- explicit capabilities for authority such as `Console`, `File`, and `Alloc`.

The goal is not to replace C or Rust everywhere. It is to cover a narrower
case: small, auditable systems code where the cost of being wrong is high enough
that tests alone are not the evidence you want.

## Authority Is Visible

In C, a function can call anything visible through a header. In Rust, effects are
usually a convention: a function's type tells you a lot about ownership, but not
whether it can read files, print, allocate, or cross a process boundary.

Concrete makes authority part of the function boundary:

```con
fn parse_byte(data: Int, offset: Int) -> Int {
    return data + offset;
}

fn report(result: Int) with(Console) {
    if result == 0 { println("ok"); } else { println("fail"); }
}
```

`parse_byte` has no ambient authority. It cannot write to the console, open a
file, allocate through `Alloc`, or call a hidden network API unless its signature
says so through capabilities.

That is less flexible than C. It is also easier to review.

## Visible Is Step One; Knowable Is the Point

Visibility alone is reporting. The deeper consequence of Concrete's refusals is
what they make computable: with no closures, no trait objects, no macros, and
whole-program monomorphization, callable values come from a closed set of named
functions, and indirect calls carry value identity rather than a name to
re-resolve. A function-pointer target may still be chosen at runtime, but its
possible targets are statically enumerable.

That gives per-function facts—authority, allocation, failure modes,
obligations—a tractable path to conservative whole-program facts from `main`
outward. A sound analysis must retain every possible target of an indirect
call, and today's reports do not yet turn every such edge into a complete
composition theorem. The advantage is narrower and still valuable: the
possible callable set is finite and source-visible rather than extended by
captured code, runtime type dispatch, or generated source.

Today `--report authority` and `--report caps` expose part of this structure,
while capability-bearing function-pointer types enforce authority at indirect
call sites. The direction of the proof work is to make target-set completeness
and transitive composition checked whole-program claims. That step is roadmap
work, not a shipped feature.

## Evidence Is Visible

Most codebases have evidence, but it is scattered:

- unit tests in one directory;
- fuzzers somewhere else;
- informal invariants in comments;
- proof scripts, if any, outside the language;
- assumptions in issue trackers or people's heads.

Concrete's audit surface keeps those categories separate. A claim can be:

- `proved_by_lean` - checked by the Lean kernel;
- `proved_by_kernel_decision` - closed by a kernel-checked decision procedure
  such as `omega` or `bv_decide`;
- `proved_at_callsite` - checked from a caller's concrete arguments;
- `runtime_checked` - guarded dynamically in a gradual mode;
- `tested_by_oracle` - tested against a reference implementation;
- `assumed` or `trusted` - accepted as a named boundary;
- `missing` or `stale` - not currently valid evidence.

This is intentionally not one green badge. A C developer knows the difference
between "covered by a test" and "guaranteed by the type checker." Concrete keeps
that distinction in the report.

## Proofs Do Not Silently Drift

The common failure mode for external verification is synchronization. The code
changes; the spec or proof still exists; the report still looks comforting.

Concrete's strongest proof path ties the theorem to a stored digest of the
extracted source body. The compiler extracts a ProofCore expression, records a
body fingerprint, and checks that the registered proof is still attached to
the same body shape. A source link with no stored fingerprint is `unbound`, not
proved; a changed stored body becomes stale.

That is useful containment, not yet a complete semantic-subject digest. The
current hash does not cover every signature/type fact, source contract, or
transitive callee. Bugs 059/060 and R-0004 make those limits explicit rather
than letting “fresh” overclaim.

That is why the HMAC-SHA256 flagship matters. Its proof is not "there is a
handwritten model that looks like the implementation." The shipped claim is:

1. the Concrete source body extracts to this ProofCore body and has a stored
   fingerprint;
2. this ProofCore body refines an independent SHA-256/HMAC spec;
3. the named theorems replay through the Lean kernel;
4. if the represented body drifts, the claim turns stale.

That is the core thesis in one example.

## Concrete Can Prove Real Code Now

The current real proof artifact is HMAC-SHA256. The chain covers:

- block-to-word packing;
- SHA-256 schedule expansion;
- compression rounds;
- state serialization;
- padded multi-block hashing;
- outer HMAC composition.

The proof is kernel-checked and spec-drift-tied. It is also bounded and honest:
the README and audit files state the input bounds, the trusted backend boundary,
and the remaining assumptions.

The useful result was not only the HMAC theorem. The proof forced reusable
infrastructure out of the project:

- loop induction over extracted `while` programs;
- array get/set frame lemmas;
- bitvector automation;
- function-table and call-wrapper scaffolding;
- refinement theorem patterns.

That reusable layer is now `Concrete/ProofKit`.

## Trust Is Named

Concrete is not pretending the whole stack is proved.

Today:

- the Lean kernel checks proof terms;
- the Concrete compiler is written in Lean, but not fully verified;
- the source-body-to-ProofCore tie is checked by a stored fingerprint and the
  current spec-drift mechanism, with the full-subject gaps tracked in R-0004;
- LLVM, clang, the linker, libc, and the host kernel remain trusted;
- machine-level timing claims remain assumptions unless a lower layer checks
  them;
- unprofiled floating point is excluded from proof eligibility.

This is not a weakness to hide. It is the project rule: every claim must be
proved, enforced, tested, reported, assumed, or trusted. No vague middle.

## Contracts Are The Authoring Surface

Handwritten Lean proofs are too expensive to be the normal user interface.

The direction is source-level evidence:

```con pseudocode
spec fn ch_spec(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ ((~x) & z)
}

#[ensures(result == ch_spec(x, y, z))]
fn ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ ((~x) & z);
}
```

The source states the claim. The compiler reports the obligation. The evidence
backend says how it was discharged: Lean, a kernel decision procedure, a
call-site check, a runtime check, or an explicit assumption.

Loop contracts follow the same principle:

```con pseudocode
#[invariant(0 <= i && i <= 8)]
#[variant(8 - i)]
while i < 8 {
    set i = i + 1;
}
```

This is closer to SPARK or Dafny ergonomics than to "write a proof assistant
program in your source file." There are no tactics in `.con` files.

## What C Developers Get

Compared with C, Concrete gives up some freedom:

- no ambient authority;
- no untracked pointer tricks in ordinary code;
- no implicit unchecked resource ownership;
- no silent proof/evidence claims.

In exchange, it gives you compiler-visible boundaries:

- which functions allocate;
- which functions can perform I/O;
- which code is trusted;
- which functions are pure;
- which loops are bounded or have obligations;
- which claims are proved, tested, assumed, or stale.

The intended feeling is not "C with a theorem prover glued on." It is "small C
style systems code where the audit trail is part of the toolchain."

## What Rust Developers Get

Rust already gives strong memory safety and an excellent ownership model.
Concrete is not trying to beat Rust at ecosystem maturity, async libraries, or
general-purpose ergonomics.

The difference is the evidence surface.

Rust's type system can tell you a lot about borrowing, lifetimes, and data
races. Concrete is trying to make different facts visible:

- this function needs exactly these capabilities;
- this proof is tied to this source body;
- this loop invariant was checked by this engine;
- this crypto implementation refines this independent spec;
- this assumption is still in the trusted base.

Concrete's linear ownership is simpler and less expressive than Rust's borrow
checker. That is deliberate. The project is trading some generality for a
smaller proof and audit story.

## What Concrete Is Not Trying To Be

Concrete should resist features that make ordinary systems code harder to audit:

- no row effects for now;
- no dependent programs in source;
- no hidden proof DSL;
- no general macro/comptime system yet;
- no graded modal type system in v1;
- no implicit async execution;
- no proof-directed optimization before the proof story is ergonomic.

Those ideas may be useful in other languages. Concrete's filter is stricter:
does the feature make authority, resource use, trust, and evidence more visible
to a reviewer?

If not, it waits.

## The Short Version

Concrete is for the narrow case where all of these matter at once:

- systems-shaped code;
- no garbage collector;
- explicit authority;
- predictable resource boundaries;
- a closed, statically enumerable callable set, so conservative program-wide
  analysis has a finite target space;
- kernel-checked proofs for selected functions;
- proof drift detected by the toolchain;
- honest reporting of what is still trusted.

If you want the broadest production systems language, use Rust.

If you want direct low-level control with a mature toolchain, use C or Zig.

If you want small systems code where the evidence ledger is part of the
language, Concrete is the experiment.
