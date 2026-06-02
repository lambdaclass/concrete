# Anti-Features: What the First Release Will NOT Include

Status: design reference (Phase 3, item 57)

This document is the stable-subset refusal list. It names features that the first release will not include, explains why each is excluded on principled grounds, and distinguishes permanent exclusions from deferred possibilities.

This is not a list of things we have not gotten around to building. Every entry is a deliberate design decision motivated by Concrete's core commitments: explicit authority, predictable execution, auditable trust boundaries, and proof-compatible code.

For the language identity, see [IDENTITY.md](IDENTITY.md).
For permanent and deferred decisions, see [DECISIONS.md](DECISIONS.md).
For the language shape commitments, see [LANGUAGE_SHAPE.md](LANGUAGE_SHAPE.md).
For design admission criteria, see [DESIGN_POLICY.md](DESIGN_POLICY.md).

---

## How to Read This Document

Each entry has:

- **What:** The feature being excluded.
- **Why:** The principled reason, tied to a Concrete design commitment.
- **What Concrete does instead:** The alternative mechanism.
- **Status:** Permanent (load-bearing constraint) or Deferred (may be reconsidered with evidence).
- **Comparison:** How Rust, Zig, and Go handle the same question.

---

## Hidden Runtime Models

### No garbage collection

**Why:** GC hides memory behavior. When memory is reclaimed, how much is retained, and whether a function allocates are invisible to callers. This violates "no hidden runtime model" (Principle 2) and makes predictable execution analysis impossible.

**What Concrete does instead:** Linear ownership by default. Every struct and enum must be consumed exactly once. `defer` schedules deterministic cleanup at scope exit. `with(Alloc)` makes allocation visible in function signatures.

**Status:** Permanent. The ownership model, proof story, and predictable profile all depend on deterministic resource lifetime.

**Comparison:**
- Rust: No GC. Ownership + RAII. Same reasoning.
- Zig: No GC. Manual memory management with allocator parameter.
- Go: GC is fundamental. Tracing collector with concurrent mark-sweep. Go chose GC because it prioritizes simplicity and safety over predictability.

### No hidden async runtime / event loop

**Why:** Async runtimes (Tokio-style executors, goroutine schedulers, libuv-style event loops) create hidden concurrency, hidden scheduling, and function coloring. A function that looks like a normal call may suspend, resume on a different thread, or block a shared executor. This violates "no hidden runtime model" and "all code paths known at compile time."

**What Concrete does instead:** Single-threaded execution today. Future concurrency is research-only and must remain explicit: OS threads first, structured scopes, capability-gated concurrency authority, no implicit executor, and no function coloring. The current research direction separates optional overlap (`Async`) from required concurrent progress (`Concurrent`) without implementing either yet. See [EXECUTION_MODEL.md](EXECUTION_MODEL.md) and [../research/stdlib-runtime/async-concurrency-evidence.md](../research/stdlib-runtime/async-concurrency-evidence.md).

**Status:** Permanent for hidden runtimes. Deferred for explicit structured concurrency.

**Comparison:**
- Rust: Has async/await but requires an explicit runtime (tokio, async-std). The ecosystem fragmentation and function coloring problem are widely acknowledged costs.
- Zig: No async runtime. Uses `async`/`await` as coroutine sugar with no hidden scheduler. Zig 0.12+ removed `@Frame` and simplified the model.
- Go: Goroutines with a hidden runtime scheduler. Go chose this because it prioritizes developer ergonomics over execution visibility.

---

## Implicit Conversions

### No implicit numeric conversions

**Why:** Implicit widening (i32 to i64), narrowing (i64 to i32), or sign changes (u32 to i32) hide data loss and make arithmetic behavior ambiguous. In a language that aims for proof-compatible code, every numeric conversion must be visible at the call site so the proof model can reason about it.

**What Concrete does instead:** All numeric conversions use explicit `as` casts: `x as i64`, `n as u32`. The cast is visible in the source. Future work may add checked narrowing helpers (`try_narrow`) that return `Option`.

**Status:** Permanent. Implicit numeric conversions would break the proof model's ability to reason about integer bounds.

**Comparison:**
- Rust: No implicit numeric conversions. Explicit `as` or `From`/`Into` traits.
- Zig: No implicit numeric conversions. Explicit `@intCast`, `@truncate`.
- Go: No implicit numeric conversions. Explicit `int64(x)` syntax.
- C/C++: Pervasive implicit conversions. A major source of subtle bugs.

### No implicit string conversions

**Why:** Implicit `ToString` or `Display`-style conversions hide allocation and make it unclear whether a function call produces a new heap-allocated string or borrows an existing one. This violates allocation visibility.

**What Concrete does instead:** Explicit `format_int`, `append_int`, `bool_to_string` — each is a named function call with visible allocation. No implicit stringification in print, concatenation, or interpolation contexts.

**Status:** Permanent. Implicit string conversion hides allocation.

**Comparison:**
- Rust: `Display` trait enables implicit formatting in `format!`/`println!`, but allocation is hidden behind the macro.
- Zig: `std.fmt.format` is explicit. No implicit stringification.
- Go: `fmt.Sprint` and `Stringer` interface provide implicit formatting. Allocation is hidden.

### No implicit bool conversions

**Why:** Truthy/falsy conversions (0 is false, empty string is false, null is false) make conditional logic ambiguous. In a language targeting high-integrity code, every condition should be a `Bool` expression that the checker can reason about.

**What Concrete does instead:** `if` and `while` require `Bool` expressions. Comparisons produce `Bool`. There is no truthiness.

**Status:** Permanent. Truthiness is incompatible with type-safe conditionals and proof reasoning.

**Comparison:**
- Rust: No truthy/falsy. `if` requires `bool`.
- Zig: No truthy/falsy. `if` requires `bool`.
- Go: No truthy/falsy. `if` requires `bool`.
- C/C++, Python, JavaScript: Truthy/falsy conversions are fundamental.

---

## Broad Type Inference

### No Hindley-Milner or global type inference

**Why:** Global inference makes error messages worse (the error appears far from the cause), makes code harder to read without IDE support (types are invisible at declaration sites), and couples early compilation phases to late semantic information. It violates phase separation and "every type is visible where it matters."

**What Concrete does instead:** Local let-binding inference only. Function parameters and return types must be explicitly annotated. Generic type arguments must be supplied at instantiation. This means more typing but clearer code.

**Status:** Permanent. Diagnostics, phase separation, and auditability depend on explicit type annotations.

**Comparison:**
- Rust: Local inference within function bodies. Function signatures must be annotated. Trait-based inference can still produce confusing errors.
- Zig: No type inference except for `const`/`var` with initializer. Function signatures fully annotated.
- Go: Local inference via `:=`. Function signatures fully annotated. No generics inference (Go 1.18+ generics require explicit type arguments in some cases).
- Haskell/OCaml: Full HM inference. Powerful but error messages can be confusing and code can be hard to read without annotations.

---

## Hidden Effect Mechanisms

### No exceptions

**Why:** Exceptions create hidden control flow. Every function call becomes a potential exit point. Cleanup code may or may not run depending on whether the exception is caught. This violates "no hidden control flow" and makes predictable execution analysis impossible.

**What Concrete does instead:** `Result<T, E>` for all recoverable errors. `?` for ergonomic propagation (sugar for early return, not an exception throw). Abort-only for unrecoverable failures (OOM). See [FAILURE_STRATEGY.md](FAILURE_STRATEGY.md).

**Status:** Permanent. The abort-only, no-unwinding model is a load-bearing commitment.

**Comparison:**
- Rust: No exceptions. `Result<T, E>` + `?` operator. `panic!` exists but is not recoverable in the normal sense. Same reasoning as Concrete.
- Zig: No exceptions. Error unions + `try`/`catch` syntax. Error values, not exception objects.
- Go: No exceptions. Multiple return values + `error` interface. `panic`/`recover` exists but is discouraged for normal error handling.
- C++/Java/Python: Exceptions are fundamental. Hidden control flow is the norm.

### No effect system beyond capabilities

**Why:** Full algebraic effect systems (as in Koka, Eff, or research languages) are powerful but add significant complexity to the type system, make function signatures harder to read, and require runtime support for effect handlers. Concrete's capability system is deliberately simpler: a fixed set of named capabilities checked at compile time with no runtime cost.

**What Concrete does instead:** Nine named capabilities (`File`, `Network`, `Clock`, `Env`, `Random`, `Process`, `Console`, `Alloc`, `Unsafe`) declared in function signatures. Compile-time subset checking. `--report caps` and `--report authority` for audit. Capability aliases for convenience.

**Status:** Permanent for the current fixed-capability model. The set of capability names may grow, but the mechanism (named capabilities in signatures, compile-time subset checking) is structural.

**Comparison:**
- Rust: No effect system. Side effects are invisible in type signatures (except `unsafe`).
- Zig: No effect system. Side effects are invisible.
- Go: No effect system. Side effects are invisible.
- Koka: Full algebraic effect system. Powerful but complex.

---

## Premature Contracts and Ghost Code

### No pre/post conditions in the first release

**Why:** Contract annotations (`requires`, `ensures`, `invariant`) look like guarantees but are only as strong as the verification backing them. Adding contracts before the proof pipeline is mature creates unverified annotations that give false confidence. Concrete's ProofCore currently covers pure functions with no loops, no mutation, and no capabilities. Extending contracts to effectful code requires a more mature extraction and verification pipeline.

**What Concrete does instead:** The proof model is function-level and theorem-shaped: a Lean 4 theorem is attached to a specific function's PExpr representation. `--report proof-status` shows what is actually proved. No annotation claims more than the proof pipeline can verify.

**Status:** Deferred. Contracts may be added when ProofCore extraction covers a broader subset and the proof pipeline can verify the annotations mechanically. The prerequisite is real proof coverage, not annotation syntax.

**Comparison:**
- Rust: No built-in contracts. `#[cfg(test)]` assertions only. Third-party tools (Kani, Prusti) add verification.
- Zig: No contracts.
- Go: No contracts.
- SPARK/Ada: Full contract-based verification with pre/post/invariant. The gold standard, but requires a mature proof engine.
- Eiffel: Pioneered design-by-contract. Contracts are runtime-checked, not statically verified.

### No ghost code or specification-only types

**Why:** Ghost variables and specification types (values that exist only for verification, erased at runtime) require a mature erasure discipline and a clear boundary between specification and implementation. Adding them prematurely creates confusion about what code actually executes.

**What Concrete does instead:** Proofs attach to real Concrete functions via PExpr extraction. The proof target is the actual function body, not a separate specification. There is no ghost state.

**Status:** Deferred. May be reconsidered when the proof pipeline matures.

---

## Dynamic Dispatch

### No trait objects / dynamic dispatch

**Why:** Trait objects (`dyn Trait`) hide the concrete type and dispatch target behind a runtime vtable. The call target is unknown at compile time. This violates "all code paths known at compile time" and makes static analysis, predictable execution, and proof incomplete — the prover cannot reason about a call through an opaque vtable.

**What Concrete does instead:** Enums for closed dispatch (exhaustiveness checked at compile time). Monomorphized generics for compile-time dispatch. Typed function pointers for explicitly indirect dispatch. Struct of function pointers for manual vtable when open extension is needed.

**Status:** Permanent. The static call graph, predictable profile, and proof model depend on knowing all call targets.

**Comparison:**
- Rust: Has `dyn Trait` for dynamic dispatch. Useful for plugin architectures and heterogeneous collections. The cost is that `dyn Trait` calls are opaque to the optimizer and verifier.
- Zig: No trait objects. Uses `anytype` comptime generics and explicit function pointers.
- Go: Interfaces provide implicit dynamic dispatch. All method calls on interfaces are indirect.

---

## Closures

### No closures / anonymous functions with capture

**Why:** Closures bundle code and captured data into a single opaque value. The captured data is invisible at the call site. The closure may capture mutable references, owned values, or environment state — none of which is visible in the type signature. This violates "no hidden data flow" and "no hidden control flow."

**What Concrete does instead:** Named functions. Function pointers (no capture, C-compatible). Monomorphized generics for higher-order patterns. Enums + match for dispatch.

**Status:** Permanent. See [DECISIONS.md](DECISIONS.md) and the full analysis in `research/language/no-closures.md`.

**Comparison:**
- Rust: Has closures with explicit capture modes (`Fn`, `FnMut`, `FnOnce`). The capture is type-checked but invisible at the call site without reading the closure body.
- Zig: No closures. Uses function pointers with explicit context parameter.
- Go: Has closures with implicit capture by reference. Capture semantics are a common source of bugs (loop variable capture).

---

## Macros and Metaprogramming

### No source-generating macros

**Why:** Source-generating macros (Rust's `macro_rules!` / proc macros, C preprocessor, Lisp macros) destroy file-local parsing, couple early compilation phases to late semantic information, and make audit output unreliable. A `#[derive(Debug)]` in Rust generates code that the programmer never wrote and the auditor cannot see without expansion tools. This violates phase separation, locality, and auditability.

**What Concrete does instead:** Monomorphized generics for code reuse. Explicit code generation as a build step (not a language feature). A small surface of compiler-recognized attributes.

**Status:** Permanent. See [DECISIONS.md](DECISIONS.md).

**Comparison:**
- Rust: Powerful macro system (declarative + procedural). Essential to the ecosystem but a source of compile-time complexity and audit difficulty.
- Zig: `comptime` evaluation instead of macros. Code generation happens in the same language with compile-time function execution.
- Go: No macros. `go generate` for external code generation. The language is intentionally simple.

### No comptime evaluation (for now)

**Why:** Compile-time function evaluation (Zig's `comptime`, C++ `constexpr/consteval`) is powerful but adds a second evaluation model to the language. Concrete's phase separation requires that each compilation phase has a clear input and output. Comptime blurs the boundary between parse-time and run-time.

**What Concrete does instead:** Constant folding by LLVM. Compile-time queries (`sizeof`, `alignof`) as intrinsics.

**Status:** Deferred. May be reconsidered for compile-time constants only (not arbitrary evaluation). The prerequisite is a clear specification of what can be evaluated at compile time and how it interacts with the proof model.

**Comparison:**
- Zig: `comptime` is fundamental. Generics, conditional compilation, and many patterns are expressed through compile-time evaluation.
- Rust: `const fn` with expanding capabilities. Limited compile-time evaluation.
- Go: No comptime. Constants are simple literal expressions.

---

## Operator Overloading

### No user-defined operator overloading

**Why:** Operator overloading makes `a + b` potentially mean anything — allocation, I/O, mutation of global state. The meaning of an expression depends on the types, which may not be visible at the use site. This violates "no hidden effects" and makes audit harder.

**What Concrete does instead:** Operators have fixed semantics on primitive types. User-defined types use named methods: `vec.push(x)`, `map.insert(k, v)`, `a.add(b)`. The operation is always visible.

**Status:** Permanent for the first release. May be reconsidered later for a narrow set of mathematical types (e.g., matrix multiplication, big integers) if the proof model can handle it. Any future overloading would need to be effect-transparent — the compiler must still know what `+` does.

**Comparison:**
- Rust: Full operator overloading via traits (`Add`, `Mul`, etc.). Widely used. The cost is that `+` can allocate, panic, or have arbitrary side effects.
- Zig: No operator overloading.
- Go: No operator overloading.

---

## Variadic Functions

### No variadic functions

**Why:** C-style variadic functions (`printf(fmt, ...)`) are type-unsafe — the compiler cannot check that the arguments match the format string. Even type-safe variadics (as in C++ parameter packs or Rust macros) add complexity to the type system and make function signatures harder to read.

**What Concrete does instead:** Fixed parameter lists. For formatting, explicit `format_int`, `append`, `append_int` calls. For collections, explicit builder patterns or array literals.

**Status:** Permanent for C-style variadics. Deferred for type-safe variadic-like patterns (may be addressed through generics or explicit tuple/array passing).

**Comparison:**
- Rust: No C-style variadics. Variadic-like patterns via macros (`println!`, `vec!`).
- Zig: No C-style variadics. Uses `anytype` and comptime for flexible argument handling.
- Go: Has variadic functions (`func(args ...int)`). Type-safe but limited to homogeneous types.

---

## Default Function Arguments

### No default function arguments

**Why:** Default arguments create invisible parameters. A call `f(x)` may actually pass `f(x, 42, "default")`. The caller cannot see the full set of arguments without reading the function declaration. This violates "every call is visible at the call site."

**What Concrete does instead:** All arguments are explicit. For convenience patterns, define multiple functions: `open(path)` and `open_with_mode(path, mode)`.

**Status:** Deferred. May be reconsidered if real-program pressure demonstrates that the verbosity cost is high enough to justify the visibility cost. Would need to be designed so defaults are visible in signatures and reports.

**Comparison:**
- Rust: No default arguments. Uses builder pattern or separate functions.
- Zig: No default arguments (Zig 0.12+). Uses explicit struct-of-options pattern.
- Go: No default arguments. Uses variadic options pattern or separate functions.
- Python/C++/Kotlin: Default arguments are fundamental.

---

## Inheritance

### No class inheritance

**Why:** Inheritance creates implicit behavior chains. A method call may dispatch to a parent class, a grandparent class, or an overridden method — the call target depends on the runtime type and the inheritance chain. This violates "all code paths known at compile time" and makes reasoning about behavior difficult.

**What Concrete does instead:** Structs for data. Traits for shared behavior contracts. Enums for closed dispatch. Composition over inheritance.

**Status:** Permanent. Concrete is not an object-oriented language.

**Comparison:**
- Rust: No inheritance. Traits + composition.
- Zig: No inheritance. Explicit structs + function pointers.
- Go: No inheritance. Interfaces + embedding (composition).

---

## Implicit Trait Resolution / Typeclass Coherence

### No implicit trait resolution beyond monomorphization

**Why:** Languages like Rust and Haskell resolve trait implementations implicitly at call sites. `x.to_string()` finds the right `ToString` impl based on the type of `x`, possibly from a different crate. This creates action-at-a-distance: adding a new trait impl in one module can change behavior in another. Concrete avoids this because it makes the call graph dependent on global knowledge.

**What Concrete does instead:** Traits exist for defining shared behavior contracts. Generic functions are monomorphized at compile time with explicit type parameters. There is no implicit trait resolution — the concrete type and its implementation are always known at the instantiation site.

**Status:** Permanent for implicit resolution. The current trait system may evolve, but resolution will remain explicit.

---

## Summary Table

| Feature | Status | Why excluded |
|---------|--------|-------------|
| Garbage collection | Permanent | Hides memory behavior; breaks predictable profile |
| Hidden async/runtime | Permanent | Hidden concurrency and scheduling |
| Implicit numeric conversions | Permanent | Hides data loss; breaks proof model |
| Implicit string conversions | Permanent | Hides allocation |
| Implicit bool (truthy/falsy) | Permanent | Ambiguous conditionals |
| Hindley-Milner inference | Permanent | Worse diagnostics; breaks phase separation |
| Exceptions / unwinding | Permanent | Hidden control flow; breaks predictable profile |
| Full algebraic effects | Permanent | Complexity; runtime cost |
| Pre/post conditions | Deferred | Proof pipeline not mature enough |
| Ghost code | Deferred | Erasure discipline not designed |
| Trait objects / dyn dispatch | Permanent | Opaque call targets; breaks static analysis |
| Closures | Permanent | Hidden capture; hidden data flow |
| Source-generating macros | Permanent | Breaks phase separation and auditability |
| Comptime evaluation | Deferred | Blurs parse-time / run-time boundary |
| Operator overloading | Permanent (first release) | Hidden effects behind operators |
| C-style variadics | Permanent | Type-unsafe |
| Default arguments | Deferred | Invisible parameters |
| Class inheritance | Permanent | Implicit dispatch chains |
| Implicit trait resolution | Permanent | Action-at-a-distance |

---

## The Pattern

The exclusions cluster around one meta-principle: **if it hides behavior from the reader, the auditor, or the prover, it does not belong in the first release.**

Concrete is not trying to be the most expressive language. It is trying to be the language where you can answer "what does this code actually do?" by reading the source, checking the reports, and trusting the proofs. Every feature on this list would make that harder.

Some of these exclusions have real costs — more verbose code, more boilerplate, less convenience. Those costs are accepted because the alternative (hidden behavior) is worse for Concrete's target audience: people writing code that must be reviewed, audited, bounded, and eventually proved.

---

## What Might Change

Features marked "Deferred" may be reconsidered when:

1. **Real program pressure** demonstrates the cost is unacceptable (not hypothetical convenience, but actual pain in real examples)
2. **A design exists** that preserves Concrete's core commitments (explicit, auditable, predictable, proof-compatible)
3. **The prerequisite infrastructure** is in place (e.g., contracts require a mature proof pipeline; comptime requires a clear phase separation story)

Features marked "Permanent" would require rethinking fundamental language invariants to add. That is not impossible, but the bar is very high — it requires showing that the invariant is no longer load-bearing.
