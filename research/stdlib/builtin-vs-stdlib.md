# Builtin vs Stdlib Boundary

**Status:** Open, partially adopted (math intrinsics migrated to stdlib via `trusted extern fn`; `abs` migrated to trait-based dispatch)

Concrete needs a clear distinction between:

- the **builtin layer**, which is part of the compiler/runtime contract
- the **stdlib layer**, which is ordinary library code users program against

This distinction already exists in practice, but it needs to stay explicit or the language will slowly blur compiler internals and public APIs together.

## Builtins

A builtin is something the compiler knows about specially.

Examples:

- operations intercepted in elaboration, checking, lowering, or codegen
- low-level runtime hooks
- compiler-known primitives for allocation, collections, networking, process, etc.
- things that may not be expressible as ordinary Concrete code alone

Properties of builtins:

- may have special type checking
- may have special lowering or code generation
- may have names shaped by implementation needs rather than user-facing clarity
- form the minimal substrate the language/runtime needs

Builtins are not "just another library module."
They are part of the compiler/runtime boundary.

## Stdlib

The stdlib is the ordinary library surface users should program against.

Examples:

- `std.vec`
- `std.fs`
- `std.net`
- `std.parse`
- `std.fmt`

Properties of the stdlib:

- written in Concrete
- should follow ordinary language rules
- should expose clearer, more coherent APIs
- should wrap builtin/runtime ugliness where possible

The stdlib is where low-level substrate gets shaped into a stable, typed, user-facing surface.

## Why The Distinction Matters

The two layers have different jobs.

### Builtin layer

- minimal
- low-level
- possibly ugly
- compiler/runtime-facing

### Stdlib layer

- coherent
- typed
- ownership-honest
- user-facing

So a name like `print_string` might make sense as a low-level builtin hook, but `print` is what the stdlib should aim to expose.

That is why stdlib API cleanup matters:

- the builtin layer can stay implementation-shaped
- the stdlib should not feel like raw compiler internals leaking through

## Stringly semantic dispatch is technical debt

The compiler should not attach semantic meaning to ordinary user-visible names by raw string matching.

Good:

- an internal intrinsic identity such as `IntrinsicId`
- ordinary name resolution first
- intrinsic-backed lowering only after resolution
- string names used only at true foreign-symbol boundaries

Bad:

- special-case semantic behavior keyed directly off names like `abs`, `print_string`, or similar user-visible functions
- compiler tables whose meaning depends on ordinary stdlib names remaining frozen forever

The remaining builtin cleanup is therefore not "add more intrinsics." It is shrinking the last string-based semantic dispatch so the language surface stays honest and ordinary names stay ordinary.

## Design Rules

### 1. Keep the builtin layer small

Anything that can be moved out of the compiler and into ordinary stdlib code should eventually move there unless keeping it builtin buys something essential.

Good reasons to keep something builtin:

- code generation cannot be expressed cleanly otherwise
- layout/runtime interaction is too low-level for normal code
- bootstrapping constraints require it

Bad reason:

- "it is convenient right now"

### 2. Let the stdlib absorb ugliness

The stdlib should wrap:

- builtin naming quirks
- low-level handle management
- runtime-facing helper shapes

without hiding:

- ownership
- allocation
- effects
- failure behavior

So the stdlib should become cleaner, not more magical.

### 3. Builtin names do not define the user-facing API

Builtin names may be implementation-oriented.
That is acceptable.

The stdlib should define the real surface vocabulary users see:

- `print`
- `println`
- `read`
- `write`
- `append`
- typed handle methods

The public API should not be held hostage by low-level builtin naming.

### 4. Capability and trust rules must apply coherently across both layers

The distinction between builtin and stdlib is not a permission exemption.

The intended model is:

- builtin operations expose real semantic effects honestly
- stdlib code exposes real semantic effects honestly
- implementation-level unsafety is contained by `trusted`
- foreign boundaries remain under `with(Unsafe)`

If builtin and stdlib follow different rules, the language guarantee is weakened at its own foundation.

### 5. Reports should help make the boundary visible

Concrete's audit/report story should make it easier to see:

- what is builtin-driven
- what is stdlib-driven
- what trust/effect boundaries are in play

This does not mean exposing every internal detail in the main user surface.
It means keeping the system inspectable.

## Concrete Example

Bad long-term public surface:

- `print_string`
- `print_int`
- raw socket helper names

Better shape:

- low-level builtin hooks may still exist internally
- stdlib exposes `print`, `println`, typed handle methods, and explicit formatting/buffer APIs

The point is not to hide low-level reality.
The point is to shape it into a coherent public vocabulary.

## Long-Term Direction

Concrete should aim for:

- a **small builtin substrate**
- a **strong stdlib surface**
- a **clear audit boundary** between the two

This fits the wider language goals:

- explicitness
- auditability
- proof-friendliness
- a minimal trusted computing base

If the boundary is kept clean, the language stays easier to understand and easier to verify.
