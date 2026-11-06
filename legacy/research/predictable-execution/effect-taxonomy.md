# Effect Taxonomy

**Status:** Open

This note defines how Concrete should classify function behavior without turning the language into a full algebraic-effect system.

## Why This Note Exists

Concrete already has explicit capabilities, but those mostly describe authority: what a function is allowed to reach.

That is not the whole behavioral story.

For auditability, predictable execution, and profile design, Concrete also needs to represent:

1. operational behavior
2. trust boundaries

The point of this note is to separate those axes clearly and keep the model small.

## The Three Axes

### 1. Authority

Authority answers:

1. what resources may this function reach?
2. what classes of operation is it allowed to perform?

Examples:

1. `Network`
2. `Process`
3. `Console`
4. `Unsafe`
5. `Alloc`
6. later candidates such as `File`, `Env`, `Clock`

Authority should remain visible in signatures.

### 2. Operational Behavior

Operational behavior answers:

1. can this function allocate?
2. can it block?
3. can it recurse?
4. can it run with an unknown loop bound?
5. can it spawn concurrent work?

These are not the same as authority.

Example:

A function may require `Network`, but that does not tell you whether it sends already-available data or blocks waiting on input.

The likely first operational categories are:

1. `may_block`
2. recursion / call-cycle present
3. unknown loop bound present
4. concurrency use present
5. allocation class: `no_alloc`, `bounded_alloc`, `unbounded_alloc`

These should begin as report categories, not signature syntax.

### 3. Trust Boundaries

Trust boundaries answer:

1. does this function rely on `trusted` internals?
2. does it cross an FFI boundary?
3. does it rely on external assumptions the safe core cannot inspect?

The likely first trust-boundary categories are:

1. `crosses_ffi`
2. `uses_trusted`

These should also begin as report categories.

## What Goes Where

Concrete should keep the model intentionally split:

1. authority in signatures
2. operational behavior in reports
3. trust-boundary facts in reports
4. selective enforcement through profiles

That avoids overloading signatures with too much noise while still making the important facts visible.

## What Profiles Can Enforce

Profiles can use report categories as gates.

Examples:

1. predictable-execution profile rejects:
   - recursion
   - unknown loop bounds
   - blocking operations
   - unrestricted FFI
   - disallowed concurrency
   - unrestricted allocation
2. proof-oriented or high-integrity profiles may restrict `uses_trusted`
3. no-alloc profiles reject any function not classified as `no_alloc`

This gives Concrete enforcement without requiring every operational property to become part of ordinary function syntax.

## Why This Is Not A Full Algebraic-Effect System

Concrete should not try to become a general-purpose effect calculus.

This design is intentionally narrower:

1. no effect rows in ordinary typing
2. no effect handlers
3. no effect polymorphism as a major abstraction mechanism
4. no attempt to model every computation as a composable effect expression

Instead:

1. authority remains explicit in function signatures
2. operational and trust facts are compiler-known and reportable
3. selected profiles enforce subsets of those facts

That is enough to get most of the audit and profile value without the language cost of a full effect system.

## Recommended Next Step

The first implementation target should be report-level classification for:

1. `may_block`
2. `crosses_ffi`
3. `uses_trusted`
4. recursion / call-cycle status
5. unknown loop-bound status
6. concurrency usage
7. allocation classification

Only after those reports prove useful should Concrete consider promoting any new category into syntax or stricter profile enforcement.
