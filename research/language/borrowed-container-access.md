# Borrowed Container Access Without Lifetimes

Status: research note, design input for `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`
and ROADMAP Phase 6 #8a / Phase 5 #24.

## Question

How should Concrete let code read or mutate values stored inside containers
without returning references that can outlive the container operation?

The immediate pressure is H1: public stdlib APIs returned references inside
aggregates (`Option<&T>`, `Option<&mut T>`). A saved reference could survive a
container mutation, rehash, removal, or reallocation. The mutable half is already
withdrawn; the immutable half is frozen and disclosed until a replacement lands.

The design goal is not just "make this safe." It is to make it safe without
turning Concrete into a lifetime/region language.

## Design Constraints

- No hidden captures.
- No lifetimes.
- No region parameters in ordinary user types.
- No references inside `Option`, `Result`, structs, arrays, containers, callback
  contexts, or generic wrappers.
- Capabilities stay visible in function types and reports.
- Collection APIs should expose operations, not storage layout.
- Zero-copy stored parser results use owned views (`ByteView`), not stored
  borrowed references.
- Trusted/raw-pointer smuggling stays audit responsibility, not a safe-language
  guarantee.

## Prior Art

### Rust: returned refs plus lifetimes

Rust permits APIs such as `HashMap::get -> Option<&V>` and
`HashMap::get_mut -> Option<&mut V>` because the returned reference carries a
lifetime tied to the map borrow. While the borrow is live, the map cannot be
used in ways that would invalidate the reference.

This is expressive and ergonomic, but lifetime reasoning becomes part of the
language. Concrete deliberately does not want that burden in the first model.

### Austral: borrow blocks plus region-indexed references

Austral rejects Rust-style lifetime complexity, but references are still indexed
by regions (`Reference[T, R]`, `WriteReference[T, R]`). Borrow blocks introduce
regions, and functions can quantify over them.

The lesson is important: even "no lifetimes" languages that support ergonomic
returned borrowed references tend to need some provenance/region concept. That
is exactly what Concrete is trying not to depend on for H1.

### Swift: scoped `with...` APIs

Swift exposes temporary access through callback-shaped APIs such as
`withUnsafeMutableBufferPointer`. The borrowed pointer/view is valid only inside
the callback activation.

This family is closer to Concrete's intended V1.1 mechanism: scoped access by
construction rather than returned references.

The similarity is the shape: "the API gives you a temporary view, and that view
is only valid inside the callback." The differences are load-bearing for
Concrete:

- Swift closures can capture hidden state; Concrete callbacks use an explicit
  function pointer plus explicit context.
- Swift's `unsafe` variants rely partly on caller discipline; Concrete's safe
  surface rejects returned references and keeps raw-pointer access behind
  `Unsafe` / `trusted` audit boundaries.
- Swift does not make callback capabilities part of the function type; Concrete
  does, so authority remains visible in signatures and reports.

So Concrete borrows the scoped-access pattern, not Swift's closure or unsafe
pointer discipline.

### Haskell: `with` / `bracket`

Haskell's `withFile` / `bracket` style scopes a resource to a callback. It is
not a borrow checker, but the lifetime shape is the same: the resource is made
available only within a delimited continuation.

The useful lesson is the resource-shape invariant: acquire, pass downward to a
user callback, then release/forget after the callback returns. Concrete applies
that shape to borrowed container elements rather than file handles. The
differences are equally important:

- Haskell closures capture freely; Concrete callback state is explicit.
- Haskell relies on GC/runtime/monadic structure; Concrete relies on ownership,
  linearity, and capability-checked function types.
- Haskell's pattern scopes resources; Concrete's pattern scopes borrows and must
  also prevent references from escaping through return types.

This makes `with_value` a systems/proof-oriented cousin of `withFile`, not the
same runtime model.

### C++ and Zig: explicit discipline

C++ exposes references and iterators freely, with container-specific invalidation
rules. Zig uses explicit pointers and slices and relies on programmer discipline.
Both are useful systems-language references, but neither provides the static
safe-language guarantee Concrete wants for ordinary code.

### Go: operation/value APIs

Go maps do not expose stable references into map storage. Code reads values,
writes values, or stores pointers as values explicitly. This is less expressive
than Rust, but it avoids a large class of aliasing and invalidation problems.

Concrete's tier-1 collection direction is closer to this: operation APIs first,
borrowed access only when it is explicitly scoped.

## Candidate Designs

### 1. Returned-reference provenance (`from(self)`)

Example:

```con
#[requires(i < v.len())]
fn get_ref<T>(v: &Vec<T>, i: u64) -> &T from(v)
```

This is the smallest returned-reference provenance feature: the result borrows
one named parameter. No lifetime variables, no inference, no generic outlives
constraints.

It is still provenance. It teaches the checker that a function result borrows
one of its inputs. That is the mechanism H1 was specifically redesigned not to
need. Keep it as a deferred escape valve, not the V1.1 fix.

If it ever lands, it must remain scalar-only forever. It must not permit refs to
enter `Option`, `Result`, structs, containers, callback contexts, or aliases.

### 2. Lexical borrow block over a container projection

Example:

```con
borrow v = m.get(k) in 'R {
    use(v)
}
```

This is ergonomic and aligns with Concrete's existing borrow-block syntax. It is
also Austral-shaped: the borrow is lexical, and the owner is frozen in the
region.

The problem is that the checker must know `m.get(k)` returns a reference
borrowed from `m`. That is the same minimal `from(self)` provenance bit above.
So this is a future ergonomic syntax over a proven model, not the first
mechanism for H1.

### 3. Scoped callbacks (`with_value`)

Example shape:

```con
map.with_value(k, ctx, f)
map.with_value_mut(k, ctx, f)
```

Here the collection creates the element borrow internally and passes it down to
the callback. The borrow never appears in a return type, so no returned-reference
provenance exists to track. The callback activation is the region.

This fits Concrete's current language:

- no hidden captures;
- callback state is explicit;
- capabilities stay on the callback function type;
- the borrowed value cannot be returned through an aggregate;
- mutation/reallocation of the container is blocked by the live receiver borrow
  and the "container not reachable from context" invariant.

This is the chosen V1.1 mechanism.

### 4. Operation/value APIs

Tier 1 avoids borrowed access entirely:

```con
contains(k)
remove(k) -> Option<V>
replace(k, v) -> Option<V>
update(k, fn(V) -> V) -> bool
```

These APIs move values in and out or perform container-owned operations. They do
not expose storage. This is the best default surface and closed the dangerous
mutable half of H1 without new language machinery.

### 5. Clone-returning access

`get_cloned(k) -> Option<V>` is useful long term, but it requires a deliberate
Clone design: explicit operation, capability-visible, audit-visible, and
linear-safe. It must not be rushed in as an H1 patch.

Clone belongs in the value model, not in the returned-reference fix.

## Chosen Direction

Concrete's direction is:

1. **Tier 1 now:** operation/value APIs. Withdraw mutable aggregate-ref APIs and
   avoid exposing storage.
2. **V1.1:** scoped callbacks (`with_value`, `with_value_mut`, `modify`) for
   non-Copy borrowed reads/mutations.
3. **Stored zero-copy:** owned `ByteView` offsets, not stored references.
4. **Deferred:** lexical borrow-block container projections, only if scoped
   callbacks prove awkward in real workloads.
5. **Deferred:** scalar `from(self)`, only if real workloads prove the
   callback/view model insufficient.
6. **Never:** refs nested inside aggregates or stored data structures.

## Soundness Invariant for Scoped Callbacks

The callback context must not contain or reach the container being accessed.

Because Concrete has no closures, callback state is explicit. In safe code, if
the container is not in the context, the callback cannot mutate or reallocate it
while the element borrow is live. Trusted/raw-pointer aliases are outside the
safe guarantee and remain audit responsibility.

This invariant must be enforced by `scripts/tests/check_callable_values.sh` with
positive and negative fixtures.

## Workload Calibration

The known real workloads did not require returned borrowed references to escape:

- `lox` does not depend on map borrowed access.
- `kvstore` already fits tier-1 operation APIs.
- `integrity` has one immutable read of a `String` used inside a scoped compare;
  it migrates naturally to `with_value`.

That evidence is why `from(self)` stays deferred.

## Non-Goals

- No Rust-style lifetime variables.
- No Austral-style region parameters in ordinary references.
- No hidden closure captures.
- No public safe `Option<&T>` / `Option<&mut T>` replacement.
- No `Clone` as an H1 shortcut.

## Decision Summary

The best design for Concrete is not the most expressive borrowed-reference
design. It is the smallest design that preserves the language's thesis:

- operations over storage access;
- scoped callbacks over returned refs;
- owned views over stored refs;
- deferred provenance only with evidence.
