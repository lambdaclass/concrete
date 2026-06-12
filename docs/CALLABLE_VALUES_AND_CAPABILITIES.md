# Callable Values and Capability-Polymorphic Callbacks

Status: design checkpoint (ROADMAP Phase 5 #24 / #24a) — the single model that
must exist before the Phase 6 stdlib iteration/HOF surface, scoped collection
callbacks, and the V1.1 immutable-read withdrawal land. This document records
the *decided* design. Items marked **[implemented]** already hold today; items
marked **[specified]** are settled design that is not yet built; items marked
**[deferred]** are explicitly out of scope for v1.

## 0. Why this document is the keystone

Three separate threads all wait on one model:

1. The higher-order stdlib surface (`map` / `fold` / `for_each` families) cannot
   harden without a capability-polymorphic callback type, or it splits
   combinatorially into `map`, `map_file`, `map_alloc`, … (the problem named in
   `research/language/capability-polymorphism.md`).
2. Scoped collection callbacks (`with_value` / `with_value_mut` / `modify`) are
   the V1.1 tier of the H1 resolution (ROADMAP #8a): they give borrowed access
   to a stored element where the borrow is scoped to the call and cannot escape.
3. The last open half of H1 — the immutable read accessor
   `HashMap::get -> Option<&V>` (and its siblings) — can only be **withdrawn**
   once scoped callbacks exist to replace it. Until then it stays disclosed and
   frozen (see `docs/KNOWN_HOLES.md`, H1).

Designing these as one model — rather than bolting on a closure type when the
HOF surface needs it — is what keeps Concrete's "no semantically dark
constructs" frame intact. A callback that silently captures state, or silently
erases the capabilities of the code it runs, would be exactly such a dark
construct.

## 1. The governing decisions

These are the design calls this document encodes. They are deliberate and
constrain everything below.

1. **Bound callbacks are an explicit function pointer + context.** There are no
   closures and no hidden captures. A "bound callback" is the explicit pairing
   of (a) a function pointer whose first parameter is the context and (b) a
   context value the caller supplies. The combinator takes both, by separate
   parameters. Nothing is captured implicitly.
2. **There are exactly three context modes:** shared `&Ctx`, mutable `&mut Ctx`,
   and consuming `Ctx`. These correspond to the read / mutate / one-shot shapes.
3. **Capabilities live on the callback function type and are required at the
   call site.** A callback typed `fn(&Ctx, A) with(C) -> R` may exercise exactly
   the authority in `C`; the combinator that calls it must itself hold `C` (via
   capability polymorphism, `cap C`). Capabilities are never erased by passing a
   function through a combinator.
4. **No hidden captures.** All state a callback touches arrives either as its
   context parameter or as an ordinary argument. There is no environment.
5. **Scoped collection callbacks cannot receive a context that can reach the
   borrowed container** (the *container-not-in-context* invariant). This is the
   soundness keystone that makes `with_value` sound *without* a provenance /
   lifetime system.
6. **No references are returned from these APIs.** A scoped callback yields a
   borrowed `&V` / `&mut V` *into* the callback for the duration of the call; the
   combinator returns only owned values (`Option<R>`, counts, etc.). This is the
   same no-aggregate-ref subtraction that fixes H1 (ROADMAP #8a).
7. **`from(param)` returned references stay deferred** (ROADMAP #8a1). They are
   not this design's job and are not the H1 fix.

## 2. What already exists today [implemented]

Concrete already has the *use-site binding* form of capability polymorphism.
This document builds on it; it does not replace it.

```con
// Capability-set type parameter `cap C`. The capset lives on the inner fn
// type; `C` is bound by the caller's ordinary function-level cap polymorphism.
fn apply<T, U, cap C>(f: fn(T) with(C) -> U, x: T) with(C) -> U {
    return f(x);
}
```

- `cap C` declares a capability-set type parameter (`Concrete/Ast.lean`:
  `capParams`). `CapSet` supports concrete caps, variables, and union.
- A function pointer type carries its capset: `fn(T) with(C) -> R`
  (`Ty.fn_ params capSet retTy`). Function pointers are `Copy` and capture
  nothing (`docs/VALUE_MODEL.md`).
- Calling through a function pointer **requires the caller to hold the fn type's
  capability set**, in both `Check` and `CoreCheck`. The smuggling hole — a
  pure-looking wrapper accepting `f: fn(i32) with(Network) -> i32` and calling
  it — is closed (2026-06-09), locked by `adversarial_neg_cap_fnptr_smuggle.con`
  (rejected, E0240) and `cap_fnptr_declared.con` (positive).
- Capability-set subset checking at the call site is enforced
  (`error_cap_poly_insufficient.con`: a caller without `File` cannot satisfy a
  `with(C)` instantiated to `{File}`).

The decided representation question (§4) is therefore **already answered by the
language**: Concrete uses use-site binding, not a struct-level `BoundFn<Ctx, A,
R, Caps>`. What is *not* yet built is the bound-callback *context* threading
(§3), the scoped collection callbacks (§5), and the elision shortcut (§7).

## 3. Bound callbacks: function pointer + explicit context [specified]

A bound callback is two things passed explicitly:

- a **function pointer** whose **first parameter is the context**, and
- a **context value** the caller owns and supplies.

The context mode is expressed in the type of that first parameter. There is no
new type constructor and no environment object — the "binding" is just the
combinator taking the context as its own parameter and forwarding it.

### 3.1 The three context modes

| Mode | Fn-pointer shape | Context threading | Callable how many times | Use |
|---|---|---|---|---|
| **Shared** | `fn(&Ctx, A) with(C) -> R` | combinator holds `&Ctx`, passes `&Ctx` each call | many | read-only accumulation against fixed context |
| **Mutable** | `fn(&mut Ctx, A) with(C) -> R` | combinator holds `&mut Ctx`, reborrows `&mut Ctx` each call | many | folding into a mutable accumulator / sink |
| **Consuming** | `fn(Ctx, A) with(C) -> R` | combinator owns `Ctx`, moves it into the one call | **at most once** | one-shot transform that consumes its context |

Canonical signatures (combinator side):

```con
// Shared context: ctx borrowed immutably, callback invoked per element.
fn for_each_with<T, Ctx, cap C>(
    self: &Vec<T>, ctx: &Ctx, f: fn(&Ctx, &T) with(C) -> Unit
) with(C)

// Mutable context: ctx borrowed mutably, reborrowed on each call (a real fold
// into a caller-owned sink, no return-by-aggregate needed).
fn for_each_into<T, Ctx, cap C>(
    self: &Vec<T>, ctx: &mut Ctx, f: fn(&mut Ctx, &T) with(C) -> Unit
) with(C)

// Consuming context: the single call owns ctx. Only valid for one-shot APIs
// (it cannot be the per-element callback of a for_each — see §3.3).
fn with_owned<Ctx, R, cap C>(
    ctx: Ctx, f: fn(Ctx, ()) with(C) -> R
) with(C) -> R
```

Call site (no capture; context is explicit):

```con
struct Sum { total: Int }
fn add(acc: &mut Sum, x: &Int) { acc.total = acc.total + *x; }

fn main() -> Int {
    let mut s: Sum = Sum { total: 0 };
    v.for_each_into(&mut s, add);   // s threaded explicitly; `add` captures nothing
    return s.total;
}
```

### 3.2 Copyability, movability, linearity

The two halves bind differently and that is the whole point of having no
environment:

- **The function pointer is always `Copy`** (value model, unchanged). Passing it
  to a combinator copies it; there is nothing to consume.
- **The context's discipline is the context type's own discipline**, threaded by
  the mode:
  - *Shared* (`&Ctx`): the context is borrowed immutably for the duration of the
    combinator call; it is neither moved nor mutated; it remains usable by the
    caller after the call. A `Copy` or linear `Ctx` both work.
  - *Mutable* (`&mut Ctx`): the context is exclusively borrowed for the duration;
    the combinator **reborrows** `&mut Ctx` for each per-element call (the same
    reborrow discipline as a borrow block, applied N times in sequence — never
    two live `&mut` simultaneously). It is not consumed; the caller regains it
    when the combinator returns.
  - *Consuming* (`Ctx`): the context is **moved** into the combinator and then
    into the single callback call. If `Ctx` is linear it is consumed there
    (and the callback is responsible for consuming/returning it per linearity);
    if `Ctx` is `Copy` the move is a copy.
- **Linear resources in the context are fully visible.** Because the context is
  an ordinary value parameter, a linear field inside `Ctx` is tracked by the
  ordinary linear checker — there is no environment in which a linear value could
  be silently captured and dropped. A `for_each_into` whose `&mut Ctx` holds a
  linear `File` cannot leak it: the file is still owned by the caller's `Ctx`
  binding and must be consumed at its scope as usual.

### 3.3 Why consuming mode is one-shot only

A `for_each` calls its callback once per element. A consuming callback
`fn(Ctx, A) -> R` moves `Ctx` into its single call, so it cannot be the
per-element callback of a multi-element iteration — there is no `Ctx` left for
the second element. This is enforced by ordinary linearity (use-after-move on
`ctx`), not by a special rule. Consuming mode is therefore reserved for one-shot
combinators (`with_owned`, `with_value` over a single key, a builder's
`finish`), and the combinator signature makes the arity explicit by taking `Ctx`
by value rather than `&mut Ctx`.

### 3.4 Reborrowing `&mut Ctx`

Per-element mutable callbacks need a fresh `&mut Ctx` for each call without ever
holding two simultaneously. This is the borrow-block reborrow discipline already
in the language, applied in a loop: the combinator's loop body opens a reborrow
of its own `&mut Ctx`, passes it to `f`, and the reborrow ends when `f` returns,
before the next iteration. No aliasing `&mut` ever exists. The combinator may
not hand the same `&mut Ctx` to two pending calls because it never has two
pending calls — iteration is sequential.

## 4. Representation: use-site binding, not `BoundFn<…>` [decided]

ROADMAP #24 required comparing two bound-callback representations:

- **(A) struct-level capability parameters** — a `BoundFn<Ctx, A, R, Caps>`
  value that packages context + function + a capability set as one type.
- **(B) Concrete-shaped use-site binding** — the capset lives on the inner fn
  type (`f: fn(&Ctx, A) with(C) -> R`) and `C` is bound by the caller's ordinary
  function-level capability polymorphism (`cap C`).

**Decision: (B), use-site binding.** Reasons:

1. It is **already implemented** (§2). `cap C` + `fn(T) with(C) -> R` ship today;
   (A) would be a new first-class value type with its own value-model, audit, and
   proof story.
2. It keeps capabilities **on the type the callback actually has**, checked at
   the call site, with the lattice subset rule the checker already performs. (A)
   would reify a capability set as part of a runtime value's type — closer to the
   "first-class capability values" the research note explicitly rejects.
3. It avoids introducing a value that *looks* like a closure. A `BoundFn` value
   that carries a context is one rename away from "the environment object"; (B)
   keeps context an ordinary, separately-visible parameter, preserving "no hidden
   captures" by construction.

`BoundFn` is **[deferred]**: not rejected forever, but it earns its place only if
a real workload needs to *store* a bound callback as a first-class value (e.g. a
dispatch table of context+fn pairs). Until then it is unnecessary, and adding it
early would re-open the closure/value-model questions this design closes.

## 5. Scoped collection callbacks (V1.1 H1 tail) [specified]

These replace the immutable read accessor `get -> Option<&V>` that is the last
open half of H1. They give the callback borrowed access to a stored element,
scoped to the call.

```con
// Shared read of one element, scoped to f. Returns f's result BY VALUE.
fn with_value<K, V, Ctx, R, cap C>(
    self: &HashMap<K, V>, key: &K, ctx: &Ctx,
    f: fn(&Ctx, &V) with(C) -> R
) with(C) -> Option<R>

// Mutable access to one element in place, scoped to f. No &mut V escapes.
fn with_value_mut<K, V, Ctx, R, cap C>(
    self: &mut HashMap<K, V>, key: &K, ctx: &mut Ctx,
    f: fn(&mut Ctx, &mut V) with(C) -> R
) with(C) -> Option<R>

// Convenience over with_value_mut for the no-result case.
fn modify<K, V, Ctx, cap C>(
    self: &mut HashMap<K, V>, key: &K, ctx: &mut Ctx,
    f: fn(&mut Ctx, &mut V) with(C) -> Unit
) with(C) -> Bool
```

The borrowed `&V` / `&mut V` lives only inside `f`'s activation. The combinator
returns `Option<R>` where `R` is whatever `f` produces *by value* — never a
reference. The container is borrowed (`&self` / `&mut self`) for the whole call,
so it cannot be reallocated by anyone else for the duration.

This is the borrow-block trick generalized from a lexical scope to a data
structure: the element borrow is real, but it is provably confined to the
callback's activation, so no provenance tracking is needed.

### 5.1 The container-not-in-context invariant (soundness keystone)

`with_value` is sound **iff the callback cannot reach the container through its
context** and rehash / realloc / mutate-structure it while holding the borrowed
element. Otherwise an `insert` inside the callback could grow and move the table,
leaving the live `&V` dangling — the exact H1 failure, smuggled through the
callback's context.

**Invariant:** *the container being accessed must not be reachable from the
callback's context.*

How it is enforced (mostly by rules that already exist):

1. **References cannot be stored in aggregates** (the flat no-aggregate-ref ban,
   permanent — ROADMAP #8a). A context struct therefore **cannot hold a
   `&HashMap` / `&mut HashMap` field**. This removes the obvious smuggling path
   structurally.
2. **The container is already borrowed by the call.** `with_value_mut` takes
   `&mut self`; while that borrow is live, no second `&mut self` can be formed,
   so the `&mut Ctx` passed alongside cannot also be (or contain) `&mut self`.
   The ordinary borrow checker rejects passing a context that re-borrows the
   receiver.
3. **A residual gate check** (`scripts/tests/check_callable_values.sh`) pins
   that the `Ctx` type of a scoped-callback call does not name the container type
   by reference, so that even if (1) or (2) were ever weakened, a context that
   structurally mentions the container is rejected at the API boundary with a
   named diagnostic rather than silently compiling.

Because of (1) the invariant is *largely a theorem about the existing rules*,
not a new analysis: with no aggregate refs and a live receiver borrow, there is
no well-typed way to put the container in the context. The gate exists to keep
that property from regressing and to give a precise error if someone tries.

### 5.2 What this withdraws

Once `with_value` ships, `HashMap::get -> Option<&V>` and its read-accessor
siblings (`OrderedMap::get`/`min_key`/`max_key`, `OrderedSet::min`/`max`,
`Vec::get`, `Slice::get`, `Deque::get`, `BinaryHeap::peek`) are withdrawn in
favor of scoped reads (Copy values keep their by-value `get` where one exists).
This closes the immutable half of H1. The mutable half is already closed
(`get_mut` withdrawn, replaced by `update`, 2026-06-11). See `docs/KNOWN_HOLES.md`.

## 6. Capability polymorphism for combinators [implemented base / specified surface]

The combinator carries whatever its callback carries, no more, no less:

```con
fn map<T, U, cap C>(xs: &Vec<T>, f: fn(&T) with(C) -> U) with(C) -> Vec<U>
```

- If `f` is pure, `C = {}` and `map` is pure.
- If `f` is `with(File)`, `map` requires `with(File)`.
- A caller satisfies `with(C)` by holding a superset of the instantiated `C`,
  expanded through the capability lattice (`Concurrent ⊇ Async`, etc.).

The base machinery (`cap C`, fn-type capsets, call-site subset check, smuggle
rejection) is **[implemented]**. The surface work remaining is mechanical:
giving the stdlib HOF family these signatures and the elision shortcut below.

**Elision shortcut [specified].** Per the research note, the common case should
need no explicit `cap C`. A combinator whose only capability source is its
callbacks may write the propagated form (e.g. `with(f)` / inferred union) and the
checker computes the required set as the union of the capsets of all callbacks
invoked in the body. This is set propagation, not row unification.

**Out of scope for v1** (per research note "what can wait"): capability-set
bounds (`where C ⊆ {…}`), capability subtraction, and first-class capability
values. None are needed for the HOF surface.

## 7. How `for x in …` desugars [decided]

`for` is a **loop construct, lowered directly to a bounded loop — not a bound
callback.** A `for x in xs { … }` body that reads or mutates outer locals does so
lexically, exactly like a `while`, with no context struct and no callback. This
is the no-closure-friendly answer: lexical iteration keeps direct access to outer
mutable locals because it never crosses a function boundary.

Bound callbacks (§3) are the *explicit* HOF surface (`map`/`fold`/`for_each`)
for when iteration is passed as a value (stored, composed, handed to `spawn`).
The two do not compete: `for` is the ergonomic in-place loop; the callback
combinators are for first-class, capability-polymorphic, composable iteration.
The internal iterator protocol that `for` lowers against (element-by-value vs
element-by-`&`/`&mut` binding) is specified with the iteration work (ROADMAP
#23) and must obey the same container-not-in-context confinement when it yields
borrowed elements.

## 8. Proof and evidence story [decided]

A call through a bound or scoped callback must have a **named obligation /
evidence shape** — it may not be a silent hole in the evidence surface.

**Granularity decision: generic contract + per-instance proof artifact.** The
combinator carries a *generic contract* describing its callback discipline
(e.g. "`with_value` calls `f` at most once and never after returning";
"`for_each_into` calls `f` exactly `len` times; the element borrow does not
escape the call"). Each monomorphized instantiation gets its own proof artifact,
reported as **`proved_for_instance`**.

- Audit / proof-status reports must distinguish `proved_for_instance` from any
  future `proved_generic` class (generic-once proofs are a later optimization;
  v1 proves per instance, which is sound and matches how monomorphization
  already specializes).
- The capability surface of a combinator is reportable: declared capset (with
  `cap` params expanded), inferred capset from callback uses, the match/mismatch
  verdict, and the per-call-site required set after substitution (this mirrors
  the report list in `research/language/capability-polymorphism.md`).
- A scoped callback's confinement (container-not-in-context, element borrow does
  not escape) is part of the contract the instance proof discharges, so the
  soundness keystone is evidence-bearing, not just a checker rule.

## 9. The gate [specified]

`scripts/tests/check_callable_values.sh` must prove, once the surface is built:

1. **Hidden captures are rejected** — there is no syntactic form that lets a
   callback reference a binding that is neither its context nor an argument.
2. **Callback capabilities are not erased** — a combinator calling a `with(C)`
   callback must itself require `C`; the smuggle fixture stays red
   (already locked: `adversarial_neg_cap_fnptr_smuggle.con`).
3. **Context resources remain visible** — a linear resource inside a context is
   tracked by the linear checker (cannot be silently dropped via a callback).
4. **The three consumption modes behave differently** — shared reuses context,
   mutable threads one exclusive borrow, consuming is one-shot (use-after-move on
   a second call is rejected).
5. **Container-not-in-context is enforced** — a scoped-callback call whose
   context type reaches the accessed container by reference is rejected with a
   named diagnostic.
6. **Proof / evidence reports name the instantiated callable shape** — a call
   through a bound callback shows `proved_for_instance` and the combinator's
   capability surface is reportable.

Examples to add under `examples/callbacks/`:
`bare_fn`, `bound_shared`, `bound_mut`, `bound_once`, `cap_polymorphic_map`,
`with_value_mut` (plus a negative `container_in_context` that must be rejected).

## 10. What this design explicitly does NOT add [decided]

- **No closures / no environment.** Captures are always explicit context.
- **No `BoundFn<…>` first-class callable value** (§4) — deferred until a storage
  workload needs it.
- **No returned references from these APIs** (§1.6). Scoped access yields a
  borrow *into* the callback only.
- **No `from(param)` returned references** (ROADMAP #8a1, deferred). Not the H1
  fix; H1 is fixed by subtraction + scoped callbacks.
- **No `view struct … from(Bytes)`** (deferred with the above).
- **No first-class capability values, no effect handlers, no capability
  subtraction, no row-polymorphic effects** (research note "what not to add").
- **No implicit capability widening** — widening is explicit or via lattice
  subsumption only.

## 11. Build order

1. **[done]** Capability polymorphism base (`cap C`, fn-type capsets, call-site
   subset check, fn-ptr capability requirement / smuggle rejection).
2. **[this doc]** The model above — design gate for #24a satisfied by this file.
3. Bound-callback context threading (§3) + the elision shortcut (§6).
4. Scoped collection callbacks (§5) + the container-not-in-context gate (§9).
5. Withdraw the immutable read accessors (§5.2) — closes the H1 tail.
6. The capability-polymorphic HOF stdlib surface (`map`/`fold`/`for_each`) and
   the iteration protocol `for` lowers against (ROADMAP #23).

## 12. References

- `research/language/capability-polymorphism.md` — the permission-set (not
  effect) polymorphism model this builds on.
- `docs/VALUE_MODEL.md` — Copy / Clone / Move / Borrow; fn pointers are Copy and
  capture nothing.
- `docs/KNOWN_HOLES.md` — H1 (returned-reference provenance), mutable half
  closed, immutable half withdrawn by §5 here.
- ROADMAP Phase 5 #24 / #24a (this item), #8a / #8a1 (H1 by subtraction,
  `from()` deferred), #23 (iteration), #6b (inference through references — done,
  the prerequisite for `&T`-callback ergonomics).
