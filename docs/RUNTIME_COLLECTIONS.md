# Runtime-Oriented Collection Maturity

Status: PARTIALLY SUPERSEDED (2026-06-11) — the borrowed-accessor design below
is being withdrawn. The `map.get(&k) -> Option<&V>` / `map.get_mut(&k) ->
Option<&mut V>` "lookup" and "mutating lookup" rows (§4) are the **H1 known
hole**: aggregate-wrapped returned references that compile but are unsound (a
saved ref can survive a rehash). Per the H1 resolution (ROADMAP Phase 7,
[KNOWN_HOLES.md](KNOWN_HOLES.md) H1) they are replaced by value/operation APIs
(`contains`, value-`get` for Copy, `remove -> Option<V>`, `update(k, fn(V) ->
V)`), owned `ByteView` for stored zero-copy, and scoped callbacks
(`with_value`/`with_value_mut`, V1.1). Treat the `Option<&V>` / `Option<&mut V>`
accessor sections below as historical, not the intended surface.

Original status: stable direction (freeze-ready)

This document settles the question posed by ROADMAP item 57:

> define runtime-oriented collection maturity explicitly before stdlib freeze: interpreters, analyzers, schedulers, and storage-like programs should have credible map/update patterns, nested mutable structure idioms, and frame-friendly environment/container patterns, with a clear decision about what belongs in stdlib versus example-only support.

For the broader stdlib direction, see [STDLIB.md](stdlib/STDLIB.md).
For collection-free pressure work (fixed-capacity), see [STDLIB_DESIGN_PRINCIPLES.md](stdlib/STDLIB_DESIGN_PRINCIPLES.md).

---

## 1. The Problem

Runtime-oriented programs — interpreters (`lox`, `mal`), analyzers, schedulers, VMs (`vm`), policy engines — share a handful of access patterns that fixed-capacity and parser workloads do not exercise:

1. **Symbol-to-value maps** with string keys and dynamic values (variable environments, symbol tables, intern pools).
2. **Nested mutable environments** (lexical scopes, call frames, stack machines).
3. **Frame-stack push/pop** patterns where a scope is entered, mutated, and torn down.
4. **Mutable values inside maps** (updating a field, not replacing the whole entry).
5. **Long-lived intern pools** that outlive individual frames.

The question is: which of these patterns does the first-release stdlib support directly, and which are left as example-shaped helpers that programs build from primitives.

---

## 2. Stable Stdlib Surface (frozen target)

The first-release stdlib commits to the following collection vocabulary for runtime-oriented code:

| Type | Module | Keys / values | Ownership | Use |
|---|---|---|---|---|
| `Vec<T>` | `std.vec` | indexed by `u64` | linear (owns `T`) | dynamic arrays, stack frames, intern pools |
| `HashMap<K, V>` | `std.map` | user-provided hash/eq fn ptrs | linear (owns `K`, `V`) | symbol tables, general key-value |
| `OrderedMap<K, V>` | `std.ordered_map` | sorted by comparator | linear | deterministic iteration order |
| `OrderedSet<T>` | `std.ordered_set` | sorted | linear | deterministic membership |
| `Set<T>` | `std.set` | user-provided hash/eq | linear | membership |
| `Deque<T>` | `std.deque` | indexed | linear | scheduler queues, BFS workloads |
| `Bytes` | `std.bytes` | indexed | linear | byte buffers |
| `String` | `std.string` | — | linear | UTF-8 text |

These are enough for every interpreter, analyzer, and scheduler we have written so far. They are all linear (ownership-honest), all visible `with(Alloc)`, and none requires a trait system to instantiate — keys carry their hash/eq as function pointers, Zig-style.

**Owned-resource collections.** The intended long-term rule is that collections
own their live elements. Dropping or clearing a collection destroys every live
non-`Copy` element exactly once; `pop`, `remove`, and `swap_remove` move an
element out and transfer ownership to the caller. H18 CLOSED 2026-07-16:
every shipped container destroys its live non-`Copy` elements through
compiler drop-glue (the `Destroy`-bounded impls + conditional `impl Destroy`
for composition), with no `drop_with(f)` APIs and no stored destructor
function pointers — exactly the rules below, now implemented and gated
(COLLECTIONS-DROP-GLUE).

**Drop-glue rules (RESOLVED 2026-07-15 — one source of truth; both review
threads converged here).** The enforcer behind every rule below is
**linearity itself**: the must-consume checker (E0208/E0294, enforced by the
H12-checked front end) makes an unconsumed non-`Copy` value a COMPILE ERROR.
That is what turns these rules from conventions into guarantees — you cannot
compile code that drops a `File` on the floor, forgets to drain a
`Vec<File>`, or leaks a `Vec<String>` without `.drop()`. Everything below is
sound because the checker won't let you forget.

Four decisions, each marked permanent or v1:

1. **`Destroy` is infallible-only — PERMANENT semantic rule, not a v1
   restriction.** A destructor cannot return `Result` (what would a
   container's drop do with 50 close errors?), so any type whose cleanup can
   fail must NOT implement `Destroy`. Fallible cleanup stays an explicit
   linear consumer — `close(self) -> Result` — exactly as `io.Writer`/
   `TextFile` already ship. This is the error-honesty rule (13t/E0286)
   applied to generated code: swallowing close errors inside drop-glue would
   reintroduce the must-use violation the checker exists to prevent, in the
   one code path nobody reads.
2. **`Destroy` propagates conditionally — PERMANENT.** `Vec<T>: Destroy` iff
   `T: Destroy` (same conditional-impl machinery as conditional `Copy` on
   `Option`/`Result`). Consequence: `Vec<File>` has NO `.drop()` — the
   compile error names the reason, and the idiom is **drain-and-close**:
   `pop()` each element, handle each `Result` visibly, then drop the empty
   container. Every close error has an owner. The rule COMPOSES recursively:
   `Vec<Vec<ConsoleHandle>>: Destroy` because `Vec<ConsoleHandle>` is, and
   the glue's capability set unions up the nesting the same way — nested
   containers need no extra rules.
3. **Drop-glue capabilities are derived and visible — PERMANENT (final
   model).** The glue's `with(...)` is the union of the element/key/value
   destructors' capabilities, computed per instantiation at monomorphization
   and stamped on the container's `drop` signature. A drop that performs
   authority-bearing work without that authority in its signature would be a
   capability-laundering path (the fs.con leak class, arriving through
   generated code). This invariant is non-negotiable.
4. **v1 glue is `with(Alloc)` — DOCUMENTED V1 RESTRICTION, not the final
   model.** Under rule 1, every `Destroy` impl the stdlib actually has needs
   at most `Alloc` (String, Bytes, nested containers — memory cleanup), so
   rule 3 holds degenerately and the front-end machinery for symbolic
   "capabilities of `T`'s destructor" in generic checking (an associated
   capability set — new Check machinery, beyond the shipped `<cap C>`
   parameter propagation) is deferred until a workload writes a genuinely
   infallible non-`Alloc` destructor (plausible candidate: a `with(Device)`
   buffer release). When that pull arrives, rule 3's full mechanism is the
   pinned design; v1 code must not assume `Alloc`-only anywhere it would be
   load-bearing.
5. **Glue destroys LIVE slots only — and abort-not-unwind makes that the
   entire liveness story.** After `pop`/`remove`/`swap_remove`, the vacated
   slots are skipped; liveness comes free from the existing `len`/occupancy
   flags. Because Concrete aborts rather than unwinds, there is no
   panic-partial-init state to track — no Rust-style drop-flags, no
   unwind-safety analysis. This is why the design is materially simpler than
   Rust's.

Two adjacent rules pinned at the same time:

- **Disposal stays forced-explicit — PERMANENT.** The generated glue runs
  only inside an explicit `.drop()` / `defer x.drop()`; the compiler never
  inserts destruction at scope end. E0208 (linear value not consumed) remains
  the error. Implicit scope-end auto-drop is the precise step that would turn
  Concrete from linear to affine; no ergonomics pass may take it.
- **`replace(i, v) -> T` is the overwrite primitive.** Returning the
  displaced element is linear-honest and needs no glue; destroying `set(i, v)`
  is sugar available only for `T: Destroy`. Same for map `insert` returning
  the previous value.

**Read/write access split.** `get(i) -> Option<T>` is a copy-out API and exists
only when `T: Copy`. For non-`Copy` elements, safe code uses scoped callbacks
(`with_at`, `with_value`, and their mutation forms where the alias gate permits)
or explicit move-out (`pop`, `remove`, `swap_remove`). The OVERWRITE primitive
is `replace(i, v) -> T` (and map `insert` returning the previous value): the
displaced element is handed back to the caller, never silently destroyed — a
destroying `set(i, v)` is sugar available only for `T: Destroy`. Safe
collection APIs do not return `&T` / `&mut T`; references stay second-class
and scoped.

**Not in the stable surface** (deliberately deferred):

- `BTreeMap`/`BTreeSet` as distinct types from `OrderedMap`/`OrderedSet`. The ordered variants suffice; a tree-specific API can come later if workloads require predictable comparator-driven iteration beyond what sorted vectors give.
- `LinkedList`, `SinglyLinked*`, doubly-linked variants. No workload has justified the pointer overhead versus `Deque<T>`.
- Persistent / immutable / copy-on-write collections. Deferred until a workload provides evidence.
- Concurrent collections (`ConcurrentHashMap`, lock-free queues). Out of scope until the concurrency story is settled.
- Reference-counted collections (`Rc<Vec<T>>` and kin). Concrete rejects implicit refcounting; if a workload demands shared ownership, it uses explicit arena/cleanup patterns.

---

## 3. Canonical Patterns (the shape real code uses)

These are not stdlib types. They are the shapes that interpreters, analyzers, and schedulers keep building on top of the stable surface. Examples should use these shapes consistently so the pattern is discoverable.

### 3.1 Environment = `Vec<Frame>` of `HashMap<String, Value>`

```concrete pseudocode
struct Frame { bindings: HashMap<String, Value>; }
struct Env   { frames: Vec<Frame>; }
```

- `push_scope` / `pop_scope` are vector push/pop of a new `Frame`.
- Lookup walks `frames` from the top down until a matching key is found.
- Nested mutability is achieved by mutating the top frame's map; no cross-frame aliasing is required.
- Teardown is deterministic: `pop_scope` destroys the top frame, which linearly destroys the contained `HashMap`.

This is the shape `lox` and `mal` use today. It is not in stdlib because the value type `Value` is program-specific and because frame semantics differ across languages.

### 3.2 Intern pool = `Vec<String>` + `HashMap<String, u32>`

```concrete pseudocode
struct InternPool {
    by_id: Vec<String>;
    by_name: HashMap<String, u32>;
}
```

- `intern(s)` checks `by_name`, returns existing id or appends and returns the new index.
- IDs are `u32` and stable for the life of the pool.
- The pool outlives individual frames; lookups during interpretation use IDs, not strings.

This is a two-collection idiom, not a dedicated type. A dedicated `InternPool` type is reconsidered only if two or more example programs need identical semantics (section 6).

### 3.3 Work queue / scheduler = `Deque<Task>`

```concrete pseudocode
fn schedule(q: &mut Deque<Task>, t: Task) { q.push_back(t); }
fn next(q: &mut Deque<Task>) -> Option<Task> { q.pop_front() }
```

- Schedulers, BFS/worklist analyzers, and simple task runners use `Deque<T>` directly.
- Priority queues are explicitly out of scope for the first release (no heap type in stdlib).

### 3.4 Fact index (analyzer) = `HashMap<Key, Vec<Fact>>`

```concrete pseudocode
fn add_fact(idx: &mut HashMap<Key, Vec<Fact>>, k: Key, f: Fact) with(Alloc) {
    match idx.remove(&k) {
        Option::Some { value } => {
            let mut v: Vec<Fact> = value;
            v.push(f);
            idx.insert(k, v);
        },
        Option::None => {
            let mut v: Vec<Fact> = Vec::new();
            v.push(f);
            idx.insert(k, v);
        }
    }
}
```

- The "lookup-then-append-or-insert" pattern is the canonical way to accumulate facts under a key.
- No multimap type is provided in stdlib; the two-line idiom above is clear and matches the ownership story.

---

## 4. Access and Update Rules

Runtime workloads lean on the following conventions. These are not new; they are pulled together here so interpreters/analyzers can be written consistently.

- **Copy lookup:** `map.get(&k) -> Option<V>` exists only when `V: Copy`; it
  copies the value out and leaves the map unchanged.
- **Scoped lookup without mutation:** `map.with_value(&k, ctx, f) -> Option<R>`.
  The callback receives `&V`, and the reference cannot escape.
- **Scoped mutating lookup:** `map.with_value_mut(&k, ctx, f) -> Option<R>` or
  `map.modify(&k, f)` where the alias gate permits it. The callback receives
  `&mut V`, and the reference cannot escape.
- **Insert-or-replace:** `map.insert(k, v)`. Transfers ownership of `v` into the map; prior value (if any) is returned as `Option<V>` for the caller to destroy.
- **Remove:** `map.remove(&k) -> Option<V>`. Caller owns the returned value and is responsible for destroying it.
- **Iteration order:** unspecified for `HashMap`/`Set`, deterministic ascending for `OrderedMap`/`OrderedSet`, insertion order for `Vec`/`Deque`.

Mutating a value inside a `HashMap` is the scoped callback path. There is no
"entry API" (à la Rust's `Entry`) in the first release; the two-line match idiom
in section 3.4 covers the pressure cases without a new type.

---

## 5. What Stays Example-Only

These patterns recur but are deliberately kept out of stdlib because they are program-shaped, not library-shaped:

- **Environment / scope / frame types.** The shape of `Value` is program-specific; each interpreter builds its own `Env`.
- **Intern pools.** Two-collection idiom; no abstraction savings from a dedicated type yet.
- **Visitor / walker state.** Analyzers carry program-specific state that belongs in the program.
- **Multi-index / secondary-index collections.** Compose from two maps; the explicit shape is clearer than a generic abstraction.
- **Arena-allocated scratch collections.** When a workload needs them, the arena is the composable mechanism, not a new collection.

Examples may factor recurring helpers into their own local modules; they do not become stdlib until two unrelated examples prove the same API shape.

---

## 6. Promotion Rules (stdlib vs example)

A pattern moves from example-only into stdlib only if **all** of the following hold:

1. Two or more unrelated examples independently implement the same API shape.
2. The shape is stable across those uses (no per-program flags or type parameters that bend the signature).
3. The stdlib addition does not require a new language feature (trait, macro, GAT).
4. The addition preserves the stdlib design rules in [STDLIB.md](stdlib/STDLIB.md): explicit allocation, explicit ownership, small and sharp, one vocabulary.

Patterns that fail any of these remain example-shaped. That is not a negative judgment on the pattern; it is a statement that the evidence does not yet support a stable stdlib commitment.

---

## 7. Freeze Close-Out Status

The current runtime-collection surface is accepted as usable on the current
evidence, with H18 explicitly tracked as the Phase 7 owned-resource collection
closure:

- [x] Safe collection APIs do not return references; reads use Copy `get`, scoped
      callbacks, or explicit move-out.
- [x] `insert` on map types returns the displaced value as `Option<V>`.
- [x] `Deque<T>::push_back` / `pop_front` / `push_front` / `pop_back` exist and are covered by stdlib tests; the scheduler/work-queue shape in section 3.3 remains the canonical usage pattern.
- [x] Stdlib maps accept explicit `hash` / `eq` function pointers (no trait dependency).
- [x] Runtime-heavy evidence is sufficient: `lox` runs end-to-end against the frozen surface, and the remaining drift (`Vec<Binding>` tables rather than `HashMap<String, Value>` + `Vec<Frame>`) is example-shape, not a missing stdlib API. A canonical-shape rewrite remains useful follow-up evidence, but not a freeze blocker.
- [x] `Env`, `Frame`, `InternPool`, and multimap variants are intentionally example-shaped here and in [STDLIB.md](stdlib/STDLIB.md).

---

## 8. Reconsideration Triggers

The stable surface in section 2 is reconsidered only if item-67 medium-workload evidence produces:

- A third unrelated example that re-implements an environment, intern pool, or multi-index shape identically to two prior ones — then the shape may graduate to stdlib.
- A persistent/immutable-collection workload that cannot be reasonably expressed with linear `insert`/`remove` (unlikely in the kinds of programs Concrete targets).
- A scheduler workload that needs priorities and where building a heap from `Vec<T>` becomes the bottleneck.

Any such finding is recorded in the stdlib gap ledger and triggers a scoped design revision. Until then, the surface in section 2 is the frozen runtime-collection commitment.
