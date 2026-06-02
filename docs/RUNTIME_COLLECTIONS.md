# Runtime-Oriented Collection Maturity

Status: stable direction (freeze-ready)

This document settles the question posed by ROADMAP item 57:

> define runtime-oriented collection maturity explicitly before stdlib freeze: interpreters, analyzers, schedulers, and storage-like programs should have credible map/update patterns, nested mutable structure idioms, and frame-friendly environment/container patterns, with a clear decision about what belongs in stdlib versus example-only support.

For the broader stdlib direction, see [STDLIB.md](STDLIB.md).
For collection-free pressure work (fixed-capacity), see [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).

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

```concrete
struct Frame { bindings: HashMap<String, Value>; }
struct Env   { frames: Vec<Frame>; }
```

- `push_scope` / `pop_scope` are vector push/pop of a new `Frame`.
- Lookup walks `frames` from the top down until a matching key is found.
- Nested mutability is achieved by mutating the top frame's map; no cross-frame aliasing is required.
- Teardown is deterministic: `pop_scope` destroys the top frame, which linearly destroys the contained `HashMap`.

This is the shape `lox` and `mal` use today. It is not in stdlib because the value type `Value` is program-specific and because frame semantics differ across languages.

### 3.2 Intern pool = `Vec<String>` + `HashMap<String, u32>`

```concrete
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

```concrete
fn schedule(q: &mut Deque<Task>, t: Task) { q.push_back(t); }
fn next(q: &mut Deque<Task>) -> Option<Task> { q.pop_front() }
```

- Schedulers, BFS/worklist analyzers, and simple task runners use `Deque<T>` directly.
- Priority queues are explicitly out of scope for the first release (no heap type in stdlib).

### 3.4 Fact index (analyzer) = `HashMap<Key, Vec<Fact>>`

```concrete
fn add_fact(idx: &mut HashMap<Key, Vec<Fact>>, k: Key, f: Fact) with(Alloc) {
    match idx.get_mut(&k) {
        Option::Some { value } => value.push(f),
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

- **Lookup without mutation:** `map.get(&k) -> Option<&V>`. Returns a borrow; no allocation.
- **Mutating lookup:** `map.get_mut(&k) -> Option<&mut V>`. Returns a mutable borrow; caller mutates in place.
- **Insert-or-replace:** `map.insert(k, v)`. Transfers ownership of `v` into the map; prior value (if any) is returned as `Option<V>` for the caller to destroy.
- **Remove:** `map.remove(&k) -> Option<V>`. Caller owns the returned value and is responsible for destroying it.
- **Iteration order:** unspecified for `HashMap`/`Set`, deterministic ascending for `OrderedMap`/`OrderedSet`, insertion order for `Vec`/`Deque`.

Mutating a value inside a `HashMap` is the `get_mut` path. There is no "entry API" (à la Rust's `Entry`) in the first release; the two-line match idiom in section 3.4 covers the pressure cases without a new type.

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
4. The addition preserves the stdlib design rules in [STDLIB.md](STDLIB.md): explicit allocation, explicit ownership, small and sharp, one vocabulary.

Patterns that fail any of these remain example-shaped. That is not a negative judgment on the pattern; it is a statement that the evidence does not yet support a stable stdlib commitment.

---

## 7. Freeze Close-Out Status

The first-release runtime-collection surface is accepted as freeze-ready on the current evidence:

- [x] `HashMap::get_mut` and `OrderedMap::get_mut` exist and return `Option<&mut V>`.
- [x] `insert` on map types returns the displaced value as `Option<V>`.
- [x] `Deque<T>::push_back` / `pop_front` / `push_front` / `pop_back` exist and are covered by stdlib tests; the scheduler/work-queue shape in section 3.3 remains the canonical usage pattern.
- [x] Stdlib maps accept explicit `hash` / `eq` function pointers (no trait dependency).
- [x] Runtime-heavy evidence is sufficient: `lox` runs end-to-end against the frozen surface, and the remaining drift (`Vec<Binding>` tables rather than `HashMap<String, Value>` + `Vec<Frame>`) is example-shape, not a missing stdlib API. A canonical-shape rewrite remains useful follow-up evidence, but not a freeze blocker.
- [x] `Env`, `Frame`, `InternPool`, and multimap variants are intentionally example-shaped here and in [STDLIB.md](STDLIB.md).

---

## 8. Reconsideration Triggers

The stable surface in section 2 is reconsidered only if item-67 medium-workload evidence produces:

- A third unrelated example that re-implements an environment, intern pool, or multi-index shape identically to two prior ones — then the shape may graduate to stdlib.
- A persistent/immutable-collection workload that cannot be reasonably expressed with linear `insert`/`remove` (unlikely in the kinds of programs Concrete targets).
- A scheduler workload that needs priorities and where building a heap from `Vec<T>` becomes the bottleneck.

Any such finding is recorded in the stdlib gap ledger and triggers a scoped design revision. Until then, the surface in section 2 is the frozen runtime-collection commitment.
