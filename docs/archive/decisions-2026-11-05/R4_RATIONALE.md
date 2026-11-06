# R4 — Concrete rules for the final review's findings (Claude, resolution round)

This document answers every high and medium finding of Codex's final review (`dtr/final.log`) with a rule the owner can accept or reject as written. It does not change any file in the repository.

Conventions:
- **Spellings** marked *(spelling: owner)* are placeholders. Every surface spelling goes to the owner, per the owner-reviews-all-language-changes rule.
- **REVISES X — owner decision** marks a rule that changes a recorded decision or a conformance example.
- **OPEN — owner** marks a question the decisions leave open. I list the options and give a recommendation; nothing is decided silently.
- **Lens.** Code is mostly written by models, so every rule is chosen for checking and reading, never for writing effort. A rule is preferred when a reader can predict behaviour from the signature alone, and when failure appears in a type rather than in a trap.

Citations are to main at `b78fff1a5`.

---

## F1. Allocation progress (Q1.11, Q8.7) — high

Grounding: `docs/decisions.md:432–439` (fallible allocation that conserves its input; the allocator stays valid), `docs/compiler/VERIFIED_PIPELINE.md:118–128` (adequacy is independent of reported success, and an always-failing allocator cannot satisfy it), `docs/compiler/A1_BOUNDARIES.md:103–116`.

**Rule F1.0: valid layouts.** `Layout = { size : u64, align : u64 }` is valid when:
- `align` is a power of two and at most the target's `MaxAlign` (4096 on `linux-x86_64-hosted`);
- `size ≤ MaxAlloc` (the target's checked maximum allocation size; Q3.5).

All arithmetic below is on mathematical naturals, so it never wraps. A request whose size computation (for example `cap · size_of(T)`) exceeds `MaxAlloc` is **TooLarge**: it fails without any layout and without touching any state.

**Rule F1.1: availability is a predicate, not an outcome.** The spec defines `Avail : AllocState → Layout → Prop` for each allocator kind, and both kinds include address-space availability.
- **Arena:** the arena owns the address interval `[σ.base, σ.base + σ.capacity)`, with `σ.base + σ.capacity ≤ 2^64 − 1`, and allocates upward from `σ.base + σ.top`. With `start = alignUp(σ.base + σ.top, L.align)` (absolute address), `Avail σ L ↔ L valid ∧ start + L.size ≤ σ.base + σ.capacity`. The new block is `[start, start + L.size)`, inside the arena's interval and above every earlier block, so it is nonoverlapping by construction.
  - `remaining(a: &Allocator<A>) with(Alloc) -> u64` (`= capacity − top`) is observable, and it is the only allocator statistic exposed.
- **System allocator (`Sys`):** an independent storage model, not an unconstrained oracle.
  - `σ` records the set of live blocks, as address intervals in `[1, 2^64 − 1)`, which pairwise do not overlap in occupied bytes (F13), and `inUse σ`, the sum of their sizes.
  - `Avail σ L ↔ L valid ∧ inUse σ + L.size ≤ HeapBudget ∧ ∃ start, start % L.align = 0 ∧ [start, start + L.size) ⊆ [1, 2^64 − 1) ∧ that interval overlaps no live block of σ`.
  - On success, the block returned is such an interval. The `∃` is the model's statement; the source sees only `Ok` or `Err`.
  - `HeapBudget` is a component of the target budget `B` in `Enough(P, env, B)`.
  - That the real system allocator succeeds whenever this holds is a named trusted-base assumption of the target. It is not hidden.
  - Nothing about `σ` is observable from source.

**Rule F1.2: the four laws, stated as Lean theorems about every allocator primitive.**

| law | statement |
|---|---|
| Progress | `Avail σ L` → the allocation returns `Ok`. |
| Failure frame | On `Err`, the allocator state is unchanged: the same `top`, the same live set and the same `Avail` for every layout. A failed allocation never consumes capacity. |
| Release monotonicity | Releasing a block never makes an available layout unavailable. In an arena, releasing a block adds nothing, and capacity returns only on reset. |
| Realizability | For each target, an explicit state and budget satisfy `Enough` for a nonempty test program, giving a concrete success witness. A spec where every allocation fails fails qualification (Q8.7). |

**Rule F1.3: growth is concrete.**
- `push` requests storage only when `len = cap`. It then requests `max(2·cap, len+1)` elements, computed over naturals, and nothing else. The request is `TooLarge` if that element count exceeds `2^64 − 1` (possible even for zero-sized elements, whose `len` is a `u64`) or its byte size exceeds `MaxAlloc`. A zero-sized element type's byte size is 0, so its growth needs no storage and fails only as `TooLarge`.
- `reserve(n)` is a no-op returning `Ok` when `cap ≥ len+n`. Otherwise it requests exactly `len+n` elements, and it is `TooLarge` if `len+n` overflows `u64` or the byte size exceeds `MaxAlloc`.

This makes "storage available" a decidable question about one layout.

Signatures (decided shape from `decisions.md:432`; the payload types here are new):

```
fn alloc<T, A>(a: &mut Allocator<A>, v: T) with(Alloc) -> Result<Heap<T, A>, AllocFailure<T>>
fn push<T, A>(v: &mut Vec<T, A>, a: &mut Allocator<A>, x: T) with(Alloc) -> Result<Unit, AllocFailure<T>>
fn reserve<T, A>(v: &mut Vec<T, A>, a: &mut Allocator<A>, n: u64) with(Alloc) -> Result<Unit, AllocFailed>
enum AllocReason { NoSpace(Layout), TooLarge }          // Copy
struct AllocFailure<T> { value: T, reason: AllocReason } // the rejected value, moved back
struct AllocFailed { reason: AllocReason }               // Copy; nothing to return
```

Every failure leaves `v` exactly unchanged: the same `len`, `cap`, elements and backing.

**Example: failure, then retry.**

```
// Item: Destroy (an impl exists), so Vec<Item, A>: Destroy (F3).
fn add<A>(v: &mut Vec<Item, A>, cache: Vec<Item, A>, a: &mut Allocator<A>, x: Item)
    with(Alloc) -> Result<Unit, AllocFailure<Item>> {
    match push(v, a, x) {
        Ok(u) => { destroy(cache); return Ok(u); }
        Err(f) => {
            // f.value is x, untouched; v and a are unchanged (failure frame).
            destroy(cache);              // releases cache's backing (release monotonicity)
            return push(v, a, f.value);  // succeeds iff Avail now holds for the grown layout
        }
    }
}
```

What a reader can predict: whether the second `push` succeeds depends only on `Avail` after the release, not on hidden history.

## F2. Drain and traversal (Q1.12, Q2.4, Q2.5, Q3.6) — high

Grounding:
- Conservation, `decisions.md:388–393` (authority rule 9) and `:438` ("a consuming container operation returns the container when an obligation survives");
- callbacks are a function pointer plus explicit context, with exact capability sets, `decisions.md:368–371` (rule 4).

Two operations cover every case.
- `drain` consumes elements without producing new ones; it is the only way to empty a container of non-`Destroy` elements.
- `map_into` transforms `T` into `U`.

Both:
- process elements in increasing index order;
- run each callback at most once per element;
- keep callback effects, which are in the trace and are not undone (Q1.12).

The callback's capability set `K` is an explicit capability argument (per the Q5 never-infer revision).

```
enum Step<T, E> { Next, Stop { survivor: Option<T>, err: E } }

fn drain<T, A, C, E, K>(v: Vec<T, A>, ctx: &mut C,
                        f: fn(&mut C, T) with(K) -> Step<T, E>)
    with(Alloc, K) -> Result<Unit, Drained<T, A, E>>

struct Drained<T, A, E> {
    at: u64,                 // index of the element whose callback stopped
    survivor: Option<T>,     // that element, if the callback did not consume it
    rest: Vec<T, A>,         // indices at+1 .. len, untouched, in the ORIGINAL backing
    err: E,
}
```

- **Success:** every element was passed to `f` and is consumed, so `Next` means the callback took ownership. The backing is then released.
- **Stop:** the result accounts for every element.
  - Elements `0..at−1` were passed to their callbacks, which own them from then on. A callback may move what survives into `ctx` or into `E`, under ordinary ownership checking. Neither `Next` nor `Stop { survivor: None }` permits dropping an obligation.
  - Element `at` is consumed or is `survivor`.
  - The suffix is in `rest`, moved to the front of the original backing.
- **Early termination is a `Stop`.** There is no second mechanism.
- **`drain` never allocates**, so it has no allocation failure. `Alloc` is needed only to release the backing.

```
enum MapStep<T, U, E> { Next(U), Stop { survivor: Option<T>, err: E } }

fn map_into<T, U, A, C, E, K>(v: Vec<T, A>, a: &mut Allocator<A>, ctx: &mut C,
                              f: fn(&mut C, T) with(K) -> MapStep<T, U, E>)
    with(Alloc, K) -> Result<Vec<U, A>, MapError<T, U, A, E>>

enum MapError<T, U, A, E> {
    NoSpace { input: Vec<T, A>, requested: Layout },   // before any callback ran; input untouched
    Stopped { done: Vec<U, A>,       // transformed prefix 0..at−1, in the NEW backing
              survivor: Option<T>,   // element at, if the callback kept it
              rest: Vec<T, A>,       // untouched suffix at+1..len, in the ORIGINAL backing
              err: E },
}
```

- `map_into` allocates the output backing for `len` elements before calling any callback, so there is no allocation failure mid-traversal.
- On success the input backing is released.
- An empty `rest` or `done` still owns its backing until it is consumed (destroyed or drained).
- On `Stopped`:
  - nothing is reconstructed (the review's objection: a transformed `T` cannot be rebuilt);
  - the transformed prefix, the survivor and the suffix are three separate fields with their own owners;
  - the two backings are released only by their new owners.

In-place update without a hole (Q3.6): there is no element reference (`decisions.md:481–487`).
- `swap_at<T, A>(v: &mut Vec<T, A>, i: u64, x: T) -> T` replaces and returns the element; it traps if `i ≥ len`, like indexing.
- Transformations that consume are `map_into`.

Traps follow the fatal-exit rule (OD68, OD69).

## F3. The Destroy inventory (Q2.2, Q2.3, Q9.6) — high

> **Superseded 2026-11-05 (owner, row Q2.8, [decisions.md](../../decisions.md#decided-2026-11-05-revisions)):** the disjunctive bound of F3.1 is not adopted. Each container has separate disjoint impls (`T: Copy` and `T: Destroy`), sixteen named impls in all (row Q2.2); coherence uses Copy/Destroy disjointness only (F3.1b, revising TRAIT-1@1). The text below is kept as history.

Grounding: OD28 (`decisions.md:184`: Destroy only by an impl, and Copy types are not Destroy), OD35 (`:191`), rule 9 (`:388`: Destroy requires at most `Alloc`), OD59 (`:215`: no `free`).

**Rule F3.1: the bound "releasable".** A container may hold a Copy payload, which needs no destructor, or a `Destroy` payload. OD28 makes Copy types non-Destroy; taken here as disjointness: a type never has both. The condition is therefore a disjunction, `T: Copy | Destroy`. It is written only in container impls.

**OPEN — owner (new bound syntax).** The options:
- (a) a closed disjunctive bound `T: Copy | Destroy`, admitted only in `Destroy` impl heads;
- (b) two impls per container, one with `T: Copy` and one with `T: Destroy`, admitted under F3.1b;
- (c) only `T: Destroy`, which makes `Vec<u8, A>` non-destroyable and unusable.

Recommendation: (a). Both (a) and (b) are checkable. (a) is one impl per container, so a reader looks in one place, and the bound states the whole condition. (b) doubles the impls. `Result` (two different payload slots) is written as two impls under either option, because its condition is not a product of per-slot conditions.

**Rule F3.1b: coherence with disjointness.** Copy and Destroy are disjoint (OD28). Coherence may therefore use `T: Copy` versus `T: Destroy` to tell two impls apart. That is the only negative reasoning coherence admits, and it is part of the same owner item.

**Rule F3.2: the initial inventory.** It is exhaustive: no other type is Destroy, except user types with written impls. Each impl has a named identity in coherence, which is the first word in each table row: `Destroy@Array`, `Destroy@Heap`, `Destroy@HeapArray`, `Destroy@Vec`, `Destroy@String`, `Destroy@Option`, `Destroy@ResultOk`, `Destroy@ResultErr` and `Destroy@OrderedMap`. There are no others. An impl never applies to a Copy instance, since every listed type that could be Copy is excluded by its condition.

| owner type | `Destroy` when | needs | destruction order |
|---|---|---|---|
| `Destroy@Array`: `[T; N]` (language impl, OD35) | `T: Destroy` (only; a Copy `T` makes the array Copy) | `T`'s | index 0..N−1; no storage |
| `Destroy@Heap`: `Heap<T, A>` (primitive) | `T: Copy \| Destroy` | `Alloc` | payload (if Destroy), then the cell |
| `Destroy@HeapArray`: `HeapArray<T, A>` (primitive) | `T: Copy \| Destroy` | `Alloc` | 0..len−1, then the backing |
| `Destroy@Vec`: `Vec<T, A>` (std, over `HeapArray`) | `T: Copy \| Destroy` | `Alloc` | 0..len−1, then the backing |
| `Destroy@String`: `String<A>` (std) | always | `Alloc` | the backing |
| `Destroy@Option`: `Option<T>` (std) | `T: Destroy` | `T`'s | the payload if `Some` |
| `Result<T, E>` (std), two impls | `Destroy@ResultOk`: `T: Destroy, E: Copy \| Destroy`; `Destroy@ResultErr`: `T: Copy, E: Destroy` (disjoint by F3.1b; both Copy means Result is Copy and has no impl) | `T`'s ∪ `E`'s | the present payload |
| `Destroy@OrderedMap`: `OrderedMap<K, V, A>` (std) | `K: Copy \| Destroy` and `V: Copy \| Destroy` | `Alloc` | ascending key order; per entry key then value; then the nodes |

- `HashMap` is **not** in the initial inventory. Its destruction order would depend on hashing, and it is admitted only with a stated order.
- A struct or enum is Destroy only by a written impl. Its destructor is a checked consuming `destroy` body (Q2.7). The order of fields is whatever that body writes.
- A trap during destruction stops all cleanup (OD68).

## F4. Text views (Q4.6) — high

> **Superseded in part 2026-11-05 (owner, row Q4.7, [decisions.md](../../decisions.md#decided-2026-11-05-revisions)):** F4.3's text-only `slice … else` form with a mandatory `else` is not adopted. One scoped form opens every view, `borrow [mut] source [from a until b] as view { … } [else { … }]`; without `else` an invalid range traps, and a proven invalid range without `else` is refused (OD118). The text below is kept as history.

Grounding: OD127 (`decisions.md:283`: every String is UTF-8), Q4.3 (byte length), Q3.5 (`u64`), OD36 (`decisions.md:192`: scoped `borrow … as … { }` blocks), and `decisions.md:481–487`.

**Rule F4.1: offsets are bytes**, of type `u64`.

**Rule F4.2: the view's operations.**

| operation | signature | out of range |
|---|---|---|
| length | `len(t: &TextView) -> u64` | — |
| byte | `byte_at(t: &TextView, i: u64) -> u8` | traps if `i ≥ len`, exactly like array indexing; refused statically when provable (MEM-6, OD118) |
| boundary test | `is_boundary(t: &TextView, i: u64) -> bool` | false if `i > len` |
| equality | `eq(x: &TextView, y: &TextView) -> bool` | — (bytes) |
| order | `cmp(x: &TextView, y: &TextView) -> Ordering` | — (lexicographic bytes) |
| copy out | `to_string<A>(t: &TextView, a: &mut Allocator<A>) with(Alloc) -> Result<String<A>, AllocFailed>` | — |
| subview | scoped form below | fallible block |

A boundary is 0, `len`, or an index whose byte is not `0b10xxxxxx`.

**Rule F4.3: the subview form is scoped and fallible**, and its `else` branch is mandatory *(spelling: owner)*:

```
slice t from a until b as s { ... s is a TextView ... } else { ... range invalid ... }
```

- **Valid** means `a ≤ b ≤ len(t)` and both `a` and `b` are boundaries.
- **Why fallible, not a trap:** boundaries depend on runtime content. A failure the reader can see in the code is preferred to a trap.
- **Why `byte_at` traps:** it has the same contract as array indexing, and one indexing contract is enough.

**Rule F4.4: the binder.**
- `s` is second-class. It cannot be returned, stored or captured, the same as a borrow.
- Its root is `t`'s root plus a byte-range projection (Q3.3).
- It holds a shared loan of that root for the block, so the underlying `String` cannot be mutated inside the block.
- Nested `slice` blocks narrow the range further.
- A `StaticStr` opens the same view type, with a static root.

## F5. Provenance and the call-only extern annotation (Q3.7, Q3.9, Q9.7) — high

Grounding: OD64 (`decisions.md:220`: borrow-derived pointers are confined, `ptr::expose` is refused on them, and an extern parameter is call-only by default or kept), `decisions.md:91` (the syntax is still undecided).

**Rule F5.1: the memory model.**
- Memory is a set of blocks `b`, each with `base`, `size`, `align` and `alive`.
- A pointer is `(prov : Option BlockId, addr : u64)`.
- Integers carry no provenance, and integer arithmetic is ordinary arithmetic.

**Rule F5.2: exposure.**
- `ptr::expose<T>(p: *T) with(Unsafe) -> u64` adds `p.prov` to the state's exposed set and returns `p.addr`.
- It is refused statically on a borrow-derived pointer (OD64).
- When a block is released, it leaves the exposed set.

**Rule F5.3: reconstruction, a priority rule.** `ptr::from_exposed<T>(x: u64) with(Unsafe) -> *T` takes the first step that applies:
1. Exactly one live, exposed, **nonempty** block has `base ≤ x < base + size`: that block. Live nonempty blocks never overlap (F13), so at most one does.
2. Otherwise, exactly one live, exposed, **nonempty** block has `x = base + size` (one-past): that block.
3. Otherwise: no provenance.

Zero-sized blocks are never candidates.

| case | result |
|---|---|
| fabricated integer that falls in no live exposed block | no provenance |
| fabricated integer that happens to fall inside a live exposed block | that block's provenance (exposure makes the block's addresses public; that is the cost of `expose`) |
| `expose(p) + k`, still inside the same block | provenance of that block |
| exposed block released; address reused by a block not exposed | no provenance |
| reused address, new block also exposed | the new block, never the old one |
| one-past of b1 equals the base of b2 (both exposed) | b2 (in bounds wins) |
| only zero-sized blocks at `x` | no provenance (zero-byte accesses need none) |
| a pointer obtained earlier for b, after b is released | keeps `prov = b`; any access violates the liveness premise |

Accessing memory through a pointer requires every validity premise for the bytes read or written: provenance, in bounds, alive, aligned, initialized for a read, and write access for a write. A violated premise is a failed proof obligation of the `Unsafe` code. It is never defined behaviour, and the safe theorem assumes these premises (rule 5, `decisions.md:372`). A zero-byte access needs only alignment and a non-null address.

**Rule F5.4: the call-only extern annotation.** OPEN — owner (spelling only; the meaning follows OD64).
- **call-only (the default):** foreign code may read, and write only when the argument came from a `&mut` or an owned allocation, during the call only.
  - After the call returns, no access is allowed, and that is a listed trusted-base assumption.
  - A borrow-derived pointer may go only to call-only parameters.
- **kept:** a kept parameter does not take a raw pointer. It takes an owned allocation (`Heap<T, A>` or `HeapArray<T, A>`) **by value**, so the source gives it up.
  - The extern returns a linear receipt `Kept<H>`, where `H` is the consumed owned allocation type. It is a trusted std type with no operations.
  - The receipt keeps the allocator dependency alive (`decisions.md:429`), so the allocator cannot be given up while C holds the memory.
  - Its paired extern, named in the first extern's contract, consumes the `Kept<H>` and returns `H`. That ownership transition is checked in source.
  - That C stops using the memory before the paired call returns is a listed trusted-base assumption.
  - Borrow-derived pointers can never reach a kept parameter, because they are not owned allocations.

  This ownership design is substantive: **OPEN — owner**, not only a spelling.

Example with the recommended spelling *(spelling: owner)*. C receives `(pointer, length)` per the ABI; the source sees owned values only:

```
extern fn ring_submit(kept buf: HeapArray<u8, Sys>) -> Kept<HeapArray<u8, Sys>>
extern fn ring_complete(r: Kept<HeapArray<u8, Sys>>) -> HeapArray<u8, Sys>   // paired with ring_submit
extern fn checksum(p: *u8, len: u64) -> u32                                  // call-only (no marker)
```

- `Kept<H>` is linear, has no operations, and is not `Destroy`, so it can end only through the paired extern.
- While a `Kept<H>` exists, `H`'s allocator counts it as a live allocation.
- The contract of `ring_submit` names `ring_complete` as its pair. The checker verifies that the pair's parameter is exactly `Kept<H>` and that its result is `H`.

Spelling options:
- (a) a parameter marker `kept buf: H`, where `H` is an owned allocation type, and no marker means call-only;
- (b) an attribute `#[kept] buf: H`;
- (c) markers on both forms.

Recommendation: (a). The default is the safe case, and the exception is visible at the declaration.

## F6. The hosted target table (Q7.1–Q7.8) — high

> **Superseded in part 2026-11-05 (owner, row Q7.11, [decisions.md](../../decisions.md#decided-2026-11-05-revisions)):** the standard streams use the new `Console` capability, not `Device` (OPEN F6.1 is answered with option (c), revising authority rule 13). Every `with(Device)` on a `StdIn`, `StdOut` or `StdErr` operation in the table below reads `with(Console)`. The text below is kept as history.

Grounding:
- `decisions.md:356–380` (rules 1–6);
- `:403–408` (rule 12, grants passed to `main`);
- `:441–460` (filesystem, environment, time and network);
- `:465–495` (reset, who mints, `main` declares what it wants, mounts and DNS).

`docs/spec/trusted.md:35` assigns `Device` to device access.

**One initial target, `linux-x86_64-hosted`, requires Linux ≥ 5.6** for `openat2`. Every other target is deferred.

`main` declares the grants it wants as parameters. The launcher mints exactly those and maps each parameter name to the launch configuration. A parameter type this target does not offer is a compile error. A configured name that is missing refuses to start, before `main`, with exit code 125 *(value: owner)*.

| grant type | instances | launch config | operations (rights) | narrowing / descendants | ending it |
|---|---|---|---|---|---|
| `Allocator<Sys>` | ≤ 1 | — | alloc, arenas, `with(Alloc)` | arenas made from it (child owns backing, Q1.6) | `give_up(a) with(Alloc) -> Unit`, only with no live allocation |
| `Directory` | one per parameter name | `--dir NAME=PATH:RIGHTS` | rights ⊆ {list, read, write, create, remove}, `with(Fs)` | `narrow(d, r) with(Fs) -> Directory` (consumes; rights ∩ r; cannot fail); `open_dir(&d, path, r) with(Fs) -> Result<Directory, FsError>` (child, rights ⊆ parent ∩ r; `d` stays) | `close_dir(d) with(Fs) -> Unit` (consumes in every outcome) |
| `File` (acquired, not granted) | — | — | per open mode | none | `close(f) with(Fs) -> Result<Unit, CloseError>`; consumes in every outcome (Linux `close` always releases the descriptor); `CloseError` is Copy and holds no handle |
| `Environment` | ≤ 1 | `--arg …`, `--env NAME` (allowlist) | args and allowlisted vars, read with `with(Env)`; scoped views, no allocation | `narrow_env(e, names) with(Env) -> Environment` (consumes) | `give_up(e) with(Env) -> Unit` |
| `Clock<Monotonic>`, `Clock<Wall>` | ≤ 1 each | — | `now(&c) with(Time) -> u64` ns; resolution from `clock_getres`, recorded in the launch manifest; Wall may go backward | none | `give_up(c) with(Time) -> Unit` |
| `StdIn`, `StdOut`, `StdErr` | ≤ 1 each | — | read or write bytes, `with(Device)` (see OPEN F6.1) | none | `give_up(s) with(Device) -> Result<Unit, IoError>`; flushes; consumes in every outcome; the descriptor stays open for the runtime's trap message |
| `ConnectGrant` | one per name | `--connect NAME=tcp:CIDR,…:PORTS` | `connect(&g, addr) with(Network) -> Result<Socket, NetError>`; `g` stays | `narrow_connect(g, scope) with(Network) -> ConnectGrant` (consumes; scope ∩) | `give_up(g) with(Network) -> Unit` |
| `ListenGrant` | one per name | `--listen NAME=tcp:ADDR:PORT` | `listen(g) with(Network) -> Result<Listener, ListenFailed>`; `ListenFailed { grant: ListenGrant, err: NetError }` returns the grant | none | through the listener, `close(l) with(Network)`; or `give_up(g) with(Network) -> Unit` |
| `Resolver` | one per name | `--resolve NAME=server:IP:PORT;transport:udp\|tcp;suffix:S,…;types:A,AAAA` | `resolve(&r, name, a) with(Network, Alloc) -> Result<Vec<IpAddr, A>, ResolveError>`; a name or type outside the scope fails before any query; addresses grant nothing | none | `give_up(r) with(Network) -> Unit` |

`Socket` and `Listener` close like `File`: `close(s) with(Network) -> Result<Unit, CloseError>`, which always consumes.

**Startup and shutdown** come from the target's table (rule 12, `decisions.md:403–408`) and appear in the trace:
- `Start(grants)`: the launcher mints the requested grants.
- `Exit(code)`: main returned `code: u8`, which is the process exit status.
- `Trap(kind)`: the runtime writes a fixed message to descriptor 2 and exits with status 134.

These are effects of the target, not of source code. No source function gains a hidden permission.

The resolver's service and transport are explicit in its configuration. The system resolver configuration (`/etc/resolv.conf`) is never read, since it would be hidden input.

**Process spawning and other devices are not offered by this target** (Q7.4 is deferred). The "where supported" wording is gone: an absent service is absent from the table.

**Dynamic checks** (Q7.8). The primitive itself enforces each one, atomically:
- **Path lookup.** It uses `openat2(dirfd, path, RESOLVE_BENEATH|RESOLVE_NO_SYMLINKS|RESOLVE_NO_XDEV|RESOLVE_NO_MAGICLINKS)`, after refusing absolute paths and `..` in the syntax. The `NO_XDEV` flag is the refusal to cross a mount (`decisions.md:492`).
- **Connect.** The numeric address and port are checked against the scope before the syscall.
- **Resolve.** The name's suffix and record type are checked before the query.

**OPEN F6.1 — owner: the permission for the standard streams.** The reserved set (`decisions.md:410`) has no `Console`. The options:
- (a) `Device`: a stream is a device the target hands over;
- (b) `Fs`;
- (c) a new `Console` capability, which REVISES authority rule 13.

Recommendation: (a). It keeps the reserved set and keeps `Fs` meaning the filesystem namespace.

## F7. The loop form and its elaborations (Q9.1, Q9.3, Q9.4) — high

Grounding:
- OD47 (`decisions.md:203`): `while`, `for i from a until b`, half-open, bounds evaluated once, `continue` advances, the Core desugaring carries the measure `b − i`.
- `defer` (`:496–507`).
- OD68.

**Rule F7.1: Core `while`.**

```
while L { C } do { B }
```

- **`C`** is a condition region: an ANF block evaluated before every iteration that ends in a `bool` atom.
  - It may call, bind temporaries, match, and have effects.
  - Every linear obligation it creates is discharged inside it, and it registers no `defer`.
  - Its temporaries die at its end.
- **`B`** is a body block with its own deferred scope.
- **`L`** is a loop identity that resolved exits target: `break L` and `continue L`.
- **`continue L`** leaves every scope between it and `L`'s body, running their deferred calls in reverse, innermost first. Then it evaluates `C` again.
- **`break L`** does the same, then leaves the loop.

This is the single Core loop. ANF applies inside `C`, which answers the review's point that an atom fixed before the loop cannot model a condition recomputed each iteration.

**Example 1: an effectful condition.**

```
while next(&mut r) != 0 { work(&mut r); }
⇒ while L1 { let t1 = next(&mut r); let t2 = t1 != 0; t2 } do { work(&mut r); }
```

`next` runs before each iteration, and its effects occur each time `C` is evaluated.

**Example 2: a counted loop, a labelled `continue`, and `defer`.** The hidden counter advances *before* the body, so `continue` needs no step region.

```
'outer: for i from a until b {
    defer log_i(i);
    for j from 0 until n {
        defer tick();
        if skip(i, j) { continue 'outer; }
        use(i, j);
    }
}
⇒
{ let lo = a; let hi = b; let mut k = lo;          // bounds evaluated once
  while Louter { k < hi } do {                      // measure: hi − k  (proof-only)
    let i = k; k = k + 1;                           // advance first; cannot overflow: k < hi
    defer log_i(i);
    { let lo2 = 0; let hi2 = n; let mut k2 = lo2;
      while Linner { k2 < hi2 } do {                // measure: hi2 − k2
        let j = k2; k2 = k2 + 1;
        defer tick();
        if skip(i, j) { continue Louter; }          // runs tick(), leaves Linner, runs log_i(i), re-tests k < hi
        use(i, j);
      } }
  } }
```

`i` and `j` are immutable, and `k` and `k2` cannot be named in source. The measure is a proof annotation of the Core node and has no runtime effect. It is computed over mathematical integers, `toNat(hi − k)`, so a full-range signed loop cannot overflow it.

## F8. Provisional approval and freeze (Q9.14, Q9.15, P15) — high

Grounding: Q9.14 (four checked proofs before freeze), Q9.17 (kernel-only).

The sequence:
1. **A2a, provisional approval.** The owner accepts the decision rows. That approval makes them binding as the design to formalize, but they are not yet frozen.
2. **A2b, design formalization.** A minimal `ConcreteSpec/Spec.lean`, headed `-- PROPOSED: not normative until Core freeze`, defines only what the four theorems need. All four are checked by the kernel, with no `native_decide`:
   - `reset_fresh`: an allocator reset returns a brand distinct from every brand live in the input state. Resets in a loop give pairwise-distinct brands, and a value from the old brand is not accepted by the new allocator's operations.
   - `defer_cleanup_releases`: on every structured exit from a block with deferred `destroy` and `give_up` calls and a held loan, the calls run in reverse, each loan is released after its call, and the allocator-dependency check holds.
   - `open_mut_freezes_root`: inside a mutable borrow block on a `Heap`, no other access to the root type-checks, and a shared open never gives write access.
   - `rep_roundtrip`: for slice-1 types (`u8`–`u64`, `bool`, empty struct), encoding a value at its layout and then decoding it returns the value.
3. **Discoveries.** Any formalization finding that changes a row goes back to the owner before freeze.
4. **A2c, freeze.** The owner approves, the four theorems are checked, and the outside reader has passed. Then the `PROPOSED` header is removed and A3's one-main-file gate applies.

The roadmap's A2 row reads "provisional approval → four checked theorems against the proposed Spec.lean → freeze". R-0560, the normative-file task, depends on the freeze, not on A2 as a whole.

## F9. Restart consistency (P5–P7, P15, P21) — high

**Rule F9.1: order.** These are separate tasks.
1. **T0, prepare on main beside the old tree.** Land:
   - the new root CI workflow;
   - `required-checks.txt`, enforced by `train watch` and `land`;
   - `RETENTION.tsv`;
   - `docs/archive/roadmap-2026-11/task_disposition.tsv`;
   - `scripts/oracle/ORACLE.lock`;
   - the authenticated oracle artifact (F15);
   - `ISSUES.tsv` (F16).
2. **T1, rehearse.** Do the move on a throwaway branch, then run: fresh clone → build → hook → exact-SHA CI → `train.sh --dry-run land`. Negative controls (a broken build, a failing gate, a missing required check) must each be refused.
3. **T2, move in one train.** After the move:
   - `legacy/` is byte-frozen. A root check compares its tree hash with the recorded tree **minus the paths whose deletion a satisfied `RETENTION.tsv` entry authorized**;
   - no legacy gate ever runs, so the conditional "run when `legacy/` changes" is removed;
   - any change under `legacy/` is refused, except deletions that a satisfied `RETENTION.tsv` entry authorizes.
4. **B3.** Delete the old pipeline: compiler sources and their scripts, gates and build files.
   - Retained assets (corpus, regressions, examples, std, bug ledger, site, docs, research) stay in `legacy/` until each entry is migrated or retired by the owner.
   - `legacy/` disappears when `RETENTION.tsv` has no live entry, not at B3.

**Rule F9.2: the day-one root** (R-0558) adds `RETENTION.tsv`, `required-checks.txt`, `ISSUES.tsv`, `scripts/oracle/` and `docs/archive/roadmap-2026-11/` (including `task_disposition.tsv`) to the previous list.

## F10. P18, one wording for Phase E (P15, P18) — high

Grounding: OD115 (`decisions.md:271`), OD116 (`:272`), OD117 (`:273`), and `:511` (contracts see surviving parameter state, with no implicit entry snapshot).

Replacement text for Phase E and its tasks:

> **Contracts and termination within OD115–OD117.**
> - No source `old`, quantifier or implication, and no snapshot construct. A property relating entry state to exit state is a Lean theorem about the function's semantics.
> - Frame properties (what a function leaves unchanged) are Lean theorems. A source frame clause would be a new language decision for the owner.
> - Specification functions have a body and are total (OD115), checked by structural recursion or a checked measure.
> - Ordinary functions have no total category (OD116). Their termination, where wanted, is a per-function Lean theorem.

Task changes:
- **R-0524**: "Frame and entry/exit theorems in Lean, no `old`". Done when such theorems are proved on the model for the flagship examples, and the surface has no `old`.
- **R-0526**: kept, "Checked totality of spec functions (OD115)".
- `NEW_ROADMAP.md:114` replaces "typed and two-state contracts, totality" with "typed contracts, Lean entry/exit and frame theorems, spec-function totality".

## F11. Entry snapshots (Q9.11, P16, P18) — medium

**Rule:** no new snapshot facility. Q9.11's "entry snapshots are explicit" is replaced by the following.
- `requires` reads parameters at entry.
- `ensures` reads `result` and the surviving parameter state after deferred calls (OD144, `decisions.md:511`). A consumed parameter is invisible.
- A `ghost let` (OD81) is body-local and invisible to contracts, so it can never act as a snapshot.
- An entry-to-exit relation is a Lean theorem (F10).

Ordinary code may pass a Copy value as an ordinary parameter and constrain it in `requires`. That is plain code, not a facility, and no rule mentions it.

## F12. Constant refusal (Q6.1–Q6.6) — medium

Grounding: TYPE-22 (`docs/spec/types.md:1497–1520`), OD118.

The transfer rules. "Known" means a fixed-width value is computed statically, using the runtime checked-operation definitions.

| form | known? | refusal |
|---|---|---|
| integer or bool literal | yes | — |
| `let x = e` (immutable) with `e` known | yes | — |
| any `mut` local, any read after assignment | no | — |
| use of a module constant `C` | iff `C`'s initializer is known | its initializer's violation (see below) |
| checked unary or binary op, known operands | yes, if defined | **refuse** if the op traps on them (overflow) |
| `/` or `%` with a known zero divisor | — | **refuse**, whether or not the dividend is known |
| `<<` or `>>` with a known count that is negative or `≥` the operand's width (ARITH-3@1, `docs/spec/arithmetic.md:107`) | — | **refuse**, whether or not the shifted value is known |
| `MIN / -1`, `MIN % -1` with both known | — | refuse (covered by the known-operands row) |
| `wrapping_add`, `wrapping_sub`, `wrapping_mul`, `saturating_add`, `saturating_sub`, `saturating_mul` (ARITH-4@1, `arithmetic.md:196`): the complete list of named arithmetic recognized here | yes, when both operands are known, by ARITH-4@1's definitions | — (they never trap) |
| `as` cast on known (only lossless casts exist, OD11, `decisions.md:167`) | yes, always defined | — (it never traps) |
| fixed-array index with a known index | — | refuse if `≥ N` |
| any other call, including named lossy conversions and spec fns in runtime code (the named list above is closed; adding a name is a spec change) | no | — |
| `if` or `match` result | no (no path inference) | both branches checked |
| `&&` and `\|\|` | per operand | both operands checked (Q6.5); executed conditionally |
| contract or spec arithmetic (mathematical `Int`) | not runtime | none: it has its own semantics |

Generic bodies are checked parametrically, then rechecked after substitution (Q6.3). Unreachable branches are checked (Q6.4).

**Evaluation cannot fail for lack of resources.**
- It executes no user function and no loop.
- Constant initializers form an acyclic dependency graph; a cycle is refused.
- Each constant is evaluated once per compilation and memoized.
- Every value is fixed-width.

So the total work is linear in the total size of all initializers and bodies, and there is no resource-failure outcome. This removes Q6.6's "evaluator-resource" class.

**REVISES TYPE-22@1's conformance example — owner decision.**
- "Check all written code" (Q6.4) makes an initializer with a proven violation refused **even when the constant is unused**.
- So both TYPE-22 examples change: `unused-constant-never-runs` (`types.md:1508`, exit 42) and `constant-traps-at-use` (`types.md:1524`, abort on overflow) both become `reject` examples, with the static refusal at the initializer.
- TYPE-22's runtime rule (evaluated at each use, never at start) is unchanged.

The options:
- (a) refuse proven violations in every constant initializer, as recommended. This is OD118's spirit: a proven violation is refused everywhere.
- (b) check an initializer only at uses, keeping the example, including uses in dead branches.

## F13. Zero-sized values and addresses (Q3.8, Q3.10) — medium

**Rule:** a collision is an overlap of occupied bytes, `[addr, addr+size)`.
- Zero-sized blocks and slots occupy no bytes. They may share addresses with each other and with one-past addresses.
- Every block satisfies `base + size ≤ 2^64 − 1`, so its one-past address is representable.
- An allocation that does not fit fails as an ordinary `AllocFailure`. Address exhaustion is part of `Avail` (F1).
- Ownership, identity and destruction count never derive from address equality. `ptr_eq` compares addresses only, returns a Copy `bool`, and grants nothing.
- A zero-sized element still has its own obligation and its own destruction, in index order.
- Exposing a zero-sized block never yields provenance on reconstruction, which is harmless because zero-byte accesses need none (F5).

## F14. Assurance wording (Q8.1–Q8.5, P15) — medium

Replace "verified from source to native code" (`NEW_ROADMAP.md:3`) and "end-to-end compiler" (`:111`) with:

> The first milestone proves the compiler's front end and lowering correct from exact source to BIR, for slice 1, in Lean. The native build is qualified by differential testing, not proved: the printer, LLVM, linking and runtime are named assumptions. A claim about the native image is conditional on those assumptions. The approved launcher and target manifest is `linux-x86_64-hosted` (F6).

## F15. The oracle source and fallback runners (P5, P13) — medium

- **The oracle artifact** is the built `legacy-compiler-final` tarball, with its SHA-256 recorded in `ORACLE.lock` beside the toolchain and flake lock hashes. It is published as a GitHub release asset, which is independent of ci-2. The ci-2 cache is only a cache.
- **A fallback (GitHub-hosted) runner** fetches the artifact by digest and verifies it. If the fetch or verification fails, the oracle check is **incomplete**, which is not passed. Incomplete blocks landing whenever the check is in `required-checks.txt`.
- **Checks that can run on fallback:** build, spec and gate checks, and the oracle diff on x86_64-linux. Heavy campaigns do not run there; they are reported as skipped, never as passed.

## F16. Issue intake and operational completion (P8, P12, P14, P22) — medium

- **P12, revised.** From day one there is a minimal `ISSUES.tsv` at the root, with the columns `id, date, area (spec|proof|compiler|launcher|ci|migration), evidence (path or SHA), owner, severity, closure condition, status`. A small root check validates the columns and statuses. The full bug ledger and tool return with slice 1.
- **P8, explicit.** The status page and the work queue both stop, along with the fuzz farm, perf tracking and the nightly campaign. Only landing services remain: the train, review, the hook and the landing bot.
- **P14.** Before handover:
  1. copy the data to ci-2;
  2. compare a `sha256sum` manifest of the source against the archive;
  3. restore a sample from the archive into a scratch directory and compare it;
  4. only then does the owner stop and wipe the farm on ci-3.

## F17. P17, split (medium)

- **Default arguments:** a Permanent refusal. They conflict with visible arguments.
- **`comptime` and broad `#[cfg]`:** keep evidence-based reopening. They are reopened only with a workload, a proof story, phase separation and an audit report, and only by a new owner decision.

Recommendation: split the row as above. OPEN — owner.

## F18. Precedence (Q5, Q9.15, P2, P3, P15–P20) — medium

**Precedence statement**, for CORE_PROPOSAL.md, PHILOSOPHY.md and the roadmap:

> Recorded decisions (docs/decisions.md) govern. An accepted row supersedes a recorded decision only through a revision the owner explicitly approved and marked as such (OD45/OD87 in Q5; TYPE-22's examples in F12). R4's replacements supersede the conflicting draft rows (Q1.11, Q1.12, Q2.2–2.5, Q3.6–3.10, Q4.6, Q6, Q7, Q9.1–9.4, Q9.11, Q9.14, P5–P8, P12–P18, P21) and the roadmap passages listed below. Where this document differs from those, it is superseded and will be edited.

The passages to reconcile:
- PHILOSOPHY.md:62 and :271 (capability inference, now never-infer);
- PHILOSOPHY.md:151 (rule 6's generated chapters, now Lean is the spec);
- NEW_ROADMAP.md:11 (it "holds binding rules", but the binding decisions now live in `docs/decisions.md`, per P20);
- NEW_ROADMAP.md:3 and :111 (assurance wording, F14);
- NEW_ROADMAP.md:53, :125 and :131, and R-0558, R-0563 and R-0564 (restart order, F9);
- NEW_ROADMAP.md:114 and :237, R-0524 and R-0526 (P18, F10);
- the A2 row and R-0560 (freeze sequence, F8);
- CORE_PROPOSAL.md's superseded recommendations: the Q5 inference, the labelled `loop`, and every row R4 replaces.

A separate reconciliation pass does the edits.

## Q3.5 (low)

The reason becomes: "One length and index type, `u64`, on every target, with explicit conversions; each target states a checked maximum allocation size, which is part of `Avail`." The "another profile may choose" clause is removed.

---

## Owner items raised here

- **F3.1 and F3.1b** — the disjunctive bound `T: Copy | Destroy` (new syntax), and coherence using Copy/Destroy disjointness.
- **F5.4** — the `kept` spelling and the receipt-based ownership design (`Kept<H>`).
- **F6** — the target table and the exit code 125.
- **F6.1** — the stream permission. `Device` is recommended; `Console` would revise rule 13.
- **F12** — REVISES TYPE-22@1's two examples: a constant initializer with a proven violation is refused, whether or not it is used.
- **F17** — the P17 split.
- **F8** — the A2a/A2b/A2c sequence.
- **F10 and F11** — P18's wording (no OD changes).
- **F4.3** — the `slice … else` spelling.
- **F7** — the Core `while` with a condition region. It refines OD47's Core desugaring and does not revise it.
