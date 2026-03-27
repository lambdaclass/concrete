# Iterators And Traversal Surfaces

Status: resolved — per-container traversal APIs landed, no iterator tower

Priority: P1 (was); now closed

## Question

Should Concrete add iterator support, and if so, what is the smallest form that helps real programs without importing a large abstraction culture from Rust or functional languages?

## Resolution

The traversal story is now complete with three tiers:

1. **`for_each`** — side-effect traversal (printing, logging)
   - `Vec<T>.for_each(fn(&T))`
   - `HashMap<K,V>.for_each(fn(&K, &V))` / `keys_for_each` / `values_for_each`
   - `HashSet<K>.for_each(fn(&K))`

2. **`fold<A>`** — stateful traversal with explicit accumulator, no closures needed
   - `Vec<T>.fold<A>(init, fn(A, &T) -> A) -> A`
   - `HashMap<K,V>.fold<A>(init, fn(A, &K, &V) -> A) -> A` / `keys_fold<A>` / `values_fold<A>`
   - `HashSet<K>.fold<A>(init, fn(A, &K) -> A) -> A`

3. **`keys()` / `elements()` / `values()`** — materialization to Vec when you need a collection

`fold<A>` was blocked by a compiler bug: method-level generics (`fn fold<A>` inside `impl<K,V>`) parsed but crashed at lowering because (a) self parameter types lost their generic args and (b) generic structs were only instantiated once at the LLVM level. Fixed in `c0c5b54`.

This is the right **current** design and also the right **base** for the long-term design.

## Design Decisions

- **No cursors**: would require borrowing lifetimes, which Concrete will never add. Lifetimes fail multiple design filters (inference-heavy, murkier diagnostics, phase coupling). This is a permanent design decision, not a deferral.
- **No closures**: `fold` threads state through the return value instead of capturing mutable locals
- **No iterator trait**: per-container APIs cover the need; a shared protocol adds complexity without proven benefit
- **No lazy adapter chains**: explicit traversal only
- **No hidden control flow**: traversal semantics should remain obvious from container APIs and ordinary call structure

## End-Game Design

The best end-state is not a Rust-style iterator ecosystem.

It is:

1. **explicit per-container traversal as the semantic truth**
   - `for_each`
   - `fold<A>`
   - materialization helpers such as `keys()` / `values()` / `elements()`
2. **optional thin syntax sugar later**
   - only if it lowers directly and transparently to the existing traversal APIs
   - for example, a future `for` loop form would be acceptable only as sugar over the already-explicit traversal model
3. **small targeted additions only if real programs force them**
   - the most likely remaining gap is not “full iterators,” but narrow early-exit helpers such as:
     - `find`
     - `any`
     - `all`
     - `try_fold`

The design center should remain:

- explicit traversal
- explicit allocation
- explicit control flow

not a compositional abstraction tower.

## What Was Considered And Rejected

- Rust-style `trait Iterator<T> { fn next(&mut self) -> Option<T>; }` — requires trait system + lifetimes for cursors
- C-style `fn for_each_ctx(ctx: *mut u8, f: fn(*mut u8, &K))` — too low-level for a default stdlib pattern
- Cursor/handle-based iteration — requires lifetime tracking which Concrete won't add
- trait-heavy combinator ecosystems (`map`, `filter`, `zip`, `collect` as a design center) — not aligned with Concrete's goals even if they are common elsewhere

## Evidence

Phase H programs showed the traversal pressure:
- kvstore needed parallel `Vec<String>` because HashMap had no traversal (now uses HashMap + fold)
- integrity monitor used O(n) linear manifest scanning (now uses HashMap + HashSet with fold)
- `for_each` alone was insufficient because it can't accumulate without closures

## Post-Landing Evidence

Migration audit across all Phase H examples confirmed the traversal APIs are useful but narrow:

- **kvstore**: uses `HashMap.fold<String>` for compact/list — clean fit
- **vm, mal, policy_engine, integrity, verify, lox, toml**: all keep explicit `while` loops because their iteration patterns need early exit, multiple mutable accumulators, captured context, or are state machines (VM dispatch)
- **for_each** is useful for simple side-effect traversal but rare in practice — most side-effect loops also need accumulation
- **fold** is the right core primitive for stateful traversal without closures, but only fits pure-accumulation patterns

The bigger ergonomic win is not fold/for_each adoption but migrating from old `vec_get`/`vec_len` free-function API to the method API (`v.get()`, `v.len()`, `v.push()`). This is blocked for single-file examples (vm, mal, policy_engine) which can't access `std.vec` without project conversion.

## Current Recommendation

The traversal surface is sufficient as the core model.

Revisit only if:

- real programs show a repeated early-exit pattern that `fold` + `for_each` + materialization cannot cover well
- there is repeated evidence for a tiny shared protocol that does not introduce hidden semantics
- thin syntax sugar can be justified without creating a second traversal model
