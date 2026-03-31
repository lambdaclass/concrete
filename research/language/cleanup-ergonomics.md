# Cleanup Ergonomics Design

Status: active — item 1 landed during Phase H, item 2 in progress, items 3-5 deferred

## Problem

Concrete's linear ownership model requires explicit cleanup of owned values (strings, vecs, etc.). This is by design — every cost is visible. But the current surface creates friction:

- `drop_string` calls on every early-return path are easy to forget
- temporary strings created just for comparison require allocate + drop
- multiple pool arguments thread through every function signature

The JSON parser was the first sustained test of this friction at scale (~450 lines).

## Design Options

### 1. `defer drop_string(s)` as standard idiom — LANDED

Keep destruction explicit, but eliminate the "forgot a drop on an early return" class of bugs.

Before:
```con
let kw: String = "true";
if match_keyword(s, p, &kw) {
    drop_string(kw);
    return result;
}
drop_string(kw);
```

After:
```con
let kw: String = "true";
defer drop_string(kw);
if match_keyword(s, p, &kw) {
    return result;
}
```

This was the highest-leverage single change. It preserves explicitness while eliminating duplicated cleanup on branching control flow. Go proved this pattern works.

What landed:

- `defer` now uses scope-aware lowering rather than the earlier flat function-scoped approximation
- deferred calls run at block exit, loop-iteration exit, `break`, `continue`, early return, and implicit function end
- the checker reserves deferred values instead of consuming them immediately
- additional regression coverage now includes block scope, loop iteration, break/continue, and consuming-call LIFO behavior
- the JSON parser was updated to use `defer` throughout, removing roughly 40 lines of duplicated cleanup boilerplate
- the largest practical win was replacing repeated `if err != 0 { cleanup(...); return err; }` blocks in `main` with straight early returns guarded by one scoped `defer cleanup(...)`

Follow-on concern:

- defer-heavy control flow may duplicate cleanup IR at multiple exit sites
- this is acceptable for now because correctness and clear scope semantics matter more than minimizing IR size
- if real programs show meaningful IR bloat, cleanup outlining into shared blocks can be revisited later

### 2. More mutation-oriented string APIs — DO NOW

Reduce temporary string allocations so there are fewer things to drop.

Already landed:
- `string_push_char(&mut s, ch)` — append single char
- `string_append(&mut s, &other)` — append string
- `string_append_int(&mut s, n)` — append integer as text
- `string_append_bool(&mut s, b)` — append bool as text

Still useful:
- `string_clear(&mut s)` — reset length to 0 without freeing (reuse buffer)
- `string_eq(a, b) -> bool` already exists
- consider `string_starts_with`, `string_ends_with` for parser patterns
- consider `string_from_int(n) -> String` for one-shot conversions

The builder pattern is proven by the JSON parser. The remaining gap is that keyword matching still requires temporary `String` allocation where a `&str` literal borrow would suffice.

### 3. Scoped helper abstractions — LATER

Small stdlib patterns that own a resource and clean it up at scope end via explicit `defer` inside the helper, not hidden compiler magic.

Example: a `with_temp_string(fn(&mut String))` pattern, or a `StringPool` that owns all allocated strings and frees them in one call.

Prerequisites: scoped `defer` (now landed), possibly closures or function pointers.

When to revisit: when programs reach 1k+ lines and explicit `defer` still leaves too much ceremony in helper-heavy code.

### 4. General `drop(x)` via Destroy trait — LATER

Unify `drop_string`, `vec_free`, etc. into a single `drop(x)` surface. This requires a trait-like `Destroy` mechanism.

Current state: each type has its own named destructor. This is explicit but creates a vocabulary explosion as the stdlib grows.

When to revisit: when the stdlib has 5+ distinct drop-like functions and the naming inconsistency causes real confusion. Also ties into trait/interface design decisions.

### 5. Selective borrow-friendly APIs — LATER

Many functions should take `&String` instead of forcing owned `String` churn. This is partially done (e.g., `print_string(&s)`, `match_keyword(s, p, &kw)`), but string literals still require allocation + ownership + drop.

The deeper fix would be `&str`-style borrowed string slices (pointing into an existing string without owning), but this has significant grammar and proof cost.

When to revisit: when scoped `defer` + mutation APIs have been in use for 2-3 programs and the remaining friction is measured, not speculated.

## Design Principles

- Keep cleanup explicit — no implicit destructor insertion, no GC, no hidden lifetime extension
- Make scoped `defer` the dominant cleanup pattern
- Reduce temporary allocations with better string/builder APIs
- Unify destruction ergonomics over time so individual `drop_X` calls don't feel like manual bookkeeping
- Every improvement should pass the DESIGN_POLICY.md admission criteria

## Evidence Source

The JSON parser (`examples/json/main.con`) was the first real pressure test. The `parse_string` function had to be restructured from early-return-with-drop to flag-pattern-with-post-loop-drop because the linear checker prevented consuming variables declared outside a loop. Scoped `defer` addresses that class of cleanup problem directly.
