# Arena Allocation

Status: research

## Problem

Concrete programs that parse, interpret, or process structured data naturally create pools of values that share a lifetime. The JSON parser and MAL interpreter both use `Vec<T>` as manual arenas — allocate items via `vec_push`, track by index, free the entire Vec at the end.

This works but has costs:
- `vec_push` may trigger `realloc`, copying all existing data
- No way to reset and reuse without freeing individual items
- Index-based access is untyped (just `i32` offsets)
- The pattern is invisible to the compiler — it can't optimize or verify it

Arena allocation (bump allocation) is the natural formalization of this pattern.

## What Already Exists

- `Vec<T>` as a de facto arena (used in json/main.con and mal/main.con)
- `Alloc` capability gating all heap operations
- Layout.lean computing type sizes (needed for bump pointer math)
- EmitBuiltins.lean generating LLVM IR for collection operations
- Intrinsic.lean mapping operations to capabilities

## Design

### Arena type

```con
let arena: Arena = arena_new(4096);          // allocate 4KB region
let p: &mut MyStruct = arena_alloc::<MyStruct>(&mut arena);  // bump-allocate
p.x = 42;
arena_reset(&mut arena);                      // reset offset to 0, no individual frees
arena_free(arena);                            // free the underlying buffer
```

Key properties:
- Single `malloc` at creation, single `free` at destruction
- `arena_alloc` is a pointer bump (offset += align_up(sizeof(T)))
- No individual deallocation — only bulk reset or free
- References from arena are valid until reset/free
- Linear ownership: arena must be freed exactly once

### Intrinsics

| Intrinsic | Signature | Capability | LLVM lowering |
|-----------|-----------|-----------|---------------|
| `arena_new(cap)` | `(Int) -> Arena` | `Alloc` | `malloc(cap)` + init struct |
| `arena_alloc::<T>(a)` | `(&mut Arena) -> &mut T` | `Alloc` | bounds check + GEP + bump offset |
| `arena_reset(a)` | `(&mut Arena) -> ()` | none | store 0 to offset field |
| `arena_free(a)` | `(Arena) -> ()` | `Alloc` | `free(buffer)` |

### Arena struct (runtime representation, 24 bytes)

```
field 0: ptr   — pointer to allocated region
field 1: i64   — current offset (bump pointer position)
field 2: i64   — capacity (total allocated bytes)
```

Identical layout to Vec/String. Could reuse the same LLVM struct type.

### Overflow policy

When `arena_alloc` would exceed capacity:
- **Option A**: abort (like current OOM behavior) — simplest, fits safety-critical use
- **Option B**: return `Option<&mut T>` — explicit, but adds checking at every allocation
- **Option C**: grow via realloc — defeats the purpose (invalidates existing pointers)

Recommend **Option A** for now. Arenas are sized upfront; exceeding capacity is a programming error in the same category as OOM.

## Difficulty Assessment

| Component | Effort | Notes |
|-----------|--------|-------|
| Type system (Arena as builtin) | 1 day | Same pattern as Vec — generic over nothing (untyped buffer) |
| Intrinsic registration | 1 hour | 4 new entries in IntrinsicId enum |
| Type checking | 1-2 days | Validate arena operations, ensure &mut returns are sound |
| LLVM codegen (EmitBuiltins) | 1-2 days | Simpler than Vec — no realloc, no element tracking |
| SSA lowering | 1 day | Direct call lowering, same as other intrinsics |
| Tests | 1 day | Rewrite json pool pattern using arenas |
| **Total** | **~1 week** | |

### What makes this easy

- **Simpler than Vec**: no get/set/pop/realloc/doubling — just bump and reset
- **All infrastructure exists**: intrinsic dispatch, capability checking, LLVM emission
- **No new type system concepts**: Arena is an opaque linear type like Vec
- **Clear LLVM lowering**: bump allocation is ~5 LLVM instructions

### What needs care

- **Alignment**: `arena_alloc::<T>` must align the bump pointer to `tyAlign(T)` before returning. The Layout module already computes this.
- **Lifetime safety**: references from `arena_alloc` are invalidated by `arena_reset` and `arena_free`. The linear checker should prevent use-after-reset, but this requires that `arena_reset` consumes and re-produces the arena (or that the checker tracks invalidation).
- **Generic dispatch**: `arena_alloc::<T>` needs the size and alignment of T at monomorphization time, same as `vec_push::<T>`.

## Interaction with Other Features

- **Allocation budgets**: An arena of size N is a `BoundedAlloc(N)` by construction. Functions that only use arena allocation (no malloc/realloc) are bounded.
- **defer**: `defer arena_free(arena)` is the natural cleanup pattern.
- **Proof story**: Arena operations are simple enough to formalize (bump pointer arithmetic, capacity bound).
- **JSON/MAL rewrite**: Both examples could be simplified — replace `Vec<Val>` pool + index tracking with `Arena` + direct pointers.

## What This Replaces

The current manual pool pattern:
```con
let mut pool: Vec<Val> = vec_new::<Val>();
let idx: i32 = vec_len::<Val>(&mut pool) as i32;
vec_push::<Val>(&mut pool, val);
// ... later ...
let v: Val = vec_get::<Val>(&mut pool, idx as Int);
```

Would become:
```con
let mut arena: Arena = arena_new(4096);
let v: &mut Val = arena_alloc::<Val>(&mut arena);
v.tag = 2;
v.data = 42;
// ... later: direct pointer access, no index indirection ...
```

Fewer allocations, no realloc copies, no index indirection, and the lifetime is explicit.
